import { Router } from 'express';
import DiscoveredEmployee from './discoveredEmployee';

export interface EventI {
  type: string;
  validate: () => Promise<void>;
  payload?: any;
}

interface Event {
  new (payload?: any): EventI;
  type: string;
}

const events: Event[] = [DiscoveredEmployee];

const router = Router();

router.get('/', (_, res) => {
  res.json(events.map((e) => e.type));
});

router.post('/:eventName', (req, res) => {
  const { eventName } = req.params;
  const payload = req.body.payload;

  const eventConstructor = events.find((e) => e.type === eventName);

  if (!eventConstructor) {
    throw new Error(`UnknownEvent: ${eventName} is not a valid event`);
  }

  const event = new eventConstructor(payload);

  res.send(event.validate());
});

export default router;
