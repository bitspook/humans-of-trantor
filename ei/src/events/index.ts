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

router.post('/:name/:version', (req, res) => {
  const { name, version } = req.params;
  const payload = req.body.payload;

  if (version !== 'v1') {
    throw new Error('UnsupportedVersion: Supported versions [ "v1" ]');
  }

  const eventConstructor = events.find((e) => e.type === name);

  if (!eventConstructor) {
    throw new Error(`UnsupportedEvent: "${name}" is not a valid event`);
  }

  const event = new eventConstructor(payload);

  res.send(event.validate());
});

export default router;
