import Router from 'express-promise-router';
import DiscoveredEmployee from './discoveredEmployee';

export interface EventI {
  type: string;
  version: string;
  validate: () => Promise<void>;
  payload?: any;
}

interface Event {
  new (version: string, payload?: any): EventI;
  type: string;
}

const events: Event[] = [DiscoveredEmployee];

const router = Router();

router.get('/', (_, res) => {
  res.json(events.map((e) => e.type));
});

router.post('/:name/:version', async (req, res) => {
  const { name, version } = req.params;
  const payload = req.body.payload;

  const eventConstructor = events.find((e) => e.type === name);

  if (!eventConstructor) {
    throw new Error(`UnsupportedEvent: "${name}" is not a valid event`);
  }

  const event = new eventConstructor(version, payload);
  await event.validate();

  res.send(event);
});

export default router;
