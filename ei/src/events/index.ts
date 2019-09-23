import Router from 'express-promise-router';
import DiscoveredEmployee from './DiscoveredEmployee';
import { sql } from 'slonik';

export interface EventI {
  id?: string;
  type: string;
  version: string;
  validate: () => Promise<void>;
  payload?: any;
}

interface Event {
  new(version: string, payload?: any): EventI;
  type: string;
}

const events: Event[] = [DiscoveredEmployee];

const router = Router();

router.get('/', (_, res) => {
  res.json(events.map((e) => e.type));
});

router.post('/:name', async (req, res) => {
  const { name } = req.params;
  const { version, payload } = req.body;
  const { db } = req.context;

  const eventConstructor = events.find((e) => e.type === name);

  if (!eventConstructor) {
    throw new Error(`UnsupportedEvent: "${name}" is not a valid event`);
  }

  const event = new eventConstructor(version, payload);
  await event.validate();

  const id = (await db.oneFirst(
    sql`INSERT INTO store.store (name, version, payload) VALUES (${name}, ${version}, ${sql.json(
      payload,
    )}) RETURNING (id)`,
  )) as string;
  event.id = id;

  res.json(event);
});

export default router;
