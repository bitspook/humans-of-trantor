// tslint:disable:import-name
import test from 'ava';
import request from 'supertest';
import app from '@src';

const urlPrefix = '/events';

test('get list of available events', async (t) => {
  const res = await request(app).get(`${urlPrefix}`);

  t.is(res.status, 200);
  t.regex(res.header['content-type'], /application\/json/);
  t.is((res.body as string[]).every((e) => typeof e === 'string'), true);
});
