// tslint:disable:import-name
import test from 'ava';
import request from 'supertest';
import app from '@src';
import DiscoveredEmployee from './discoveredEmployee';

const urlPrefix = '/events';

test(`[GET] ${urlPrefix}/`, async (t) => {
  const res = await request(app).get(`${urlPrefix}`);

  t.is(res.status, 200);
  t.regex(res.header['content-type'], /application\/json/);
  t.is((res.body as string[]).every((e) => typeof e === 'string'), true);
});

test(`[POST] ${urlPrefix}/${DiscoveredEmployee.type}/v2019 returns error for invalid event version`, async (t) => {
  const res = await request(app)
    .post(`${urlPrefix}/${DiscoveredEmployee.type}/v2019`)
    .set('Content-type', 'application/json')
    .send();

  t.is(res.status, 400);
  t.deepEqual(res.body, {
    status: 400,
    error: 'UnsupportedVersion',
    message: 'Supported versions [ "v1" ]',
  });
});

test(`[POST] ${urlPrefix}/INVALID_EVENT/v1 adds valid event to Store db`, async (t) => {
  const res = await request(app)
    .post(`${urlPrefix}/INVALID_EVENT/v1`)
    .set('Content-type', 'application/json')
    .send();

  t.is(res.status, 400);
  t.deepEqual(res.body, {
    status: 400,
    error: 'UnsupportedEvent',
    message: '"INVALID_EVENT" is not a valid event',
  });
});
