// tslint:disable:import-name
import test from 'ava';
import request from 'supertest';
import app from '@src';
import DiscoveredEmployee from './DiscoveredEmployee';

const urlPrefix = '/events';

test(`[GET] ${urlPrefix}/`, async (t) => {
  const res = await request(app).get(`${urlPrefix}`);

  t.is(res.status, 200);
  t.regex(res.header['content-type'], /application\/json/);
  t.is((res.body as string[]).every((e) => typeof e === 'string'), true);
});

test(`[POST] ${urlPrefix}/${DiscoveredEmployee.type} returns error for invalid event version`, async (t) => {
  const res = await request(app)
    .post(`${urlPrefix}/${DiscoveredEmployee.type}`)
    .set('Content-type', 'application/json')
    .send({ version: 'v2019' });

  t.is(res.status, 400);
  t.deepEqual(res.body, {
    status: 400,
    error: 'UnsupportedVersion',
    message: 'Supported versions [ "v1" ]',
  });
});

test(`[POST] ${urlPrefix}/INVALID_EVENT returns error for invalid event`, async (t) => {
  const res = await request(app)
    .post(`${urlPrefix}/INVALID_EVENT`)
    .set('Content-type', 'application/json')
    .send({ version: 'v1' });

  t.is(res.status, 400);
  t.deepEqual(res.body, {
    status: 400,
    error: 'UnsupportedEvent',
    message: '"INVALID_EVENT" is not a valid event',
  });
});

test(`[POST] ${urlPrefix}/${DiscoveredEmployee.type} should return validation errors in JSON format`, async (t) => {
  const res = await request(app)
    .post(`${urlPrefix}/${DiscoveredEmployee.type}`)
    .set('Content-type', 'application/json')
    .send({ version: 'v1' });

  t.is(res.status, 400);
  t.deepEqual(res.body, {
    status: 400,
    error: 'ValidationError',
    message: 'email is a required field',
  });
});

// TODO: Find a way to implement these tests
// test(`[POST] ${urlPrefix}/${DiscoveredEmployee.type} adds event to the store`, async () => {});

// test(`[POST] ${urlPrefix}/${DiscoveredEmployee.type} emits event through messaging bus`, async () => {});
