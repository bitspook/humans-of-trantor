CREATE MATERIALIZED VIEW standup_updates_v1_no_source AS (
WITH
  collective_standups_v1 as (
	  SELECT
      id,
  	  payload->>'project' AS project,
      payload->>'ecode' AS ecode,
      payload->>'date' AS date,
      payload->>'standup' AS standup,
	  created_at
      FROM store.store
     WHERE name = 'RECEIVED_STANDUP_UPDATE'
       AND version = 'v1'
	   ORDER BY created_at DESC
  ),
  individual_standup_v1 AS (
    SELECT
      uuid_generate_v1mc() as id, project, ecode, date, created_at,
	  btrim(regexp_split_to_table(standup, E'\\n+')) as standup
      FROM collective_standups_v1
  ),
  clean_standup_v1 AS (
      SELECT
	  id, project, ecode, date, created_at,
      regexp_replace(standup, E'^-?\\s*\\[.*\]+', '') as standup,
	  CASE WHEN starts_with(standup, '- [X]') OR starts_with(standup, '-[X]') THEN true ELSE false END AS isDelivered
	  FROM individual_standup_v1
  )
  SELECT
   id, project, ecode, date, created_at, standup, isDelivered
   FROM clean_standup_v1 WHERE standup != ''
);

CREATE MATERIALIZED VIEW sources AS (
	SELECT DISTINCT ON (standup) id as source, standup, created_at
	FROM standup_updates_v1_no_source
	ORDER BY standup, created_at ASC
);

CREATE MATERIALIZED VIEW payloads AS (
SELECT
  CASE WHEN src.source = origin.id THEN origin.id ELSE NULL END as id,
  CASE WHEN src.source = origin.id THEN NULL ELSE src.source END as source,
  origin.ecode,
  origin.project,
  origin.standup,
  origin.date,
  origin.isDelivered,
  origin.created_at as created_at
FROM sources AS src
RIGHT JOIN standup_updates_v1_no_source AS origin
ON (src.standup = origin.standup)
);

SELECT * from payloads ORDER BY standup, created_at ASC;

CREATE TEMP VIEW migrated_events AS (
SELECT
  id,
  'RECEIVED_STANDUP_UPDATE' as name,
  'v2' AS version,
  json_build_object(
	  'ecode', ecode,
	  'project', project,
	  'standup', standup,
	  'date', date,
	  'isDelivered', isDelivered,
	  'source', source
  ) AS payload,
  created_at
FROM payloads
);

INSERT INTO store.store (id, name, version, payload, created_at)
SELECT id, name, version, payload, created_at FROM migrated_events WHERE id IS NOT NULL;

INSERT INTO store.store (name, version, payload, created_at)
SELECT name, version, payload, created_at FROM migrated_events WHERE id IS NULL;

DROP VIEW migrated_events;
DROP MATERIALIZED VIEW payloads;
DROP MATERIALIZED VIEW sources;
DROP MATERIALIZED VIEW standup_updates_v1_no_source;

DELETE FROM store.store WHERE name = 'RECEIVED_STANDUP_UPDATE' AND version = 'v1';
