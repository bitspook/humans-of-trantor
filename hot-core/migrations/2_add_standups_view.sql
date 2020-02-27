CREATE VIEW standup AS (
  SELECT DISTINCT ON (id)
    CASE WHEN payload->>'source' IS NULL THEN id ELSE (payload->>'source')::UUID END AS id,
    payload->>'ecode' AS ecode,
    payload->>'project' AS project,
    payload->>'standup' AS standup,
    payload->>'date' AS DATE,
    (payload->>'isDelivered')::boolean AS isDelivered,
    CASE WHEN payload->>'priority' IS NULL THEN 0 ELSE (payload->>'priority')::Int END AS priority
    FROM store.store
   WHERE name = 'RECEIVED_STANDUP_UPDATE'
     AND version = 'v2'
	 ORDER BY id, created_at DESC
)