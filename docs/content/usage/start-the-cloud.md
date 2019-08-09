---
title: "Start the Cloud"
date: 2019-08-05T13:38:00+05:30
weight: 10
---

To bring up the HoT cloud, you can simply execute:

```sh
docker-compose up
```

This starts all the services.

1. **Docs (docs)**<br>

    Docs are up and running and accessible on http://localhost:3000 on the host
    machine. Within the cloud, they can be reached at `docs`.

2. **Store (store)**<br>

    Postgres database is used for storing all the events, hence acting as a
    store. It is accessible at port `5432` on the host machine. Services can
    directly make read-queries to the store; for instance to replay historic
    events to build fresh projections.

3. **Messaging Bus (mbus)**<br>

    Redis is used as a messaging bus which broadcasts new events to all the
    services. Services can choose to subscribe to specific events that interest
    them. Messaging Bus is accessible on port `6379` on host machine.
