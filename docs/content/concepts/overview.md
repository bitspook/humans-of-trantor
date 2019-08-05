---
title: "Overview"
date: 2019-08-05T13:40:06+05:30
weight: 5
---

HoT has 3 primary components:

1. **Event History**:
   Event History is stored in a single *PostgreSQL* table.

2. **Event Bus**:
   HoT uses *Redis* streams for distributing events to any application that
   might be interested.

3. **Event Distributor Side Car**:
   A sidecar application is included which:

   - Emits events into the Event Bus when a new event row is inserted in PostgreSQL
   - Provides a single HTTP endpoint to which applications can make POST
     requests, and the sidecar inserts them into the Event History
