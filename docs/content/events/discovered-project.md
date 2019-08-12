+++
title = "DISCOVERED_PROJECT"
date = "2019-08-12T05:58:47Z"
weight = 5
+++

- Whenever any service which is keeping track of Trantor projects discovers a
  new project, it emits an `DISCOVERED_PROJECT` event.
- It is **NOT** possible for duplicate `DISCOVERED_PROJECT` events to be
  emitted. Service emitting this event should ensure it is a new/unique project.

Following fields can be present in the event payload:

| Field | Is Required? | Description                                          |
| :--   | :--          | :--                                                  |
| id    | Yes          | UUID which will become primary key for this project. |
| name  | Yes          |                                                      |
| lead  | No           | ECode of project manager                             |
