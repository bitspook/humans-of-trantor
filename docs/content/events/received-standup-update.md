---
title: "RECEIVED_STANDUP_UPDATE"
date: 2019-11-22T17:10:00Z
weight: 15
---

- Whenever new scrum standup update from an employee is received, it emits this
  event
- It is possible for duplicate events to be emitted. Services listening for
  these events must take care of de-duplication.
- A service may emit this event when new/changed standup update is received
- Most updates will have 2 entries per work day; one given at start of the day,
  another at end of the day, or beginning of next day.

{{% notice tip %}}
Recommended employee de-duplication technique is to recognize combination of
`employee-code`, `isEod` and `project` for uniqueness of a standup update, and
replace old standup update with a more recent one for a given date.
{{% /notice %}}

Following fields can be present in the event payload:

| Field   | Is Required? | Description                                      | Type                               |
| :--     | :--          | :--                                              | :--                                |
| ecode   | Yes          | ECode of employee                                | String                             |
| project | Yes          | Project name                                     | String                             |
| standup | Yes          | Content of the update itself                     | String                             |
| date    | Yes          | Date for which this standup update is applicable | DateTime                           |
| type    | Yes          | The type of the standup                          | [committed, delivered, impediment] |
