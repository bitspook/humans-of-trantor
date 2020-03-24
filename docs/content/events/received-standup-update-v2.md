---
title: "RECEIVED_STANDUP_UPDATE V2"
date: Sun 23 Feb 2020 05:57:25 PM IST

weight: 20
---

- Whenever new scrum standup update from an employee is received, it emits this
  event
- It is possible for duplicate events to be emitted. Services listening for
  these events must take care of de-duplication.
- A service may emit this event when new/changed standup update is received
- Every entry in a day's standup emit a new event of this type

{{% notice tip %}}
`source` field should be used to pick latest representation of a single standup update for an employee
of given project on a given day
{{% /notice %}}

Following fields can be present in the event payload:

| Field       | Is Required? | Description                                             | Type     |
| :--         | :--          | :--                                                     | :--      |
| source      | No           | ID of the original standup update (null for new update) | String   |
| ecode       | Yes          | ECode of employee                                       | String   |
| project     | Yes          | Project name                                            | String   |
| standup     | Yes          | Content of the update itself                            | String   |
| date        | Yes          | Date for which this standup update is applicable        | DateTime |
| issues      | Yes          | Array of associated issue IDs                           | String[] |
| notes       | Yes          | Array of associated note IDs                            | String[] |
| isDelivered | Yes          | Has this update been delivered?                         | Boolean  |
