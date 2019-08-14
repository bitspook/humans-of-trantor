+++
title = "DISCOVERED_EMPLOYEE"
date = "2019-08-12T05:58:47Z"
weight = 5
+++

- Whenever any service which is keeping track of Trantor employees discovers a
  new employee, it emits an `DISCOVERED_EMPLOYEE` event.
- It is possible for duplicated `DISCOVERED_EMPLOYEE` events to be emitted.
  Services listening for these events must take care of de-duplication.
- A service may emit `DISCOVERED_EMPLOYEE` when new/changed details for an
  employee are discovered.

{{% notice tip %}}
Recommended employee de-duplication technique is to recognize email and/or
employee-code as unique ID of an employee, and merge rest of the employee
details on every event.
{{% /notice %}}

Following fields can be present in the event payload for version `v1`:

| Field       | Is Required? | Description                   |
| :--         | :--          | :--                           |
| email       | Yes          |                               |
| ecode       | No           |                               |
| name        | No           |                               |
| phoneNumber | No           |                               |
| dateOfBirth | No           |                               |
| designation | No           |                               |
| reportsTo   | No           | ECode of immediate supervisor |
