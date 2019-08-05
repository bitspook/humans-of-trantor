---
title: "The Event"
date: 2019-08-05T13:44:27+05:30
weight: 10
---

Events are integral to HoT. A single event is a JSON object which should have following structure:

``` json
  {
    "id": "a-valid-uuid-v4",
    "name": "VERY_IMPORTANT_EVENT",
    "version": 1,
    "payload": any
  }
```

1. **id**: Every event can be uniquely identified with its =id=
2. **name** is a string which identifies this event
3. **version** is a number. *type + version* together uniquely identifies the type
   of an event
4. **payload** can be any valid JSON type
