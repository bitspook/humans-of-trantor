---
title: "Application state"
date: 2019-08-05T13:45:59+05:30
weight: 15
---

> Application State is any data your application need/want to keep and change
> over time. State in backend services is often stored in databases.

## Maintaining Application State


1. **Creating initial State**<br />
   When building a new application for HoT, you might want create an initial
   state from the events already present in Event history. You can directly
   connect with HoT Event History database and read from the =event_history=
   table. Please refer to =docker-compose.yml= file for the hostname to which to
   make the connection should be made.

2. **Maintaining state**<br />
   During the application runtime, you should make changes only on reception of
   an event you are interested via a Redis stream. Connection to redis can be
   made using the hostname mentioned in =docker-compose.yml=. If you want to
   make a change to the application state, ask HoT side card to record an event
   for you instead.
