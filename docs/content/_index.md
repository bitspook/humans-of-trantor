+++
chapter = false
+++

# Humans of Trantor

> A history of events happening with [H]umans [o]f [T]rantor Software Private Ltd

Humans of Trantor aims to be a backbone on which Engineers of Trantor can build
products for themselves.

HoT is an experimental implementation of an
[event-sourced](https://martinfowler.com/eaaDev/EventSourcing.html) system
employing [CQRS](https://www.martinfowler.com/bliki/CQRS.html), recording and
publishing **all** events that happen with humans (not just employees) of
Trantor. We want to encourage developers of Trantor to hack their lives here at
Trantor. If you feel something needs fixing in your live at Trantor (attendance
system anyone?), get creative and fix it. Or just play around with data.

HoT itself just keeps track of all the events that happen with humans of
Trantor.

## HoT provides:

1. A list of events
2. API to post events into the system
3. A connection to (Postgre)SQL to read historic events
4. A connection to Redis where future events can be consumed via Redis streams

## CQRS

HoT judicially employs Command and Query Segregation.

- **Commands**<br />
  An application that want to make a change in its state shall record an event
  in the event-bus. *No* changes that the application can't live without shall
  be made directly into its state. Ideally, the application's state should be a
  read-only snapshot of events recorded in HoT projected to meet application's
  requirements.

  {{% notice tip %}}
  State is any form of mutable data that your application needs. Most backend applications keep their primary state in database.
  {{% /notice %}}

- **Queries**<br />
  The application's own state (e.g database) shall be a snapshot derived from
  the events recorded in HoT. Any data required by application should be read
  from this source and shouldn't require a new query made to HoT.
  <p>
  On creation of a new app, application state snapshot shall be created by
  reading the entire history of events application is interested in.
  Subsequently, application should keep its state in sync by subscribing to new
  events from the event bus.
  </p>
  <p>Application should treat its own state to be ephemeral.</p>

{{<mermaid>}}
sequenceDiagram
    participant Application 1
    participant Event Injector
    participant Store
    participant Messaging Bus
    participant Application 2

    Application 1 ->> Event Injector: Emits an Event
    Event Injector ->> Store: Validates the event and put it in the store
    Store ->> Messaging Bus: Store drops the event into messaging bus
    Messaging Bus -->> Application 2: Messaging Bus broadcasts the event that any other application in the cloud can see.
{{</mermaid>}}
