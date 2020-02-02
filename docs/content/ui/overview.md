+++
title = "Overview"
date = 2020-01-09T05:13:26Z
weight = 5
+++

## Technology

Primary technologies used:

### React Framework for UI
- We use React only for UI, and nothing more. All the state management is done by Redux, and all the
  side-effects are performed in epics.
- App has 2 kind of react components

  1. **Views**<br />
      - Views are all in `/views` directory
      - Views are supposed to be those components, which `connect` with redux store
      - Every `view` is a folder with following files:
          - index.tsx -- provides JSX of the component
          - duck.ts   -- provides the state used by this component which is to be managed by Redux

  2. **Components**

### Redux for state management
- Redux operates outside of React and stores all the data that UI has to show
{{<mermaid>}}
graph TD
Start --> Stop
{{</mermaid>}}

### Redux-observable (aka epics) for managing side-effects in React components
