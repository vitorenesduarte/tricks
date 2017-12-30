# quad

A quad is a set of replicas.
An experiment is one or more quads.

### Features
- Register occurrence of events. Each event has a counter associated.
- Workflow (e.g. only start a given quad once event X counter is Y)
- Implicit events (`start` and `end` of quads)

### Example

In this example, event `client-a_end` and `client-b_end`
are implicit events,
while `server-ready` is an event registered by replicas
in the `server` quad.

```yaml
apiVersion: v1
experiment:
- tag: server
  image: vitorenesduarte/server
  replicas: 3
  workflow:
    end:
      name: client-b_end
      value: 6
- tag: client-a
  image: vitorenesduarte/client
  replicas: 3
  env:
  - name: OPS
    value: 100
  workflow:
    start:
      name: server-ready
      value: 3
- tag: client-b
  image: vitorenesduarte/client
  replicas: 6
  env:
  - name: OPS
    value: 200
  workflow:
    start:
      name: client-a_end
      value: 3
```
