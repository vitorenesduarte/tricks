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
  image: vitorenesduarte/quad-example
  replicas: 3
  env:
  - name: TYPE
    value: server
  workflow:
    end:
      name: client2_end
      value: 6
- tag: client1
  image: vitorenesduarte/quad-example
  replicas: 3
  env:
  - name: TYPE
    value: client
  - name: OPS
    value: 100
  workflow:
    start:
      name: server-ready
      value: 3
- tag: client2
  image: vitorenesduarte/quad-example
  replicas: 6
  env:
  - name: TYPE
    value: client
  - name: OPS
    value: 200
  workflow:
    start:
      name: client1_end
      value: 3
```
