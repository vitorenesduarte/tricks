# cal

A cal entry (CE) is a set of replicas.
An experiment is one or more CE's.

### Features
- Register occurrence of events. Each event has a counter associated.
- Workflow (e.g. only start a given CE once event X counter is Y)
- Implicit events (`start` and `end` of CE)

### Example

In this example, event `client1_end` and `client2_end`
are implicit events,
while `server-ready` is an event registered by replicas
in the `server` CE.

```yaml
apiVersion: v1
experiment:
- tag: server
  image: vitorenesduarte/cal-example
  replicas: 3
  env:
  - name: TYPE
    value: server
  workflow:
    end:
      name: client2_end
      value: 6
- tag: client1
  image: vitorenesduarte/cal-example
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
  image: vitorenesduarte/cal-example
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

# Environment variables (EV)

Configuration of pods is achieved through EVs.
Some EV names are reserved and always defined in every pod:
- `TAG`
- `EXP_ID`
- `POD_ID`
- `POD_IP`
