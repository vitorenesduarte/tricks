[![Build Status](https://img.shields.io/travis/vitorenesduarte/tricks/master.svg)](https://travis-ci.org/vitorenesduarte/tricks)
[![Coverage Status](https://img.shields.io/coveralls/github/vitorenesduarte/tricks/master.svg?maxAge=60)](https://coveralls.io/github/vitorenesduarte/tricks?branch=master)

# tricks 

A tricks entry (TE) is a set of replicas/pods.
An experiment is one or more TE's.

### Features
- Register occurrence of events. Each event has a counter associated.
- Workflow (e.g. only start a given TE once event X counter is Y)
- Implicit events (`start` and `stop` of each pod in a TE)
- Subscribe to events
  - can be used to implement synchronization barrier
    (e.g. make sure all clients start at the same time)

### Planned Features
- Service discovery
- Log aggregation
- Plotting from logs (e.g. latency/throughput, CDF, bar, line)
- Detect coordination omission from logs
- Federation support (run across multiple Kubernetes clusters)
- Spot instances support (if an instance is killed, the experiment is restarted)

### Example

In this example, event `client1_stop` and `client2_stop`
are implicit events,
while `server-ready` is an event registered by replicas
in the `server` TE.

```yaml
apiVersion: v1
experiment:
- tag: server
  image: vitorenesduarte/tricks-example
  replicas: 3
  env:
  - name: TYPE
    value: server
  workflow:
    stop:
      name: client2_stop
      value: 6
- tag: client1
  image: vitorenesduarte/tricks-example
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
  image: vitorenesduarte/tricks-example
  replicas: 6
  env:
  - name: TYPE
    value: client
  - name: OPS
    value: 200
  workflow:
    start:
      name: client1_stop
      value: 3
```


### Other examples

- [examples/hello-world.yaml](examples/hello-world.yaml)
- [examples/implicit-events.yaml](examples/implicit-events.yaml)
- [examples/explicit-events.yaml](examples/explicit-events.yaml)

# Environment variables (EV)

Configuration of pods is achieved through EVs.
Some EV names are reserved and always defined in every pod:
- `TAG`: the same tag from configuration file
- `EXP_ID`: an experiment identifier generated by tricks
- `POD_ID`: unique (no other pod with the same `TAG` and `EXP_ID`
has the same id)
- `POD_IP`: pod IP from Kubernetes
