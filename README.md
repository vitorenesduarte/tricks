[![Build Status](https://img.shields.io/travis/vitorenesduarte/tricks/master.svg)](https://travis-ci.org/vitorenesduarte/tricks)
[![Coverage Status](https://img.shields.io/coveralls/github/vitorenesduarte/tricks/master.svg?maxAge=60)](https://coveralls.io/github/vitorenesduarte/tricks?branch=master)



# Tricks 

__Running an experiment should be as easy as describing it in an YAML file.__

----------

A Tricks entry (TE) is a set of replicas/pods.
An experiment is one or more TE's.

### Features
- [x] Register occurrence of events. Each event has a counter associated.
- [x] Implicit events (`start` and `stop` of each pod in a TE)
- [x] Subscribe to events
  - can be used to implement synchronization barrier
    (e.g. make sure all clients start at the same time)
- [x] Workflow (e.g. only start a given TE once event X counter is Y)
- [x] Pod discovery
- [ ] Log aggregation
- [ ] Plotting from logs (e.g. latency/throughput, CDF, bar, line)
- [ ] Detect coordination omission from logs
- [ ] Fault injection
  - (when) could be defined in the same way workflow is
  - (what) maybe provide some sort of pod selector for process faults,
    and link selector for network faults
- [ ] Federation support (run across multiple Kubernetes clusters)
- [ ] Spot/preemptible instances support (if an instance is killed, the experiment is restarted)

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
    value: loop
  workflow:
    stop:
      name: client2_stop
      value: 6
- tag: client1
  image: vitorenesduarte/tricks-example
  replicas: 3
  env:
  - name: TYPE
    value: loop
  - name: ITERATIONS
    value: 5
  workflow:
    start:
      name: server-ready
      value: 3
- tag: client2
  image: vitorenesduarte/tricks-example
  replicas: 6
  env:
  - name: TYPE
    value: loop
  - name: ITERATIONS
    value: 10
  workflow:
    start:
      name: client1_stop
      value: 3
```


### Other examples

- [examples/hello-world.yaml](examples/hello-world.yaml)
- [examples/implicit-events.yaml](examples/implicit-events.yaml)
- [examples/driver.yaml](examples/driver.yaml)

# Environment variables

Configuration of pods is achieved through environment variables.
Some variable names are reserved and always defined in every pod:
- [x] `TAG`: from configuration file
- [x] `REPLICAS`: from configuration file
- [x] `EXP_ID`: an experiment identifier generated by Tricks
- [x] `POD_ID`: unique (no other pod with the same `TAG` and `EXP_ID`
has the same id)
- [x] `POD_IP`: pod IP from Kubernetes
- [x] `TRICKS_IP`: pod IP of Tricks
- [x] `TRICKS_PORT`: port of Tricks

# Drivers API

Replicas in experiments can be written in any language,
as long as there's a driver available.
Drivers open a socket (`TRICKS_IP` and `TRICKS_PORT`)
and talk with Tricks using the following API
(__[DT]__ is used if it's a message from a Driver to Tricks,
or with __[TD]__ otherwise).

- [x] Register events __[DT]__
```json
{
  "expId": "123456",
  "type": "event",
  "eventName": "connected",
}
```

- [x] Subscription of events __[DT]__
```json
{
  "expId": "123456",
  "type": "subscription",
  "eventName": "connected",
  "value": 10
}
```

- [x] Notification of events __[TD]__
```json
{
  "expId": "123456",
  "type": "notification",
  "eventName": "connected",
  "value": 10
}
```

- [x] Pod discovery __[DT]__
```json
{
  "expId": "123456",
  "type": "discovery",
  "tag": "server"
}
```

- [x] Pod discovery __[TD]__
```json
{
  "expId": "123456",
  "type": "pods",
  "tag": "server",
  "pods": [
    {
      "id": 1,
      "ip": "10.12.13.15"
    },
    {
      "id": 2,
      "ip": "10.12.13.16"
    },
    {
      "id": 3,
      "ip": "10.12.13.17"
    }
  ]
}
```
