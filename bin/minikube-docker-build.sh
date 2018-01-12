#!/usr/bin/env bash

DIR=$(dirname "$0")
IMAGE=vitorenesduarte/tricks
CONTEXT=${DIR}/../

# Do the erlang release
cd ${CONTEXT}; make rel; cd -

# Build using minikube docker
eval $(minikube docker-env)
docker build -t ${IMAGE} ${CONTEXT}
