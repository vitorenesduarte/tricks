#!/usr/bin/env bash

DIR=$(dirname "$0")
IMAGE=vitorenesduarte/tricks
CONTEXT=${DIR}/../
eval $(minikube docker-env)
docker build -t ${IMAGE} ${CONTEXT}
