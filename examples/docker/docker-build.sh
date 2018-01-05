#!/usr/bin/env bash

DIR=$(dirname "$0")
CONTEXT=${DIR}/../
IMAGE=vitorenesduarte/tricks-example
DOCKERFILE=${DIR}/Dockerfile

# build image
docker build --no-cache \
  -t ${IMAGE} \
  -f ${DOCKERFILE} ${CONTEXT}

# push image
docker push ${IMAGE}
