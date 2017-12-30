#!/usr/bin/env bash

DIR=$(dirname "$0")
IMAGE=vitorenesduarte/quad-example
DOCKERFILE=${DIR}/Dockerfile

# build image
docker build --no-cache \
  -t ${IMAGE} \
  -f ${DOCKERFILE} ${DIR}

# push image
docker push ${IMAGE}
