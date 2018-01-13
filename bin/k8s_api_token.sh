#!/usr/bin/env bash

function get_api_token {
  ###
  # Return the API server token of the current context.
  ###

  local _secret=$(kubectl get secrets | grep default | awk '{ print $1 }')
  kubectl describe secret ${_secret} | grep -E "^token" | awk '{ print $2 }'
}

get_api_token
