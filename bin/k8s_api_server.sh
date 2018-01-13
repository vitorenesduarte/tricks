#!/usr/bin/env bash

function get_api_server {
  ###
  # Return the API server of the current context.
  ###

  kubectl config view --minify=true | grep server | awk '{ print $2 }'
}

get_api_server
