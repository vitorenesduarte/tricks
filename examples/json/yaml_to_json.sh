#!/usr/bin/env bash

rm -rf json/*.json

for f in $(ls *.yaml | sed 's/.yaml//g'); do
  json/yaml_to_json.py < ${f}.yaml > json/${f}.json
done


