#!/usr/bin/env python

import yaml, json, sys
sys.stdout.write(json.dumps(yaml.load(sys.stdin)))
