.PHONY: json

json:
	rm -rf json/*.json
	yaml_to_json.py < hello-world.yaml > json/hello-world.json
	yaml_to_json.py < implicit_events.yaml > json/implicit_events.json
	yaml_to_json.py < explicit_events.yaml > json/explicit_events.json
