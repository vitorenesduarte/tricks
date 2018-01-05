.PHONY: json

json:
	rm -rf json/*.json
	json/yaml_to_json.py < hello-world.yaml > json/hello-world.json
	json/yaml_to_json.py < implicit_events.yaml > json/implicit_events.json
	json/yaml_to_json.py < explicit_events.yaml > json/explicit_events.json

build:
	docker/docker-build.sh
