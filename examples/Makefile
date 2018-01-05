.PHONY: json

json:
	rm -rf json/*.json
	json/yaml_to_json.py < hello-world.yaml > json/hello-world.json
	json/yaml_to_json.py < implicit-events.yaml > json/implicit-events.json
	json/yaml_to_json.py < explicit-events.yaml > json/explicit-events.json

build:
	docker/docker-build.sh
