.PHONY: json

JAR=target/bundle.jar

jar:
	mvn clean compile assembly:single
	mv target/example-0.1-jar-with-dependencies.jar $(JAR)

yaml-to-json:
	rm -rf json/*.json
	json/yaml_to_json.py < hello-world.yaml > json/hello-world.json
	json/yaml_to_json.py < implicit-events.yaml > json/implicit-events.json
	json/yaml_to_json.py < explicit-events.yaml > json/explicit-events.json

docker-build: jar
	docker/docker-build.sh
