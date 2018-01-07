.PHONY: yaml-to-json

JAR=target/bundle.jar

jar:
	mvn clean compile assembly:single
	mv target/examples-0.1-jar-with-dependencies.jar $(JAR)

yaml-to-json:
	json/yaml_to_json.sh

docker-build: jar
	docker/docker-build.sh
