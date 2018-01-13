PACKAGE     = tricks
REBAR       = rebar3
HOME        = $(shell pwd)

.PHONY: test rel

all: compile

##
## Compilation targets
##

compile:
	$(REBAR) compile

##
## Test targets
##

check: test xref dialyzer lint

test: eunit ct
	${REBAR} cover -v

eunit:
	${REBAR} eunit

ct: clear
	TRICKS_HOME=${HOME} \
		K8S_API_SERVER=$(shell bin/k8s_api_server.sh) \
		K8S_API_TOKEN=$(shell bin/k8s_api_token.sh) \
		${REBAR} ct --readable=false --verbose

cover: test
	open _build/test/cover/index.html

xref:
	${REBAR} xref skip_deps=true

dialyzer:
	${REBAR} dialyzer

lint:
	${REBAR} as lint lint

shell:
	${REBAR} shell --apps ${PACKAGE}

rel:
	rm -rf _build/default/rel/
	${REBAR} release

logs:
	tail -F .lager/*/log/*.log

clear:
	pkill -9 beam.smp ; rm -rf .lager test/.rebar3 *.xml
