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
	TRICKS_HOME=${HOME} ${REBAR} ct --readable=false --verbose

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
	rm -rf _build/defaul/rel
	${REBAR} release

docker-rel: rel
	docker build -t vitorenesduarte/${PACKAGE} .

docker: docker-rel
	docker push vitorenesduarte/${PACKAGE}

logs:
	tail -F .lager/*/log/*.log

clear:
	pkill -9 beam.smp ; rm -rf .lager test/.rebar3 *.xml
