
.PHONY: all deps compile clean test release xref

all: compile test

deps:
	(cd apps/data_broadcaster/ && ../../rebar get-deps)

compile: deps
	(cd apps/data_broadcaster/ && ../../rebar compile)

clean:
	(cd apps/data_broadcaster/ && ../../rebar clean)

test:
	(cd apps/data_broadcaster/ && ../../rebar eunit skip_deps=true)

xref: compile
	(cd apps/data_broadcaster/ && ../../rebar xref skip_deps=true)

release:
	(./rebar generate)

