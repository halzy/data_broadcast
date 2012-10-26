
.PHONY: all deps compile clean test release xref

all: compile test

deps:
	./rebar get-deps

compile: deps
	./rebar compile

clean:
	./rebar clean

test:
	./rebar eunit skip_deps=true

xref: compile
	./rebar xref skip_deps=true

release: compile
	./rebar generate

