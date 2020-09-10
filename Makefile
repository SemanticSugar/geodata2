REBAR ?= $(shell which rebar3 2>/dev/null)
REBAR_FLAGS ?=

all: deps compile

deps:
	$(REBAR) get-deps $(REBAR_FLAGS)

compile:
	$(REBAR) compile $(REBAR_FLAGS)

doc:
	$(REBAR) doc $(REBAR_FLAGS)

test: compile
	$(REBAR) eunit $(REBAR_FLAGS)

clean:
	$(REBAR) clean $(REBAR_FLAGS)
	rm -rf _build

dialyzer:
	$(REBAR) dialyzer $(REBAR_FLAGS)

format:
	$(REBAR) format
