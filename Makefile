REBAR ?= $(shell which rebar3 2>/dev/null || which rebar 2>/dev/null || which ./rebar)
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

clean_plt:
	@rm -f _test/dialyzer_plt

build_plt: build-plt

build-plt:
	@mkdir -p _test
	$(REBAR) build-plt $(REBAR_FLAGS)

dialyzer:
	$(REBAR) dialyze $(REBAR_FLAGS)
