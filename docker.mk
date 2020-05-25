ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR = @`which rebar3`

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib
# export QUIET:=1
# export DEBUG:=1
export REBAR_COLOR:="low"


# use to override vars for your platform
ifeq (env.mk,$(wildcard env.mk))
	include env.mk
endif

.PHONY: deps doc

all: compile

compile:
	$(REBAR) compile

tests:
	$(REBAR) as test ct --spec ./test/spec.spec

tests_cover:
	$(REBAR) as test ct --spec ./test/spec.spec --cover

cover:
	@$(REBAR) cover
	@xdg-open "_build/test/cover/index.html"

doc:
	$(REBAR) edoc

lint:
	$(REBAR) as lint lint

xref:
	$(REBAR) as prod xref skip_deps=true

dialyzer:
	$(REBAR) dialyzer skip_deps=true

deps:
	$(REBAR) deps

clean:
	$(REBAR) clean

prod:
	$(REBAR) as prod release

rel: compile
	$(REBAR) release

run: rel
	$(REBAR) run

auto:
	$(REBAR) auto

attach:
	$(ERL) -name a@127.0.0.1 -remsh dev@127.0.0.1 -setcookie dev

upgrade_rebar:
	$(REBAR) local upgrade
