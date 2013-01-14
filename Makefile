# See LICENSE for licensing information.

# Variables definitions:
#  variable definition expanded when it's declared
APP := paperless

#  conditional variable definition, assign only if not yet assigned.
ERL ?= erl

#  variable definition recursively expanded when the variable is used,
#  not when it's declared

REBAR             = `which rebar || echo ./rebar`
REBAR_SKIP_DEPS   = skip_deps=true

# configuration for dialyzer
DIALYZER_WARNINGS = -Wunmatched_returns -Werror_handling \
	-Wrace_conditions \
	-Wunderspecs \
	-Wno_undefined_callbacks
DIALYZER_APPS     = --apps kernel stdlib sasl erts ssl \
	tools mnesia inets

# configuration for release paths & names
RELEASES_PATH         = rel/releases
LIVE_PATH             = $(RELEASES_PATH)/live
RELEASE_PATH          = rel/$(APP)
PREVIOUS_NAME         = $(APP)_previous
PREVIOUS_RELEASE_PATH = $(RELEASES_PATH)/$(PREVIOUS_NAME)
LIVE_BIN              = $(LIVE_PATH)/bin/$(APP)

RELEASE_TAR       = $(shell ls rel/*.tar.gz)
RELEASE_NAME      = $(shell basename $(RELEASE_TAR) .tar.gz)
# Makefile targets format:
#
#	target: dependencies
#	[tab] system command

# Tells to make that deps is not a file/directory
.PHONY: deps

# default task
all: deps compile

# Compiles erlang sources
compile:
	mkdir -p db
	@$(REBAR) compile

# Makes single "exec" file
escript:
	@$(REBAR) $(REBAR_SKIP_DEPS) escriptize

# Cleans all files
clean:
	@$(REBAR) clean

# Pulls all dependencies
deps:
	@$(REBAR) get-deps

format:
	find apps/ -name *.erl -exec script/erlformat {} \;

# Removes whole dependencies
distclean:
	@$(REBAR) delete-deps

# Generates documentation
doc:
	@$(REBAR) doc skip_deps=true

# Runs eunit tests
eunit:
	@$(REBAR) $(REBAR_SKIP_DEPS) eunit skip_deps=true

# Runs ct tests
ct:
	@$(REBAR) $(REBAR_SKIP_DEPS) ct

# Runs all tests clean
test: clean all eunit #ct

# Runs all tests clean
quick_test: compile dialyze eunit #ct

# Generates plt file for project
build_plt: all
	dialyzer --build_plt --output_plt .$(APP)_dialyzer.plt \
		$(DIALYZER_APPS) deps/*/ebin

# Examines project using builded plt:
#
#  What is Dialyzer?
#
#    Dialyzer is a tool that uses static analysis to identify
#    discrepancies in Erlang code base
#
dialyze: compile
	dialyzer apps/*/ebin --plt .$(APP)_dialyzer.plt \
	$(DIALYZER_WARNINGS)

# Show type definitions:
#
#  What is TypEr?
#
#    TypEr is a tool that automatically inserts type annotations
#    in Erlang code
#
#  The aims of TypEr:
#    * Facilitate documentation of Erlang code.
#    * Provide help to understand legacy code.
#    * Encourage a type-aware development of Erlang programs.
typer:
	typer -I apps/*/include \
		-I deps/*/include \
	-pa deps/lager/ebin \
		-pa deps/ejson/ebin \
		--plt .$(APP)_dialyzer.plt \
		apps/*/src

# Ensures that LIVE is down
ensure_stop:
	@$(LIVE_BIN) stop || true

# Prepers first live release:
#   - stop running instances if they are
#   - remove old releases (previous, live and current)
#   - generate inital release
#   - copy initial release as LIVE version
#   - copy initial release as PREVIOUS version
prepare_live: ensure_stop
	@echo "==> checking for running instances... kill them all ;)"
	@echo "==> remove all releases :"
	@echo "    ($(RELEASES_PATH)/$(APP), $(RELEASES_PATH)/live)."
	@mkdir -p $(RELEASES_PATH)
	rm -Rf $(PREVIOUS_RELEASE_PATH)
	rm -Rf $(RELEASE_PATH)
	rm -Rf $(LIVE_PATH)
	@$(REBAR) generate
	cp -Rf $(RELEASE_PATH) $(LIVE_PATH)
	cp -Rf $(RELEASE_PATH) $(PREVIOUS_RELEASE_PATH)

# Runs new clean live start, but bufore start just test code and makes
# sure that all green, and now warnings form dialyzer
start_live: test all prepare_live start

# Starts LIVE instance
start:
	@echo "==> starting $(APP)"
	@$(LIVE_BIN) start && echo "ok" \
		|| true

# Restarts LIVE indance
restart:
	@echo "==> restarting $(APP)"
	@$(LIVE_BIN) restart \
		|| true

# Stops LIVE instance
stop:
	@echo "==> stoping $(APP)"
	@$(LIVE_BIN) stop \
		|| true

# Generates new release:
#   - runs tests & dialyzer
#   - remove old PREVIOUS release
#   - move old CURRENT release as new PREVIOUS
#   - generate new CURRENT release
release: test
	@echo "==> generating release"
	@$(REBAR) generate

# Generate new upgrade package based on PREVIOUS version:
#   - remove all old *.tar.gz packages
#   - generate new one
generate_upgrade: release
	rm -Rf rel/*.tar.gz
	cp -Rf $(PREVIOUS_RELEASE_PATH) rel/$(PREVIOUS_NAME)
	@$(REBAR) generate-appups previous_release=$(PREVIOUS_NAME)
	@$(REBAR) generate-upgrade previous_release=$(PREVIOUS_NAME)

# Hot deploy new release on LIVE
upgrade: generate_upgrade
	@echo "==> upgrade"
	mv $(shell ls rel/*.tar.gz) $(LIVE_PATH)/releases
	@$(LIVE_BIN) upgrade $(shell basename `ls rel/*.tar.gz` .tar.gz)
	rm -Rf rel/$(PREVIOUS_NAME)
	rm -Rf $(PREVIOUS_RELEASE_PATH)
	mv $(RELEASE_PATH) $(PREVIOUS_RELEASE_PATH)

# Use PREVIOUS release as LIVE
prepare_rollback:
	@echo "==> rollback (use previous release as live)"
	rm -Rf $(LIVE_PATH)
	cp -Rf $(PREVIOUS_RELEASE_PATH) $(LIVE_PATH)

# Cold rollback
rollback: ensure_stop prepare_rollback start

# Attach shell to running LIVE instance
attach:
	@$(LIVE_BIN) attach
