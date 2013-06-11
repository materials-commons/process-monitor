APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	public_key mnesia syntax_tools compiler
COMBO_PLT = $(HOME)/.pm_combo_dialyzer_plt

.PHONY: deps test rel

all: deps compile

compile: deps
	./rebar compile

deps:
	./rebar get-deps

generate:
	./rebar generate

cli: compile
	./rebar -C rebar.pmctl.config escriptize

#cli: compile
#	./rebar escriptize
#	./rebar -C rebar.config.sfpw escriptize

rel: stop-epmd rel-config deps compile cli generate

devrel: stop-epmd devl-rel-config deps compile cli generate

stop-epmd:
	-epmd -kill

rel-config:
	cp rel/files/app.config.rel rel/files/app.config

dev-rel-config:
	cp rel/files/app.config.dev rel/files/app.config

relclean:
	rm -rf rel/process_monitor

appclean:
	rm -f ebin/*.beam

clean: distclean
	./rebar clean

distclean:
	./rebar delete-deps

test: all
	./rebar skip_deps=true eunit

docs: deps
	./rebar skip_deps=true doc

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	dialyzer --plt $(COMBO_PLT) ebin
