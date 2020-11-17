.PHONY: all compile run test clean dialyzer
.PHONY: build_plt dialyzer

DIALYZER_APPS= asn1 compiler crypto erts inets kernel public_key sasl ssl stdlib syntax_tools tools

REBAR=./rebar3

all: $(REBAR) compile

compile:
	$(REBAR) install_deps
	$(REBAR) compile

run:
	erl -pa _build/default/lib/*/ebin

test:
	$(REBAR) ct skip_deps=true verbose=3

clean:
	$(REBAR) clean
	rm -rf ./test/*.beam
	rm -rf ./test/TEST*.xml
	rm -rf ./erl_crash.dump

build_plt: clean compile
ifneq ("$(wildcard erlang.plt)","")
	@echo "Erlang plt file already exists"
else
	dialyzer --build_plt --output_plt erlang.plt --apps $(DIALYZER_APPS)
endif
ifneq ("$(wildcard eredis_sync.plt)","")
	@echo "eredis_sync plt file already exists"
else
	dialyzer --build_plt --output_plt eredis_sync.plt _build/default/lib/*/ebin
endif

add_to_plt: build_plt
	dialyzer --add_to_plt --plt erlang.plt --output_plt erlang.plt.new --apps $(DIALYZER_APPS)
	dialyzer --add_to_plt --plt eredis_sync.plt --output_plt eredis_sync.plt.new _build/default/lib/*/ebin
	mv erlang.plt.new erlang.plt
	mv eredis_sync.plt.new eredis_sync.plt

dialyzer:
	dialyzer --src src --plts erlang.plt eredis_sync.plt -Wunmatched_returns -Werror_handling -Wrace_conditions
