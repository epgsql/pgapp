REBAR=rebar

all: get-deps compile

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	@$(REBAR) clean

run:
	erl -pa ebin ./deps/*/ebin ./deps/*/include \
	-name pgapp@127.0.0.1 \
	-config "pgapp.config" \
	-eval "application:start(pgapp)."

dialyzer: build.plt compile
	dialyzer --plt $< ebin

build.plt:
	dialyzer -q --build_plt --apps erts kernel stdlib ssl deps/poolboy deps/epgsql --output_plt $@

.PHONY: all compile clean dialyzer
