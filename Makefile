REBAR=./rebar3

all: get-deps compile

$(REBAR):
	wget https://s3.amazonaws.com/rebar3/rebar3
	chmod +x rebar3

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
	-eval "application:ensure_all_started(pgapp)."

dialyzer: build.plt compile
	dialyzer --plt $< ebin

build.plt:
	dialyzer -q --build_plt --apps erts kernel stdlib ssl deps/poolboy deps/epgsql --output_plt $@

.PHONY: all compile clean dialyzer
