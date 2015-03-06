REBAR=rebar

all: compile

compile:
	$(REBAR) compile

clean:
	@$(REBAR) clean

dialyzer: build.plt compile
	dialyzer --plt $< ebin

build.plt:
	dialyzer -q --build_plt --apps erts kernel stdlib ssl deps/poolboy deps/epgsql --output_plt $@

.PHONY: all compile clean dialyzer
