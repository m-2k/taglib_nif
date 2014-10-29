REBAR = rebar

.PHONY: all clean erl test doc

all: clean erl test

erl:
	$(REBAR) compile

test:
	$(REBAR) eunit

clean:
	$(REBAR) clean

doc:
	$(REBAR) doc


