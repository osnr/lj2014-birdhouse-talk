build/%.js: %.elm
	elm --only-js $<

elm-examples: $(patsubst %.elm, build/%.js, $(wildcard *.elm))

all: elm-examples

