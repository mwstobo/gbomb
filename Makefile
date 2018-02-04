.PHONY: test
JBUILD = jbuilder
BIN = gbomb

all: build test

clean:
	$(JBUILD) clean

build:
	$(JBUILD) build

test:
	$(JBUILD) runtest
