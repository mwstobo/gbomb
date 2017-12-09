JBUILD = jbuilder
BIN = gbomb

all: build

clean:
	$(JBUILD) clean

build:
	$(JBUILD) build
