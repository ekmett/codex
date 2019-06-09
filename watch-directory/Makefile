PWD=$(shell pwd)
LIBRARY=$(shell basename $(PWD))

all:
	cabal v2-build

install:
	cabal v2-install

run:
	cabal v2-run $(LIBRARY)

repl:
	cabal v2-repl $(LIBRARY) --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j

test:
	cabal v2-test spec

watch:
	ghcid -p $(LIBRARY) --color -c "cabal v2-repl $(LIBRARY) --repl-options=-fno-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j"

lint:
	find src -name "*.hs*" -print | xargs hlint

.PHONY: all install run repl test watch lint
