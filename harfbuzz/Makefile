PWD=$(shell pwd)
LIBRARY=$(shell basename $(PWD))

all:
	cabal v2-build

install:
	cabal v2-install

docs:
	@RESULT=`cabal new-haddock $(LIBRARY) 2>/dev/null | tail -n 2`; \
	if [[ `echo $$RESULT | head -c 22` = "Documentation created:" ]]; then \
		$(OPEN) `echo $$RESULT | tail -c +23`; \
	fi

run:
	cabal v2-run $(LIBRARY)

repl:
	cabal v2-repl $(LIBRARY) --repl-options=-fobject-code --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j

test:
	cabal v2-run spec -- --color always

watch:
	ghcid -p $(LIBRARY) --color -c 'cabal v2-repl $(LIBRARY) --repl-options=-fno-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j'

lint:
	find src -name "*.hs*" -print | xargs hlint

.PHONY: all install run repl test watch lint

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	OPEN = open
else
	OPEN = echo
endif
