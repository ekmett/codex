all:
	cabal v2-build

install:
	cabal v2-install

run:
	cabal v2-run truetype

repl:
	cabal v2-repl truetype --repl-options=-fobject-code --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j

test:
	cabal v2-test truetype-test

watch:
	ghcid -p truetype --color -c 'cabal v2-repl truetype --repl-options=-fno-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j'

.PHONY: all install run repl test watch
