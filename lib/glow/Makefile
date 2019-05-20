all:
	cabal v2-build

install:
	cabal v2-install

#run:
#	cabal v2-run glow

repl:
	cabal v2-repl glow --repl-options=-fobject-code --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j

#test:
#	cabal v2-test glow-test

watch:
	ghcid -p glow --color -c 'cabal v2-repl glow --repl-options=-fno-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j'

.PHONY: all install run repl test watch
