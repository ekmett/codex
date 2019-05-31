all:
	cabal v2-build

install:
	cabal v2-install

repl:
	cabal v2-repl fontconfig:lib:fontconfig --repl-options=-fobject-code --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j

watch:
	ghcid -p fontconfig --color -c 'cabal v2-repl fontconfig:lib:fontconfig --repl-options=-fno-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j'

.PHONY: all install run repl test watch
