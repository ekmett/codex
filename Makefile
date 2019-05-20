all:
	cabal v2-build

run:
	cabal v2-run codex

repl:
	cabal v2-repl codex --repl-options=-fobject-code

test:
	cabal v2-test codex-test

watch:
	ghcid -p codex --color -c 'cabal new-repl codex --repl-options=-fno-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j'

.PHONY: all run repl test watch
