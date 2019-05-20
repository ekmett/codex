all:
	cabal v2-build

run:
	cabal v2-run codex

repl:
	cabal v2-repl codex --repl-options=-fobject-code

test:
	cabal v2-test codex-test

watch:
	ghcid -c 'cabal new-repl codex --repl-options=-fno-code'

.PHONY: all run repl test watch
