ifneq ("${IN_NIX_SHELL}",)
PROJECT_FILE := --project-file nix-shell-cabal.project
endif

all:
	cabal new-build ${PROJECT_FILE} all

configure:
	cabal v2-configure ${PROJECT_FILE} all

docs:
	@cabal new-haddock ${PROJECT_FILE} all
	@echo
	@echo "All documentation:"
	@find `pwd`/dist-newstyle -type f -name index.html -exec echo 'file://{}' \;

harfbuzz-example:
	cd harfbuzz && cabal new-run ${PROJECT_FILE} example

distclean:
	rm -rf dist-newstyle tags

tags:
	rm -f tags
	fast-tags -R . --exclude=dist-newstyle --exclude=old

.PHONY: all distclean docs tags
