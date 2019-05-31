all:
	cabal new-build all

docs:
	@cabal new-haddock all
	@echo
	@echo "All documentation:"
	@find `pwd`/dist-newstyle -type f -name index.html -exec echo 'file://{}' \;

distclean:
	rm -rf dist-newstyle

.PHONY: all distclean
