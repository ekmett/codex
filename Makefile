all:
	cabal new-build all

docs:
	@cabal new-haddock all
	@echo
	@echo "All documentation:"
	@find `pwd`/dist-newstyle -type f -name index.html -exec echo 'file://{}' \;

harfbuzz-example:
	cd harfbuzz && cabal new-run example

distclean:
	rm -rf dist-newstyle tags

tags:
	rm -f tags
	fast-tags -R . --exclude=dist-newstyle --exclude=old

.PHONY: all distclean docs tags
