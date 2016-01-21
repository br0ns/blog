all: site

.PHONY: clean watch build deploy

site: site.hs Site
	if [ ! -d .cabal-sandbox ] ; then \
	  cabal sandbox init; \
	  while ! cabal install --only-dependencies ; do :; done; \
	fi
	cabal build
	cabal install --bindir=.

clean: site
	./site clean

watch: site
	./site watch

build: site
	./site build

deploy: site
	./site deploy
