all: site

.PHONY: site clean watch deploy

site:
	if [ ! -d .cabal-sandbox ] ; then \
	  cabal sandbox init; \
	  cabal install --only-dependencies; \
	fi
	cabal build
	cabal install --bindir=.

clean:
	./site clean

watch:
	./site watch

build:
	./site build

deploy:
	./site deploy
