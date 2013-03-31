all: clean init build


staticfetcher.py:
	wget https://raw.github.com/nh2/staticfetcher/master/staticfetcher.py

.PHONY: statics_fetch statics_fetch_force statics_clean

statics_fetch: staticfetcher.py
	python statics.py fetch

statics_fetch_force: staticfetcher.py
	python statics.py fetch --force

statics_clean: staticfetcher.py
	python statics.py clean


.PHONY: clean init build test browsertest

clean:
	(test -f staticfetcher.py && make statics_clean) || true
	rm -f staticfetcher.py
	rm -f .cabal-configured

init: statics_fetch_force

.cabal-configured: *.cabal
	cabal configure --disable-library-profiling --disable-shared
	touch .cabal-configured

build: .cabal-configured
	cabal build

test: build statics_fetch
	./test.py

browsertest: build statics_fetch
	./test.py --browser
