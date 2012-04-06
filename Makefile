all: init


staticfetcher.py:
	wget https://raw.github.com/nh2/staticfetcher/master/staticfetcher.py


statics_fetch: staticfetcher.py
	python statics.py fetch

statics_fetch_force: staticfetcher.py
	python statics.py fetch --force

statics_clean: staticfetcher.py
	python statics.py clean


clean: statics_clean
	rm -f staticfetcher.py

init: clean statics_fetch_force

test: statics_fetch
	./test.py

browsertest: statics_fetch
	./test.py --browser
