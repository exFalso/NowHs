#!/usr/bin/env python


ROOT_DIR = '.'

STATICS = {
	'js/jasmine.js': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine.js',
	'js/jasmine-html.js': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine-html.js',
	'css/jasmine.css': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine.css',

	'js/coffee-script.js': 'https://raw.github.com/jashkenas/coffee-script/master/extras/coffee-script.js',

	# TODO Get this from upstream once merged (https://github.com/jcarver989/phantom-jasmine/pull/1)
	'js/console-runner.js': 'https://raw.github.com/nh2/phantom-jasmine/fix-long-running-tests/lib/console-runner.js',
	'js/run_jasmine_test.coffee': 'https://raw.github.com/nh2/phantom-jasmine/fix-long-running-tests/lib/run_jasmine_test.coffee',

	'runtogether.py': 'https://raw.github.com/nh2/runtogether/master/runtogether.py',
}


if __name__ == '__main__':
	import staticfetcher
	staticfetcher.Staticfetcher(STATICS, ROOT_DIR).run()
