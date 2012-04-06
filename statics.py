#!/usr/bin/env python


ROOT_DIR = '.'

STATICS = {
	'js/jasmine.js': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine.js',
	'js/jasmine-html.js': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine-html.js',
	'css/jasmine.css': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine.css',

	'js/coffee-script.js': 'https://github.com/jashkenas/coffee-script/raw/master/extras/coffee-script.js',

	'runtogether.py': 'https://raw.github.com/nh2/runtogether/master/runtogether.py',
}


if __name__ == '__main__':
	import staticfetcher
	staticfetcher.Staticfetcher(STATICS, ROOT_DIR).run()
