#!/usr/bin/env python

SERVER_COMMANDS = [
	"runhaskell Testserver.hs",
	"python -m SimpleHTTPServer 7357",
]

PHANTOM_TEST_COMMAND = "bash -c 'sleep 1 && phantomjs js/run_jasmine_test.coffee test/test.html'"

BROWSER_URL = "http://localhost:7357/test/test.html"


import argparse
import runtogether
import subprocess
import time
import webbrowser
from multiprocessing import Process

def shutdown_callback():
	# TODO try to fix this in runtogether (runhaskell replaces itself with ghc)
	subprocess.call(['pkill',  '-f', 'ghc .*Testserver'])

def runbrowser():
	time.sleep(1)
	webbrowser.open(BROWSER_URL)

def parse_args():
	parser = argparse.ArgumentParser(description='Runs the tests')

	parser.add_argument('--browser', action='store_true', default=False, help='Keeps the test servers running and shows the JS tests in the default browser.')

	return parser.parse_args()

def main():

	args = parse_args()

	commands = SERVER_COMMANDS

	if args.browser:
		# Show the browser tests in the default browser
		Process(target=runbrowser).start()
	else:
		commands += [PHANTOM_TEST_COMMAND]

	runtogether.runtogether(commands, kill_timeout=1, shutdown_callback=shutdown_callback)

if __name__ == '__main__':
	main()
