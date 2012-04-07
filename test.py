#!/usr/bin/env python

SERVER_COMMANDS = [
	"./dist/build/testserver/testserver",
	"bash -c 'python3 -m http.server 7357 || python -m SimpleHTTPServer 7357'",
]

PHANTOM_TEST_COMMAND = "bash -c 'sleep 1 && phantomjs js/run_jasmine_test.coffee test/test.html'"

BROWSER_URL = "http://localhost:7357/test/test.html"

# Seconds to wait before starting the browser when --browser is given
BROWSER_DELAY = 2

import argparse
import runtogether
import subprocess
import time
import webbrowser
from multiprocessing import Process

def runbrowser():
	try:
		time.sleep(BROWSER_DELAY)
		webbrowser.open(BROWSER_URL)
	except KeyboardInterrupt:
		print("Browser start interrupted")

def parse_args():
	parser = argparse.ArgumentParser(description='Runs the tests')

	parser.add_argument('--browser', action='store_true', default=False, help='Keeps the test servers running and shows the JS tests in the default browser.')

	return parser.parse_args()

def main():

	args = parse_args()

	commands = SERVER_COMMANDS

	procs = []

	if args.browser:
		# Show the browser tests in the default browser
		browserProc = Process(target=runbrowser)
		procs += [browserProc]
		browserProc.start()
	else:
		commands += [PHANTOM_TEST_COMMAND]

	def killProcs():
		for proc in procs:
			proc.terminate()

	runtogether.runtogether(commands, kill_timeout=1, shutdown_callback=killProcs)

if __name__ == '__main__':
	main()
