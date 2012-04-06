#!/usr/bin/env python

commands = [
	"runhaskell Testserver.hs",
	"python -m SimpleHTTPServer 7357",
]

if __name__ == '__main__':
	import runtogether
	import subprocess
	# TODO try to fix this in runtogether (runhaskell replaces itself with ghc)
	def shutdown_callback():
		subprocess.call(['pkill',  '-f', 'ghc .*Testserver'])
	runtogether.runtogether(commands, 1, shutdown_callback=shutdown_callback)
