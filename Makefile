all:
	-ghc-pkg unregister sbvPlugin
	cabal install

clean:
	@rm -rf dist
