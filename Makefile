# (c) Copyright Levent Erkok. All rights reserved.
#
# sbvPlugin is distributed with the BSD3 license. See the LICENSE file in the distribution for details.
SHELL     := /usr/bin/env bash
CABAL     = cabal
CONFIGOPTS = "-Wall -fhide-source-paths"

ifeq ($(shell uname -s),Darwin)
    TIME = /usr/bin/time caffeinate
else
    TIME = /usr/bin/time
endif

.PHONY: all install test vtest sdist clean docs gold hlint tags ci

all: install

install: $(DEPSRCS) Makefile
	@fast-tags -R --nomerge .
	@$(CABAL) new-configure --disable-library-profiling --enable-tests --ghc-options=$(CONFIGOPTS)
	@$(CABAL) new-install --lib

test:
	$(TIME) $(CABAL) new-test
	@rm -rf tests/GoldFiles/*.current

vtest:
	$(TIME) cabal new-run sbvPluginTests
	@rm -rf tests/GoldFiles/*.current

docs:
	cabal new-haddock --haddock-option=--hyperlinked-source --haddock-option=--no-warnings --haddock-option="--optghc=-DHADDOCK"

# use this as follows: make gold TGT=T49
gold:
	cabal new-run sbvPluginTests -- -p ${TGT} --accept

# recreate all golds
allgold:
	cabal new-run sbvPluginTests -- --accept

ghcid:
	ghcid --command="cabal new-repl --repl-options=-Wno-unused-packages"

sdist: install
	$(CABAL) new-sdist

veryclean: clean

clean:
	@rm -rf dist dist-newstyle cabal.project.local*

release: clean install sdist hlint docs vtest checkLinks
	@echo "*** SBVPlugin is ready for release!"

hlint: 
	@rm -f hlintReport.html
	@echo "Running HLint.."
	@hlint Data tests -i "Use otherwise" -i "Use module export list"

checkLinks:
	@brok --no-cache --only-failures $(DEPSRCS) COPYRIGHT INSTALL LICENSE $(wildcard *.md)

ci:
	haskell-ci sbvPlugin.cabal --no-tests --no-benchmarks --no-doctest --no-hlint --email-notifications --no-haddock

tags:
	@fast-tags -R --nomerge .
