# (c) Copyright Levent Erkok. All rights reserved.
#
# sbvPlugin is distributed with the BSD3 license. See the LICENSE file in the distribution for details.
SHELL     := /usr/bin/env bash
STAMPFILE = Data/SBV/Plugin/SBVPluginBuildTime.hs
DEPSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' -or -name '*.cabal' | grep -v Paths_sbvPlugin.hs | grep -v $(STAMPFILE) | grep -v dist-sandbox)
TESTSRCS  = $DEPSRCS
CABAL     = cabal
SIMPLIFY  = ./buildUtils/simplify
EXTRAOPTS = "--ghc-options=-Werror -Wall"
TIME      = /usr/bin/time caffeinate

define mkStamp
	@echo "-- Auto-generated, don't edit"		     >  ${STAMPFILE}
	@echo "module SBVPluginBuildTime (buildTime) where"  >> ${STAMPFILE}
	@echo ""					     >> ${STAMPFILE}
	@echo "buildTime :: String"			     >> ${STAMPFILE}
	@echo "buildTime = \"$(shell date)\""		     >> ${STAMPFILE}
endef

define mkTags
	@find . -name \*.\*hs | xargs fast-tags
endef

.PHONY: all install test sdist clean docs gold stamp hlint tags

all: install

install: $(STAMPFILE)

$(STAMPFILE): $(DEPSRCS) Makefile
	@-ghc-pkg unregister --force sbvPlugin
	@(make -s -C buildUtils)
	$(call mkStamp)
	$(call mkTags)
	@$(CABAL) configure --disable-library-profiling --enable-tests
	@((set -o pipefail; $(CABAL) build $(EXTRAOPTS) 2>&1 | $(SIMPLIFY)) || (rm $(STAMPFILE) && false))
	@$(CABAL) copy
	@$(CABAL) register

test: install
	$(TIME) $(CABAL) test

sdist: install
	@(set -o pipefail; $(CABAL) sdist | $(SIMPLIFY))

veryclean: clean
	@-ghc-pkg unregister sbvPlugin

clean:
	@rm -rf dist $(STAMPFILE)

docs:
	@(set -o pipefail; $(CABAL) haddock --haddock-option=--no-warnings --hyperlink-source 2>&1 | $(SIMPLIFY))

release: clean install sdist hlint docs test
	@echo "*** SBVPlugin is ready for release!"

hlint: 
	@rm -f hlintReport.html
	@echo "Running HLint.."
	@hlint Data tests -q -rhlintReport.html -i "Use otherwise"

tags:
	$(call mkTags)

# use this as follows: make gold TGTS="T01"
# where the tag is one (or many) given in the SBVUnitTest.hs file
# if TGTS is not specified, then all gold files are regenerated
golds:
	dist/build/sbvPluginTests/sbvPluginTests --gold ${TGTS}
