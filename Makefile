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

ifeq ($(shell uname -s),Darwin)
    TIME = /usr/bin/time caffeinate
else
    TIME = /usr/bin/time
endif

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

.PHONY: all install test vtest sdist clean docs gold stamp hlint tags

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
	@rm -rf tests/GoldFiles/*.current

vtest: install
	$(TIME) ./dist/build/sbvPluginTests/sbvPluginTests
	@rm -rf tests/GoldFiles/*.current

# use this as follows: make gold TGT=T49
gold:
	./dist/build/sbvPluginTests/sbvPluginTests -p ${TGT} --accept

sdist: install
	@(set -o pipefail; $(CABAL) sdist | $(SIMPLIFY))

veryclean: clean
	@-ghc-pkg unregister sbvPlugin

clean:
	@rm -rf dist $(STAMPFILE)

docs:
	@(set -o pipefail; $(CABAL) haddock --haddock-option=--no-warnings --hyperlink-source 2>&1 | $(SIMPLIFY))

release: clean install sdist checkLinks hlint docs vtest
	@echo "*** SBVPlugin is ready for release!"

hlint: 
	@rm -f hlintReport.html
	@echo "Running HLint.."
	@hlint Data tests -q -rhlintReport.html -i "Use otherwise"

checkLinks:
	@buildUtils/checkLinks

tags:
	$(call mkTags)
