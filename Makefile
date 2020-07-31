# (c) Copyright Levent Erkok. All rights reserved.
#
# sbvPlugin is distributed with the BSD3 license. See the LICENSE file in the distribution for details.
SHELL     := /usr/bin/env bash
CABAL     = cabal

ifeq ($(shell uname -s),Darwin)
    TIME = /usr/bin/time caffeinate
else
    TIME = /usr/bin/time
endif

.PHONY: all install test vtest sdist clean docs gold hlint tags

all: install

install: $(DEPSRCS) Makefile
	@fast-tags -R --nomerge .
	@$(CABAL) new-configure --disable-library-profiling --enable-tests
	@$(CABAL) new-install --lib

test: install
	$(TIME) $(CABAL) new-test
	@rm -rf tests/GoldFiles/*.current

vtest: install
	$(TIME) cabal new-run sbvPluginTests
	@rm -rf tests/GoldFiles/*.current

# use this as follows: make gold TGT=T49
gold:
	cabal new-run sbvPluginTests -- -p ${TGT} --accept

# recreate all golds
allgold:
	cabal new-run sbvPluginTests -- --accept

sdist: install
	$(CABAL) new-sdist

veryclean: clean

clean:
	@rm -rf dist dist-newstyle

release: clean install sdist hlint docs vtest checkLinks
	@echo "*** SBVPlugin is ready for release!"

hlint: 
	@rm -f hlintReport.html
	@echo "Running HLint.."
	@hlint Data tests -i "Use otherwise" -i "Use module export list"

checkLinks:
	@brok --no-cache --only-failures $(DEPSRCS) COPYRIGHT INSTALL LICENSE $(wildcard *.md)

tags:
	@fast-tags -R --nomerge .
