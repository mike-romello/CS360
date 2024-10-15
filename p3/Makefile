CABAL ?= cabal
GHC ?= ghc
STACK ?= stack

.PHONY : all
all :
	$(STACK) install

.PHONY : clean
clean :
	rm -rf .stack-work bin/while

.PHONY : test
test :
	$(STACK) test
