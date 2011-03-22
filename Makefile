.PHONY: all clean

GHC_FLAGS=-XMultiParamTypeClasses -XFlexibleInstances -XTypeSynonymInstances

all:
	cd src; ghc $(GHC_FLAGS) -outputdir ../bin --make main.hs
	cd test; runhaskell $(GHC_FLAGS) -i../src main.hs

clean:
	git clean -d -X -f
