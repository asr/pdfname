##############################################################################
# pdfname install

.PHONY : install-bin
install-bin :
	cabal install --disable-documentation

##############################################################################
# Test suite

# Tested with shelltestrunner 1.3.5.
.PHONY : test
test :
	shelltest --color \
	          --execdir \
	          --precise \
	          test/succeed.test
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Haddock test

.PHONY : haddock
haddock :
	cabal configure
	cabal haddock --executables \
	              --haddock-option=--use-unicode \
	              --hyperlink-source
	@echo "$@ succeeded!"

##############################################################################
hlint :
	hlint src/

##############################################################################
.PHONY : clean
clean :
	cabal clean
	@echo "$@ succeeded!"
