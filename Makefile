##############################################################################
# pdfname install

.PHONY : install-bin
install-bin :
	cabal install --enable-tests --disable-documentation

##############################################################################
# Test suite

.PHONY : succeed-tasty
succeed-tasty :
	cd test && ../dist/build/pdfname-tests/pdfname-tests
	@echo "$@ succeeded!"

# Tested with shelltestrunner 1.3.5.
.PHONY : succeed-shelltestrunner
succeed-shelltestrunner :
	shelltest --color \
	          --execdir \
	          --precise \
	          test/succeed.test
	@echo "$@ succeeded!"

# Tested with shelltestrunner 1.3.5.
.PHONY : fail
fail :
	shelltest --color \
	          --execdir \
	          --precise \
	          test/fail.test
	@echo "$@ succeeded!"

.PHONY : test
test :
	make succeed-tasty
	make succeed-shelltestrunner
	make fail
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
