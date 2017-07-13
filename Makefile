##############################################################################
# pdfname install

.PHONY : install-bin
install-bin :
	cabal install --enable-tests --disable-documentation

##############################################################################
# Test suite

.PHONY : succeed-tasty
succeed-tasty :
	cd test && \
	../dist/build/pdfname-tests/pdfname-tests --regex-include succeed
	@echo "$@ succeeded-tasty!"

.PHONY : fail
fail :
	cd test && \
	../dist/build/pdfname-tests/pdfname-tests --regex-include fail
	@echo "$@ succeeded"

.PHONY : cl-option
cl-option :
	cd test && \
	../dist/build/pdfname-tests/pdfname-tests --regex-include cl-option
	@echo "$@ succeeded"

.PHONY : test
test :
	make succeed-tasty
	make fail
	make cl-option
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
