##############################################################################
# pdfname install

.PHONY : install-bin
install-bin :
	cabal install --enable-tests --disable-documentation

##############################################################################
# Test suite

PDFNAME_BIN = ../dist/build/pdfname-tests/pdfname-tests
TESTS_OPTIONS =-i

.PHONY : succeed
succeed :
	cd test && \
	$(PDFNAME_BIN) $(TESTS_OPTIONS) --regex-include succeed
	@echo "$@ succeeded!"

.PHONY : fail
fail :
	cd test && \
	$(PDFNAME_BIN) $(TESTS_OPTIONS) --regex-include fail
	@echo "$@ succeeded"

.PHONY : cl-option
cl-option :
	cd test && \
	$(PDFNAME_BIN) $(TESTS_OPTIONS) --regex-include cl-option
	@echo "$@ succeeded"

.PHONY : test
test :
	cd test && \
	$(PDFNAME_BIN) $(TESTS_OPTIONS)
	@echo "$@ succeeded"

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
