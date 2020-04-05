# Author : Peter Kubov
# Login  : xkubov06
# Project: Functional project: bkg-2-cnf
# License: GPL-3
# Year   : 2020

## Output of the project.
PROJ = bkg-2-cnf

## Compiler to be used.
HC = ghc

## Important dirs
SRC_DIR=src
DOC_DIR=doc
TEST_DIR=test

## Zip name
ZIP_NAME=flp-fun-xkubov06.zip

## Main compilation file
SRC=$(SRC_DIR)/Main.hs

SRC_FILES = $(SRC_DIR)/Main.hs     \
	    $(SRC_DIR)/CFGAlgs.hs  \
	    $(SRC_DIR)/CFGParse.hs \
	    $(SRC_DIR)/CFGData.hs

DOC_FILES = $(DOC_DIR)/README.md \
	    $(DOC_DIR)/test-description.txt

TEST_FILES = test/in   \
	     test/out* \
	     test/test.sh

FILES=Makefile     \
      $(SRC_FILES) \
      $(DOC_FILES) \
      $(TEST_FILES)

$(PROJ): $(SRC_FILES)
	$(HC) -i$(SRC_DIR) $(SRC) -o $(PROJ)

.PHONY:
tests: $(PROJ)
	@cp $(PROJ) $(TEST_DIR)
	@cd $(TEST_DIR) && bash test.sh
	@rm $(TEST_DIR)/$(PROJ)

.PHONY:
clean:
	@rm -f $(PROJ) $(SRC_DIR)/*.o $(SRC_DIR)/*.hi *.zip

.PHONY:
zip:
	zip -r $(ZIP_NAME) $(FILES)
