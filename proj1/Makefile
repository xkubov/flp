# Author : Peter Kubov
# Login  : xkubov06
# Project: Functional project: bkg-2-cnf
# License: GPL-3
# Year   : 2020

## Output of the project.
PROJ = bkg-2-cnf

## Compiler to be used.
HC = ghc

## Source dir
SRC_DIR=src

## Zip name
ZIP_NAME=flp-fun-xkubov06.zip

SRC=$(SRC_DIR)/Main.hs

SRC_FILES = src/Main.hs     \
	    src/CFGAlgs.hs  \
	    src/CFGParse.hs \
	    src/CFGData.hs

DOC_FILES = doc/README.md \
	    doc/test-description.txt

TEST_FILES = test/test.cfg

FILES=Makefile     \
      $(SRC_FILES) \
      $(DOC_FILES) \
      $(TEST_FILES)

$(PROJ): $(SRC)
	$(HC) -i$(SRC_DIR) $(SRC) -o $(PROJ)

.PHONY:
clean:
	@rm -f $(PROJ) $(SRC_DIR)/*.o $(SRC_DIR)/*.hi *.zip

.PHONY:
zip:
	zip $(ZIP_NAME) $(FILES)
