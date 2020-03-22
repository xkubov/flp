#
# Author: Peter Kubov
#

## Output of the project.
PROJ = bkg-2-cnf

## Compiler to be used.
HC = ghc

## Source dir
SRC_DIR=src

SRC=$(SRC_DIR)/bkg-2-cnf.hs

$(PROJ): $(SRC)
	$(HC) -i$(SRC_DIR) $(SRC) -o $(PROJ)

.PHONY:
clean:
	@rm $(PROJ) $(SRC_DIR)/*.o $(SRC_DIR)/*.hi
