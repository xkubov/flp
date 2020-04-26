# Makefile for FLP project 2
# Author: Peter Kubov
# Date: 28. 3. 2020

OUT=flp20-log
SRC=turing-machine.pl
CC=swipl
FLAGS=--goal=main
TEST_DIR=tests

.PHONY: tests

$(OUT): $(SRC)
	$(CC) $(FLAGS) -o $(OUT) -c $(SRC)

tests: $(OUT)
	@cp $(OUT) $(TEST_DIR)
	@cd $(TEST_DIR) && bash test.sh
	@rm $(TEST_DIR)/$(OUT)

clean:
	@rm -f $(OUT)

zip:
	@zip -r flp-log-xkubov06.zip Makefile README.md tests turing-machine.pl
