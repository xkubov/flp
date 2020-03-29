# Makefile for FLP project 2
# Author: Peter Kubov
# Date: 28. 3. 2020

OUT=flp20-log
SRC=turing-machine.pl
CC=swipl
FLAGS=--goal=main

$(OUT): $(SRC)
	$(CC) $(FLAGS) -o $(OUT) -c $(SRC)

clean:
	@rm -f $(OUT)
