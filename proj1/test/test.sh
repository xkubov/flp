#!/bin/bash

PROG="./bkg-2-cnf"
I_DIR="in"
OUT_I="out-i"
OUT_1="out-1"
OUT_2="out-2"

function check_args()
{
	test ! -f $PROG && echo "Application \"$PROG\" does not exist." && exit 1
	test ! -d $I_DIR && echo "Input directory does not exist" && exit 1
	test -z "$(ls $I_DIR)" && echo "Input directory contains no tests." && exit 1
	test ! -d $OUT_I && echo "Control outputs of flag -i do not esist." && exit 1
	test ! -d $OUT_1 && echo "Control outputs of flag -1 do not esist." && exit 1
	test ! -d $OUT_2 && echo "Control outputs of flag -2 do not esist." && exit 1
}

function run()
{
	arg=$1
	test -z "$arg" && echo "Invalid run, provide option" && exit 1

	echo "Testing option $arg"
	failed=""
	failed_count=0
	total=0

	in_files="$(find $I_DIR -type f)"
	for i in $in_files; do
		expected=$(echo $i | sed "s/in/out$arg/" | sed 's/\.in/.out/')
		test "$(cat $expected)" != "$($PROG $arg $i 2>/dev/null)" && failed="$failed $i|$expected" && failed_count=$((++failed_count))
		total=$((++total))
	done

	echo "Passed " $((total-failed_count))/$total "tests"
	if [ ! $failed_count -eq 0 ]; then
		echo "Failed files:" 
		for i in $failed; do echo $i; done
	fi
	echo "==========================="
}

function main()
{
	check_args $@
	run -i
	run -1
	run -2
}

main $@
