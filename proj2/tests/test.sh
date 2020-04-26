#!/bin/bash

PROG="./flp20-log"
I_DIR="in"
OUT="out"

function check_args()
{
	test ! -f $PROG && echo "Application \"$PROG\" does not exist." && exit 1
	test ! -d $I_DIR && echo "Input directory does not exist" && exit 1
	test -z "$(ls $I_DIR)" && echo "Input directory contains no tests." && exit 1
	test ! -d $OUT && echo "Control outputs do not esist." && exit 1
}

function run()
{
	echo "Testing Turing Machine"
	echo "==========================="
	failed=""
	failed_count=0
	total=0

	in_files="$(find $I_DIR -type f)"
	echo "Measuring times"
	for i in $in_files; do
		expected=$(echo $i | sed "s/in/out/")
		test "$(cat $expected)" != "$($PROG < $i 2>/dev/null)" && failed="$failed $i|$expected" && failed_count=$((++failed_count))
		total=$((++total))
		echo -e "\t$i:" "$($PROG -t < $i | tail -n 1)ms"
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
	run
}

main $@
