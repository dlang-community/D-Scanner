
rm -f test
rm -f test.o

dmd\
	src/*.d\
	libdparse/src/std/*.d\
	libdparse/src/std/d/*.d\
	inifiled/source/*.d\
	src/analysis/*.d\
	-oftest\
	-g -unittest\
	-J.

./test

rm -f test test.o
