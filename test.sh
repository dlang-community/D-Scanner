
rm -f test
rm -f test.o

dmd\
	src/*.d\
	libdparse/src/std/*.d\
	libdparse/src/std/d/*.d\
	inifiled/source/*.d\
	src/analysis/*.d\
	-oftest\
	-g -unittest

./test

rm -f test test.o
