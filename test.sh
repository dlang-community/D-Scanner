
rm -f test
rm -f test.o

dmd\
	*.d\
	libdparse/src/std/*.d\
	libdparse/src/std/d/*.d\
	inifiled/source/*.d\
	analysis/*.d\
	-oftest\
	-g -unittest

./test

