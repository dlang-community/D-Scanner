
rm -f test
rm -f test.o

dmd\
	main.d\
	stats.d\
	imports.d\
	highlighter.d\
	ctags.d\
	astprinter.d\
	outliner.d\
	libdparse/src/std/*.d\
	libdparse/src/std/d/*.d\
	inifiled/source/*.d\
	analysis/*.d\
	-oftest\
	-g -unittest

./test

