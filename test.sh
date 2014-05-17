
rm -f test
rm -f test.o

dmd\
	main.d\
	stats.d\
	imports.d\
	highlighter.d\
	ctags.d\
	astprinter.d\
	formatter.d\
	outliner.d\
	std/*.d\
	std/d/*.d\
	analysis/*.d\
	-version=DIP61\
	-oftest\
	-g -unittest

	./test

