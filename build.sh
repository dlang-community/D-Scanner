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
	-ofdscanner\
	-g\
	-O -release -inline

#gdc\
#	main.d\
#	stats.d\
#	imports.d\
#	highlighter.d\
#	ctags.d\
#	astprinter.d\
#	formatter.d\
#	outliner.d\
#	std/*.d\
#	std/d/*.d\
#	analysis/*.d\
#	-O3 -frelease -fno-bounds-check\
#	-odscanner\

#ldc2\
#	main.d\
#	stats.d\
#	imports.d\
#	highlighter.d\
#	ctags.d\
#	astprinter.d\
#	formatter.d\
#	outliner.d\
#	std/*.d\
#	std/d/*.d\
#	analysis/*.d\
#	-O3 -release\
#	-oq -of=dscanner\
