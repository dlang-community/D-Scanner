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
	-ofdscanner\
	-m64 -g\
	-O -release

#gdc\
#	main.d\
#	stats.d\
#	imports.d\
#	highlighter.d\
#	ctags.d\
#	astprinter.d\
#	formatter.d\
#	outliner.d\
#	stdx/*.d\
#	stdx/d/*.d\
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
#	stdx/*.d\
#	stdx/d/*.d\
#	analysis/*.d\
#	-O3 -release\
#	-oq -of=dscanner\
