dmd\
	main.d\
	stats.d\
	imports.d\
	highlighter.d\
	ctags.d\
	astprinter.d\
	formatter.d\
	outliner.d\
	style.d\
	stdx/*.d\
	stdx/d/*.d\
	-ofdscanner\
	-m64\
	-O -release -noboundscheck

#gdc\
#	main.d\
#	stats.d\
#	imports.d\
#	highlighter.d\
#	ctags.d\
#	astprinter.d\
#	formatter.d\
#	outliner.d\
#	style.d\
#	stdx/*.d\
#	stdx/d/*.d\
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
#	style.d\
#	stdx/*.d\
#	stdx/d/*.d\
#	-O3 -release\
#	-oq -of=dscanner\
