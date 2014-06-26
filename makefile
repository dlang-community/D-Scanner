.PHONY: all test

DMD = /home/brian/src/dmd/src/dmd
GDC = gdc
LDC = ldc
SRC = main.d\
	stats.d\
	imports.d\
	highlighter.d\
	ctags.d\
	astprinter.d\
	outliner.d\
	libdparse/src/std/*.d\
	libdparse/src/std/d/*.d\
	analysis/*.d
INCLUDE_PATHS = -Ilibdparse/src
VERSIONS = -version=DIP61

all: dmdbuild

dmdbuild:
	${DMD} -O -release -inline -ofdscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC}

gdcbuild:
	${GDC} -O3 -frelease -odscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC}

ldcbuild:
	${LDC} -O5 -release -oq -of=dscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC}

test:
	@./test.sh

clean:
	rm -f dscanner *.o

