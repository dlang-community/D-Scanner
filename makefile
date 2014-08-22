.PHONY: all test

DMD = dmd
GDC = gdc
LDC = ldc
SRC = src/main.d\
	src/stats.d\
	src/imports.d\
	src/highlighter.d\
	src/ctags.d\
	src/astprinter.d\
	src/outliner.d\
	src/symbol_finder.d\
	src/analysis/*.d\
	libdparse/src/std/*.d\
	libdparse/src/std/d/*.d\
	inifiled/source/*.d
INCLUDE_PATHS = -Ilibdparse/src
VERSIONS =
DEBUG_VERSIONS = -version=std_parser_verbose

all: dmdbuild

debug:
	${DMD} -ofdsc ${VERSIONS} ${DEBUG_VERSIONS} ${INCLUDE_PATHS} ${SRC}

dmdbuild:
	mkdir -p bin
	${DMD} -O -release -inline -ofdscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC}

gdcbuild:
	${GDC} -O3 -frelease -odscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC}

ldcbuild:
	${LDC} -O5 -release -oq -of=dscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC}

test:
	@./test.sh

clean:
	rm -rf bin

report: all
	dscanner --report src > dscanner-report.json
	sonar-runner
