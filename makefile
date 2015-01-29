.PHONY: all test

DMD = dmd
GDC = gdc
LDC = ldc2
SRC = src/*.d\
	src/analysis/*.d\
	libdparse/src/std/*.d\
	libdparse/src/std/d/*.d\
	inifiled/source/*.d
INCLUDE_PATHS = -Ilibdparse/src
VERSIONS =
DEBUG_VERSIONS = -version=std_parser_verbose

all: dmdbuild
ldc: ldcbuild
gdc: gdcbuild

githash:
	git log -1 --format="%H" > githash.txt

debug:
	${DMD} -w -g -ofdsc ${VERSIONS} ${DEBUG_VERSIONS} ${INCLUDE_PATHS} ${SRC}

dmdbuild: githash
	mkdir -p bin
	${DMD} -w -O -release -inline -ofbin/dscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC} -J.
	rm -f bin/dscanner.o

gdcbuild: githash
	mkdir -p bin
	${GDC} -O3 -frelease -obin/dscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC} -J.

ldcbuild: githash
	mkdir -p bin
	${LDC} -O5 -release -oq -of=bin/dscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC} -J.

test:
	@./test.sh

clean:
	rm -rf dsc *.o
	rm -rf bin
	rm -f dscanner-report.json

report: all
	dscanner --report src > src/dscanner-report.json
	sonar-runner
