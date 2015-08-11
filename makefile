.PHONY: all test

DMD = dmd
GDC = gdc
LDC = ldc2
experimental_allocator_D = libdparse/experimental_allocator/src/
allocator_D = ${experimental_allocator_D}/std/experimental/allocator/
SRC = src/*.d\
	src/analysis/*.d\
	libdparse/src/std/*.d\
	libdparse/src/std/d/*.d\
	inifiled/source/*.d\
	${allocator_D}/*.d\
	${allocator_D}/building_blocks/*.d
INCLUDE_PATHS = -Ilibdparse/src -I${experimental_allocator_D}
VERSIONS =
DEBUG_VERSIONS = -version=std_parser_verbose
DMD_FLAGS = -w -O -release -inline
#DMD_FLAGS = -w

all: dmdbuild
ldc: ldcbuild
gdc: gdcbuild

githash:
	git log -1 --format="%H" > githash.txt

debug:
	${DMD} -w -g -ofdsc ${VERSIONS} ${DEBUG_VERSIONS} ${INCLUDE_PATHS} ${SRC} -J.

dmdbuild: githash
	mkdir -p bin
	${DMD} ${DMD_FLAGS} -ofbin/dscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC} -J.
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
