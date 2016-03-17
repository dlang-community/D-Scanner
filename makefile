.PHONY: all test

DC ?= dmd
DMD := $(DC)
GDC := gdc
LDC := ldc2
OBJ_DIR := obj
SRC := \
	$(shell find containers/experimental_allocator/src -name "*.d")\
	$(shell find containers/src -name "*.d")\
	$(shell find dsymbol/src -name "*.d")\
	$(shell find inifiled/source/ -name "*.d")\
	$(shell find libdparse/src/std/experimental/ -name "*.d")\
	$(shell find libdparse/src/dparse/ -name "*.d")\
	$(shell find src/ -name "*.d")
INCLUDE_PATHS = \
	-Iinifiled/source -Isrc\
	-Ilibdparse/src\
	-Ilibdparse/experimental_allocator/src\
	-Idsymbol/src -Icontainers/src
VERSIONS =
DEBUG_VERSIONS = -version=dparse_verbose
DMD_FLAGS = -w -inline -release -O -J. -od${OBJ_DIR} -version=StdLoggerDisableWarning
DMD_TEST_FLAGS = -w -g -unittest -J.

all: dmdbuild
ldc: ldcbuild
gdc: gdcbuild

githash:
	git log -1 --format="%H" > githash.txt

debug:
	${DC} -w -g -J. -ofdsc ${VERSIONS} ${DEBUG_VERSIONS} ${INCLUDE_PATHS} ${SRC}

dmdbuild: githash $(SRC)
	mkdir -p bin
	${DC} ${DMD_FLAGS} -ofbin/dscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC}
	rm -f bin/dscanner.o

gdcbuild: githash
	mkdir -p bin
	${GDC} -O3 -frelease -obin/dscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC} -J.

ldcbuild: githash
	mkdir -p bin
	${LDC} -O5 -release -oq -of=bin/dscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC} -J.

test: githash
	${DC} -w -g -J. -unittest ${INCLUDE_PATHS} ${SRC} -ofbin/dscanner-unittest -version=StdLoggerDisableWarning
	./bin/dscanner-unittest
	rm -f bin/dscanner-unittest

clean:
	rm -rf dsc
	rm -rf bin
	rm -rf ${OBJ_DIR}
	rm -f dscanner-report.json

report: all
	dscanner --report src > src/dscanner-report.json
	sonar-runner
