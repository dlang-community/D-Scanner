.PHONY: all test

DC ?= dmd
DMD := $(DC)
GDC := gdc
LDC := ldc2
OBJ_DIR := obj
LIB_SRC := \
	$(shell find containers/src -name "*.d")\
	$(shell find dsymbol/src -name "*.d")\
	$(shell find inifiled/source/ -name "*.d")\
	$(shell find libdparse/src/std/experimental/ -name "*.d")\
	$(shell find libdparse/src/dparse/ -name "*.d")\
	$(shell find libddoc/src -name "*.d") \
	$(shell find stdx-allocator/source -name "*.d")
PROJECT_SRC := $(shell find src/ -name "*.d")
SRC := $(LIB_SRC) $(PROJECT_SRC)
INCLUDE_PATHS = \
	-Isrc \
	-Iinifiled/source \
	-Ilibdparse/src \
	-Idsymbol/src \
	-Icontainers/src \
	-Ilibddoc/src \
	-Istdx-allocator/source
VERSIONS =
DEBUG_VERSIONS = -version=dparse_verbose
DMD_FLAGS = -w -inline -release -O -J. -od${OBJ_DIR} -version=StdLoggerDisableWarning
DMD_TEST_FLAGS = -w -g -J. -version=StdLoggerDisableWarning
override LDC_FLAGS += -O5 -release -oq
override GDC_FLAGS += -O3 -frelease
SHELL:=/bin/bash

all: dmdbuild
ldc: ldcbuild
gdc: gdcbuild

githash:
	git describe --tags > githash.txt

debug:
	${DC} -w -g -J. -ofdsc ${VERSIONS} ${DEBUG_VERSIONS} ${INCLUDE_PATHS} ${SRC}

dmdbuild: githash $(SRC)
	mkdir -p bin
	${DC} ${DMD_FLAGS} -ofbin/dscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC}
	rm -f bin/dscanner.o

gdcbuild: githash
	mkdir -p bin
	${GDC} ${GDC_FLAGS} -obin/dscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC} -J.

ldcbuild: githash
	mkdir -p bin
	${LDC} ${LDC_FLAGS} -of=bin/dscanner ${VERSIONS} ${INCLUDE_PATHS} ${SRC} -J.

# compile the dependencies separately, s.t. their unittests don't get executed
bin/dscanner-unittest-lib.a: ${LIB_SRC}
	${DC} ${DMD_TEST_FLAGS} -c ${INCLUDE_PATHS} ${LIB_SRC} -of$@

test: bin/dscanner-unittest-lib.a githash
	${DC} ${DMD_TEST_FLAGS} -unittest ${INCLUDE_PATHS} bin/dscanner-unittest-lib.a ${PROJECT_SRC} -ofbin/dscanner-unittest
	./bin/dscanner-unittest
	rm -f bin/dscanner-unittest

lint: dmdbuild
	./bin/dscanner --config .dscanner.ini --styleCheck src

clean:
	rm -rf dsc
	rm -rf bin
	rm -rf ${OBJ_DIR}
	rm -f dscanner-report.json

report: all
	dscanner --report src > src/dscanner-report.json
	sonar-runner

release:
	./release.sh
