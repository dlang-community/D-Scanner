.PHONY: all test clean

DC ?= dmd
GIT ?= git
DMD := $(DC)
GDC := gdc
LDC := ldc2

LIB_SRC := \
	$(shell find containers/src -name "*.d")\
	$(shell find dsymbol/src -name "*.d")\
	$(shell find inifiled/source/ -name "*.d")\
	$(shell find libdparse/src/std/experimental/ -name "*.d")\
	$(shell find libdparse/src/dparse/ -name "*.d")\
	$(shell find libddoc/src -name "*.d") \
	$(shell find libddoc/common/source -name "*.d") \
	$(shell find stdx-allocator/source -name "*.d")
PROJECT_SRC := $(shell find src/ -name "*.d")
SRC := $(LIB_SRC) $(PROJECT_SRC)

UT_OBJ_DIR = unittest-obj
OBJ_DIR := obj
OBJ = $(SRC:.d=.o)
PROJECT_OBJ = $(PROJECT_SRC:.d=.o)
LIB_OBJ = $(LIB_SRC:.d=.o)

INCLUDE_PATHS = \
	-Isrc \
	-Iinifiled/source \
	-Ilibdparse/src \
	-Idsymbol/src \
	-Icontainers/src \
	-Ilibddoc/src \
	-Ilibddoc/common/source \
	-Istdx-allocator/source
VERSIONS =
DEBUG_VERSIONS = -version=dparse_verbose
DMD_FLAGS = -w -release -O -Jbin -od${OBJ_DIR} -version=StdLoggerDisableWarning
override DMD_FLAGS += $(DFLAGS)
override LDC_FLAGS += $(DFLAGS)
override GDC_FLAGS += $(DFLAGS)
DMD_TEST_FLAGS = -w -g -Jbin -version=StdLoggerDisableWarning
override LDC_FLAGS += -O5 -release -oq -d-version=StdLoggerDisableWarning -Jbin
override GDC_FLAGS += -O3 -frelease -d-version=StdLoggerDisableWarning -Jbin
SHELL:=/usr/bin/env bash

DMD_BIN = bin/dmd/dscanner
LDC_BIN = bin/ldc/dscanner
GDC_BIN = bin/gdc/dscanner

GITHASH = bin/githash.txt

OBJ_BY_DMD = $(addprefix $(OBJ_DIR)/dmd/, $(OBJ))
UT_OBJ_BY_DMD = $(addprefix $(UT_OBJ_DIR)/dmd/, $(PROJECT_OBJ))

OBJ_BY_LDC = $(addprefix $(OBJ_DIR)/ldc/, $(OBJ))
UT_OBJ_BY_LDC = $(addprefix $(UT_OBJ_DIR)/ldc/, $(PROJECT_OBJ))

OBJ_BY_GDC = $(addprefix $(OBJ_DIR)/gdc/, $(OBJ))
UT_OBJ_BY_GDC = $(addprefix $(UT_OBJ_DIR)/gdc/, $(PROJECT_OBJ))

$(OBJ_DIR)/dmd/%.o: %.d
	@test -d $(dir $@) || mkdir -p $(dir $@)
	${DC} ${DMD_FLAGS} ${VERSIONS} ${INCLUDE_PATHS} -c $< -of=$@

$(UT_OBJ_DIR)/dmd/%.o: %.d
	@test -d $(dir $@) || mkdir -p $(dir $@)
	${DC} ${DMD_TEST_FLAGS} ${VERSIONS} -unittest ${INCLUDE_PATHS} -c $< -of=$@

$(OBJ_DIR)/ldc/%.o: %.d
	@test -d $(dir $@) || mkdir -p $(dir $@)
	${DC} ${LDC_FLAGS} ${VERSIONS} ${INCLUDE_PATHS} -c $< -of=$@

$(UT_OBJ_DIR)/ldc/%.o: %.d
	@test -d $(dir $@) || mkdir -p $(dir $@)
	${DC} ${LDC_TEST_FLAGS} ${VERSIONS} -unittest ${INCLUDE_PATHS} -c $< -of=$@

$(OBJ_DIR)/gdc/%.o: %.d
	@test -d $(dir $@) || mkdir -p $(dir $@)
	${DC} ${GDC_FLAGS} ${VERSIONS} ${INCLUDE_PATHS} -c $< -o $@

$(UT_OBJ_DIR)/gdc/%.o: %.d
	@test -d $(dir $@) || mkdir -p $(dir $@)
	${DC} ${GDC_TEST_FLAGS} ${VERSIONS} -unittest ${INCLUDE_PATHS} -c $< -o $@

all: ${DMD_BIN}
ldc: ${LDC_BIN}
gdc: ${GDC_BIN}

${GITHASH}:
	mkdir -p bin && ${GIT} describe --tags --always > ${GITHASH}

debug: ${GITHASH}
	${DC} -w -g -Jbin -ofdsc ${VERSIONS} ${DEBUG_VERSIONS} ${INCLUDE_PATHS} ${SRC}

${DMD_BIN}: ${GITHASH} ${OBJ_BY_DMD}
	${DC} -of${DMD_BIN} ${OBJ_BY_DMD}

${GDC_BIN}: ${GITHASH} ${OBJ_BY_GDC}
	${GDC} -o${GDC_BIN} ${OBJ_BY_GDC}

${LDC_BIN}: ${GITHASH} ${OBJ_BY_LDC}
	${LDC} -of=${DMD_BIN} ${OBJ_BY_LDC}

# compile the dependencies separately, s.t. their unittests don't get executed
bin/dmd/dscanner-unittest-lib.a: ${LIB_SRC}
	${DC} ${DMD_TEST_FLAGS} -c ${VERSIONS} ${INCLUDE_PATHS} ${LIB_SRC} -of$@

test: bin/dmd/dscanner-unittest-lib.a ${GITHASH} ${UT_OBJ_BY_DMD}
	${DC} bin/dmd/dscanner-unittest-lib.a ${UT_OBJ_BY_DMD} -ofbin/dmd/dscanner-unittest
	./bin/dmd/dscanner-unittest

lint: ${DMD_BIN}
	./${DMD_BIN} --config .dscanner.ini --styleCheck src

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
