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

OBJ_BY_DC = $(addprefix $(OBJ_DIR)/$(DC)/, $(OBJ))
# `sort` also removes duplicates, which is what we want
OBJ_BY_DC_DIR = $(sort $(dir $(OBJ_BY_DC)))
UT_OBJ_BY_DC = $(addprefix $(UT_OBJ_DIR)/$(DC)/, $(PROJECT_OBJ))
UT_OBJ_BY_DC_DIR = $(sort $(dir $(UT_OBJ_BY_DC)))

DSCANNER_BIN = bin/dscanner
DSCANNER_BIN_DIR = $(dir $(DSCANNER_BIN))
UT_DSCANNER_BIN = bin/dscanner-unittest
UT_DSCANNER_LIB = bin/dscanner-unittest-lib.a

INCLUDE_PATHS = \
	-Isrc \
	-Iinifiled/source \
	-Ilibdparse/src \
	-Idsymbol/src \
	-Icontainers/src \
	-Ilibddoc/src \
	-Ilibddoc/common/source \
	-Istdx-allocator/source

DMD_VERSIONS = -version=StdLoggerDisableWarning
DMD_DEBUG_VERSIONS = -version=dparse_verbose
LDC_VERSIONS = -d-version=StdLoggerDisableWarning
LDC_DEBUG_VERSIONS = -d-version=dparse_verbose
GDC_VERSIONS = -fversion=StdLoggerDisableWarning
GDC_DEBUG_VERSIONS = -fversion=dparse_verbose

DC_FLAGS += -Jbin
override DMD_FLAGS += $(DFLAGS) -w -release -O -od${OBJ_DIR}
override LDC_FLAGS += $(DFLAGS) -O5 -release -oq
override GDC_FLAGS += $(DFLAGS) -O3 -frelease

DC_TEST_FLAGS += -g -Jbin
override DMD_TEST_FLAGS += -w

ifeq ($(DC), $(filter $(DC), dmd ldmd2 gdmd))
	VERSIONS := $(DMD_VERSIONS)
	DEBUG_VERSIONS := $(DMD_DEBUG_VERSIONS)
	DC_FLAGS += $(DMD_FLAGS)
	DC_TEST_FLAGS += $(DMD_TEST_FLAGS)
	WRITE_TO_TARGET_NAME = -of=$@
else ifeq ($(DC), ldc2)
	VERSIONS := $(LDC_VERSIONS)
	DEBUG_VERSIONS := $(LDC_DEBUG_VERSIONS)
	DC_FLAGS += $(LDC_FLAGS)
	DC_TEST_FLAGS += $(LDC_TEST_FLAGS)
	WRITE_TO_TARGET_NAME = -of=$@
else ifeq ($(DC), gdc)
	VERSIONS := $(GDC_VERSIONS)
	DEBUG_VERSIONS := $(GDC_DEBUG_VERSIONS)
	DC_FLAGS += $(GDC_FLAGS)
	DC_TEST_FLAGS += $(GDC_TEST_FLAGS)
	WRITE_TO_TARGET_NAME = -o $@
endif

SHELL:=/usr/bin/env bash

GITHASH = bin/githash.txt


$(OBJ_DIR)/$(DC)/%.o: %.d
	${DC} ${DC_FLAGS} ${VERSIONS} ${INCLUDE_PATHS} -c $< ${WRITE_TO_TARGET_NAME}

$(UT_OBJ_DIR)/$(DC)/%.o: %.d
	${DC} ${DC_TEST_FLAGS} ${VERSIONS} -unittest ${INCLUDE_PATHS} -c $< ${WRITE_TO_TARGET_NAME}

${DSCANNER_BIN}: ${GITHASH} ${OBJ_BY_DC} | ${DSCANNER_BIN_DIR}
	${DC} ${OBJ_BY_DC} ${WRITE_TO_TARGET_NAME}

${OBJ_BY_DC}: | ${OBJ_BY_DC_DIR}

${OBJ_BY_DC_DIR}:
	mkdir -p ${OBJ_BY_DC_DIR}

${DSCANNER_BIN_DIR}:
	mkdir -p $@

all: ${DSCANNER_BIN}
ldc: ${DSCANNER_BIN}
gdc: ${DSCANNER_BIN}

${GITHASH}:
	mkdir -p bin && ${GIT} describe --tags --always > ${GITHASH}

debug: ${GITHASH}
	${DC} -w -g -Jbin -ofdsc ${VERSIONS} ${DEBUG_VERSIONS} ${INCLUDE_PATHS} ${SRC}

# compile the dependencies separately, s.t. their unittests don't get executed
${UT_DSCANNER_LIB}: ${LIB_SRC} | ${DSCANNER_BIN_DIR}
	${DC} ${DC_TEST_FLAGS} -c ${VERSIONS} ${INCLUDE_PATHS} ${LIB_SRC} ${WRITE_TO_TARGET_NAME}

test: ${UT_DSCANNER_BIN}

${UT_DSCANNER_BIN}: ${UT_DSCANNER_LIB} ${GITHASH} ${UT_OBJ_BY_DC} | ${DSCANNER_BIN_DIR}
	${DC} ${UT_DSCANNER_LIB} ${UT_OBJ_BY_DC} ${WRITE_TO_TARGET_NAME}
	./${UT_DSCANNER_BIN}

lint: ${DSCANNER_BIN}
	./${DSCANNER_BIN} --config .dscanner.ini --styleCheck src

clean:
	rm -rf dsc
	rm -rf bin
	rm -rf ${OBJ_DIR}
	rm -rf ${UT_OBJ_DIR}
	rm -f dscanner-report.json

report: all
	dscanner --report src > src/dscanner-report.json
	sonar-runner

release:
	./release.sh
