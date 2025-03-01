.PHONY: all test clean

.DEFAULT_GOAL := all

DC ?= dmd
GIT ?= git
DMD := $(DC)
GDC := gdc
LDC := ldc2

DMD_FRONTEND_SRC := \
	$(shell find dmd/compiler/src/dmd/common -name "*.d")\
	$(shell find dmd/compiler/src/dmd/root -name "*.d")\
	$(shell find dmd/compiler/src/dmd/visitor -name "*.d")\
	$(shell find dmd/compiler/src/dmd/mangle -name "*.d")\
	$(shell find dmd/compiler/src/dmd -maxdepth 1 -name "*.d" \
		! -name "mars.d" \
		! -name "dmsc.d" \
		! -name "e2ir.d" \
		! -name "eh.d" \
		! -name "glue.d" \
		! -name "iasmdmd.d" \
		! -name "irstate.d" \
		! -name "lib.d" \
		! -name "libelf.d" \
		! -name "libmach.d" \
		! -name "libmscoff.d" \
		! -name "libomf.d" \
		! -name "objc_glue.d" \
		! -name "s2ir.d" \
		! -name "scanelf.d" \
		! -name "scanmach.d" \
		! -name "scanmscoff.d" \
		! -name "scanomf.d" \
		! -name "tocsym.d" \
		! -name "toctype.d" \
		! -name "tocvdebug.d" \
		! -name "toobj.d" \
		! -name "todt.d" \
		! -name "toir.d" \
	)

LIB_SRC := \
	$(shell find containers/src -name "*.d")\
	$(shell find DCD/dsymbol/src -name "*.d")\
	$(shell find inifiled/source/ -name "*.d")\
	$(shell find libdparse/src/std/experimental/ -name "*.d")\
	$(shell find libdparse/src/dparse/ -name "*.d")\
	$(shell find libddoc/src -name "*.d") \
	$(shell find libddoc/common/source -name "*.d") \
	$(DMD_FRONTEND_SRC)

PROJECT_SRC := $(shell find src/ -name "*.d")
SRC := $(LIB_SRC) $(PROJECT_SRC)

OBJ_DIR := obj
UT_OBJ_DIR = $(OBJ_DIR)/unittest
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
UT_DSCANNER_LIB = bin/$(DC)/dscanner-unittest-lib.a
UT_DSCANNER_LIB_DIR = $(dir $(UT_DSCANNER_LIB))

INCLUDE_PATHS = \
	-Isrc \
	-Iinifiled/source \
	-Ilibdparse/src \
	-IDCD/dsymbol/src \
	-Icontainers/src \
	-Ilibddoc/src \
	-Ilibddoc/common/source \
	-Idmd/compiler/src

DMD_VERSIONS = -version=StdLoggerDisableWarning -version=CallbackAPI -version=DMDLIB -version=MARS -version=NoBackend -version=NoMain
DMD_DEBUG_VERSIONS = -version=dparse_verbose
LDC_VERSIONS = -d-version=StdLoggerDisableWarning -d-version=CallbackAPI -d-version=DMDLIB -d-version=MARS -d-version=NoBackend -d-version=NoMain
LDC_DEBUG_VERSIONS = -d-version=dparse_verbose
GDC_VERSIONS = -fversion=StdLoggerDisableWarning -fversion=CallbackAPI -fversion=DMDLIB -fversion=MARS -fversion=NoBackend -fversion=NoMain
GDC_DEBUG_VERSIONS = -fversion=dparse_verbose

DC_FLAGS += -Jbin -Jdmd -Jdmd/compiler/src/dmd/res
override DMD_FLAGS += $(DFLAGS) -w -release -O -od${OBJ_DIR}
override LDC_FLAGS += $(DFLAGS) -O5 -release -oq
override GDC_FLAGS += $(DFLAGS) -O3 -frelease -fall-instantiations

override GDC_TEST_FLAGS += -fall-instantiations

DC_TEST_FLAGS += -g -Jbin -Jdmd -Jdmd/compiler/src/dmd/res
override DMD_TEST_FLAGS += -w

DC_DEBUG_FLAGS := -g -Jbin -Jdmd -Jdmd/compiler/src/dmd/res

ifeq ($(DC), $(filter $(DC), dmd ldmd2 gdmd))
	VERSIONS := $(DMD_VERSIONS)
	DEBUG_VERSIONS := $(DMD_DEBUG_VERSIONS)
	DC_FLAGS += $(DMD_FLAGS)
	DC_TEST_FLAGS += $(DMD_TEST_FLAGS) -unittest
	DC_DEBUG_FLAGS += -O
	WRITE_TO_TARGET_NAME = -of$@
else ifneq (,$(findstring ldc2, $(DC)))
	VERSIONS := $(LDC_VERSIONS)
	DEBUG_VERSIONS := $(LDC_DEBUG_VERSIONS)
	DC_FLAGS += $(LDC_FLAGS)
	DC_TEST_FLAGS += $(LDC_TEST_FLAGS) -unittest
	DC_DEBUG_FLAGS += -O
	WRITE_TO_TARGET_NAME = -of=$@
else ifneq (,$(findstring gdc, $(DC)))
	VERSIONS := $(GDC_VERSIONS)
	DEBUG_VERSIONS := $(GDC_DEBUG_VERSIONS)
	DC_FLAGS += $(GDC_FLAGS)
	DC_TEST_FLAGS += $(GDC_TEST_FLAGS) -funittest
	DC_DEBUG_FLAGS += -O3 -fall-instantiations
	WRITE_TO_TARGET_NAME = -o $@
endif
SHELL:=/usr/bin/env bash

GITHASH = bin/githash.txt

FIRST_RUN_FLAG := bin/first_run.flag

ifneq (, $(findstring $(GDC), $(DC)))
		CONFIG_CMD := $(DC) dmd/config.d -o config && ./config bin VERSION /etc && rm config;
	else
		CONFIG_CMD := $(DC) -run dmd/config.d bin VERSION /etc;
	endif

$(FIRST_RUN_FLAG):
	if [ ! -f $(FIRST_RUN_FLAG) ]; then \
		$(CONFIG_CMD) \
		touch $(FIRST_RUN_FLAG); \
	fi

$(OBJ_DIR)/$(DC)/%.o: %.d | ${FIRST_RUN_FLAG}
	${DC} ${DC_FLAGS} ${VERSIONS} ${INCLUDE_PATHS} -c $< ${WRITE_TO_TARGET_NAME}

$(UT_OBJ_DIR)/$(DC)/%.o: %.d | ${FIRST_RUN_FLAG}
	${DC} ${DC_TEST_FLAGS} ${VERSIONS} ${INCLUDE_PATHS} -c $< ${WRITE_TO_TARGET_NAME}

${DSCANNER_BIN}: ${GITHASH} ${OBJ_BY_DC} | ${DSCANNER_BIN_DIR}
	${DC} ${OBJ_BY_DC} ${WRITE_TO_TARGET_NAME}

${OBJ_BY_DC}: | ${OBJ_BY_DC_DIR}

${OBJ_BY_DC_DIR}:
	mkdir -p $@

${UT_OBJ_BY_DC}: | ${UT_OBJ_BY_DC_DIR}

${UT_OBJ_BY_DC_DIR}:
	mkdir -p $@

${DSCANNER_BIN_DIR}:
	mkdir -p $@

${UT_DSCANNER_LIB_DIR}:
	mkdir -p $@

all: ${DSCANNER_BIN}
ldc: ${DSCANNER_BIN}
gdc: ${DSCANNER_BIN}

githash: ${GITHASH}

${GITHASH}:
	mkdir -p bin && ${GIT} describe --tags --always > ${GITHASH}

debug: ${GITHASH}
	${DC} -w -g -Jbin -ofdsc ${VERSIONS} ${DEBUG_VERSIONS} ${INCLUDE_PATHS} ${SRC}

# compile the dependencies separately, s.t. their unittests don't get executed
${UT_DSCANNER_LIB}: ${LIB_SRC} | ${UT_DSCANNER_LIB_DIR}
	${DC} ${DC_DEBUG_FLAGS} -c ${VERSIONS} ${INCLUDE_PATHS} ${LIB_SRC} ${WRITE_TO_TARGET_NAME}

test: ${UT_DSCANNER_BIN}

${UT_DSCANNER_BIN}: ${GITHASH} ${UT_OBJ_BY_DC} ${UT_DSCANNER_LIB} | ${DSCANNER_BIN_DIR}
	${DC} ${UT_DSCANNER_LIB} ${UT_OBJ_BY_DC} ${WRITE_TO_TARGET_NAME}
	./${UT_DSCANNER_BIN}

lint: ${DSCANNER_BIN}
	./${DSCANNER_BIN} --styleCheck src

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

# Add source files here as we transition to DMD-as-a-library
STYLE_CHECKED_SRC := \
	src/dscanner/imports.d \
	src/dscanner/main.d

style:
	@echo "Check for trailing whitespace"
	grep -nr '[[:blank:]]$$' ${STYLE_CHECKED_SRC}; test $$? -eq 1

	@echo "Enforce whitespace before opening parenthesis"
	grep -nrE "\<(for|foreach|foreach_reverse|if|while|switch|catch|version)\(" ${STYLE_CHECKED_SRC} ; test $$? -eq 1

	@echo "Enforce no whitespace after opening parenthesis"
	grep -nrE "\<(version) \( " ${STYLE_CHECKED_SRC} ; test $$? -eq 1

	@echo "Enforce whitespace between colon(:) for import statements (doesn't catch everything)"
	grep -nr 'import [^/,=]*:.*;' ${STYLE_CHECKED_SRC} | grep -vE "import ([^ ]+) :\s"; test $$? -eq 1

	@echo "Check for package wide std.algorithm imports"
	grep -nr 'import std.algorithm : ' ${STYLE_CHECKED_SRC} ; test $$? -eq 1

	@echo "Enforce Allman style"
	grep -nrE '(if|for|foreach|foreach_reverse|while|unittest|switch|else|version) .*{$$' ${STYLE_CHECKED_SRC}; test $$? -eq 1

	@echo "Enforce do { to be in Allman style"
	grep -nr 'do *{$$' ${STYLE_CHECKED_SRC} ; test $$? -eq 1

	@echo "Enforce no space between assert and the opening brace, i.e. assert("
	grep -nrE 'assert +\(' ${STYLE_CHECKED_SRC} ; test $$? -eq 1

	@echo "Enforce space after cast(...)"
	grep -nrE '[^"]cast\([^)]*?\)[[:alnum:]]' ${STYLE_CHECKED_SRC} ; test $$? -eq 1

	@echo "Enforce space between a .. b"
	grep -nrE '[[:alnum:]][.][.][[:alnum:]]|[[:alnum:]] [.][.][[:alnum:]]|[[:alnum:]][.][.] [[:alnum:]]' ${STYLE_CHECKED_SRC}; test $$? -eq 1

	@echo "Enforce space between binary operators"
	grep -nrE "[[:alnum:]](==|!=|<=|<<|>>|>>>|^^)[[:alnum:]]|[[:alnum:]] (==|!=|<=|<<|>>|>>>|^^)[[:alnum:]]|[[:alnum:]](==|!=|<=|<<|>>|>>>|^^) [[:alnum:]]" ${STYLE_CHECKED_SRC}; test $$? -eq 1
