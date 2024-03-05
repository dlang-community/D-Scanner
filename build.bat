@echo off
setlocal enabledelayedexpansion

if "%DC%"=="" set DC="dmd"
if "%DC%"=="ldc2" set DC="ldmd2"
if "%MFLAGS%"=="" set MFLAGS="-m32"

:: git might not be installed, so we provide 0.0.0 as a fallback or use
:: the existing githash file if existent
if not exist "bin" mkdir bin
git describe --tags > bin\githash_.txt
for /f %%i in ("bin\githash_.txt") do set githashsize=%%~zi
if %githashsize% == 0 (
	if not exist "bin\githash.txt" (
		echo v0.0.0 > bin\githash.txt
	)
) else (
	move /y bin\githash_.txt bin\githash.txt
)

set DFLAGS=-O -release -version=StdLoggerDisableWarning -version=CallbackAPI -version=DMDLIB -version=MARS -version=NoBackend -version=NoMain -Jbin -Jdmd -Jdmd\compiler\src\dmd\res %MFLAGS%
set TESTFLAGS=-g -w -version=StdLoggerDisableWarning -version=CallbackAPI -version=DMDLIB -version=MARS -version=NoBackend -version=NoMain -Jbin -Jdmd -Jdmd\compiler\src\dmd\res
set CORE=
set LIBDPARSE=
set STD=
set ANALYSIS=
set INIFILED=
set DSYMBOL=
set CONTAINERS=
set LIBDDOC=

set DMD_FRONTEND_DENYLIST=^
	dmd\compiler\src\dmd\mars.d^
	dmd\compiler\src\dmd\dmsc.d^
	dmd\compiler\src\dmd\e2ir.d^
	dmd\compiler\src\dmd\eh.d^
	dmd\compiler\src\dmd\glue.d^
	dmd\compiler\src\dmd\iasmdmd.d^
	dmd\compiler\src\dmd\irstate.d^
	dmd\compiler\src\dmd\lib.d^
	dmd\compiler\src\dmd\libelf.d^
	dmd\compiler\src\dmd\libmach.d^
	dmd\compiler\src\dmd\libmscoff.d^
	dmd\compiler\src\dmd\libomf.d^
	dmd\compiler\src\dmd\objc_glue.d^
	dmd\compiler\src\dmd\s2ir.d^
	dmd\compiler\src\dmd\scanelf.d^
	dmd\compiler\src\dmd\scanmach.d^
	dmd\compiler\src\dmd\scanmscoff.d^
	dmd\compiler\src\dmd\scanomf.d^
	dmd\compiler\src\dmd\tocsym.d^
	dmd\compiler\src\dmd\toctype.d^
	dmd\compiler\src\dmd\tocvdebug.d^
	dmd\compiler\src\dmd\toobj.d^
	dmd\compiler\src\dmd\todt.d^
	dmd\compiler\src\dmd\toir.d

set DMD_FRONTEND_SRC=
for %%x in (dmd\compiler\src\dmd\common\*.d) do set DMD_FRONTEND_SRC=!DMD_FRONTEND_SRC! %%x
for %%x in (dmd\compiler\src\dmd\root\*.d) do set DMD_FRONTEND_SRC=!DMD_FRONTEND_SRC! %%x
for %%x in (dmd\compiler\src\dmd\*.d) do (
    echo "%DMD_FRONTEND_DENYLIST%" | findstr /i /c:"%%x" >nul
    if errorlevel 1 (
        set "DMD_FRONTEND_SRC=!DMD_FRONTEND_SRC! %%x"
    )
)

set DMD_ROOT_SRC=
for %%x in (dmd\compiler\src\dmd\common\*.d) do set DMD_ROOT_SRC=!DMD_ROOT_SRC! %%x
for %%x in (dmd\compiler\src\dmd\root\*.d) do set DMD_ROOT_SRC=!DMD_ROOT_SRC! %%x

set DMD_LEXER_SRC=^
	dmd\compiler\src\dmd\console.d ^
	dmd\compiler\src\dmd\entity.d ^
	dmd\compiler\src\dmd\errors.d ^
	dmd\compiler\src\dmd\file_manager.d ^
	dmd\compiler\src\dmd\globals.d ^
	dmd\compiler\src\dmd\id.d ^
	dmd\compiler\src\dmd\identifier.d ^
	dmd\compiler\src\dmd\lexer.d ^
	dmd\compiler\src\dmd\tokens.d ^
	dmd\compiler\src\dmd\utils.d

set DMD_PARSER_SRC=^
	dmd\compiler\src\dmd\astbase.d ^
	dmd\compiler\src\dmd\parse.d ^
	dmd\compiler\src\dmd\parsetimevisitor.d ^
	dmd\compiler\src\dmd\transitivevisitor.d ^
	dmd\compiler\src\dmd\permissivevisitor.d ^
	dmd\compiler\src\dmd\strictvisitor.d ^
	dmd\compiler\src\dmd\astenums.d

for %%x in (src\dscanner\*.d) do set CORE=!CORE! %%x
for %%x in (src\dscanner\analysis\*.d) do set ANALYSIS=!ANALYSIS! %%x
for %%x in (libdparse\src\dparse\*.d) do set LIBDPARSE=!LIBDPARSE! %%x
for %%x in (libdparse\src\std\experimental\*.d) do set LIBDPARSE=!LIBDPARSE! %%x
for %%x in (libddoc\src\ddoc\*.d) do set LIBDDOC=!LIBDDOC! %%x
for %%x in (libddoc\common\source\ddoc\*.d) do set LIBDDOC=!LIBDDOC! %%x
for %%x in (inifiled\source\*.d) do set INIFILED=!INIFILED! %%x
for %%x in (DCD\dsymbol\src\dsymbol\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (DCD\dsymbol\src\dsymbol\builtin\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (DCD\dsymbol\src\dsymbol\conversion\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (containers\src\containers\*.d) do set CONTAINERS=!CONTAINERS! %%x
for %%x in (containers\src\containers\internal\*.d) do set CONTAINERS=!CONTAINERS! %%x

if "%1" == "test" goto test_cmd

@echo on
%DC% %MFLAGS%^
	%CORE%^
	%STD%^
	%LIBDPARSE%^
	%LIBDDOC%^
	%ANALYSIS%^
	%INIFILED%^
	%DSYMBOL%^
	%CONTAINERS%^
	%DMD_FRONTEND_SRC%^
	%DFLAGS%^
	-Ilibdparse\src^
	-IDCD\dsymbol\src^
	-Icontainers\src^
	-Ilibddoc\src^
	-Ilibddoc\common\source^
	-Idmd\compiler\src^
	-ofbin\dscanner.exe
goto eof

:test_cmd
@echo on
set TESTNAME="bin\dscanner-unittest"
%DC% %MFLAGS% ^
	%STD%^
	%LIBDPARSE%^
	%LIBDDOC%^
	%INIFILED%^
	%DSYMBOL%^
	%CONTAINERS%^
	%DMD_FRONTEND_SRC%^
	-I"libdparse\src"^
	-I"DCD\dsymbol\src"^
	-I"containers\src"^
	-I"libddoc\src"^
	-I"dmd\compiler\src"^
	-I"dmd\compiler\src\dmd\res"^
	%TESTFLAGS%^
	-lib^
	-of%TESTNAME%.lib
if exist %TESTNAME%.lib %DC% %MFLAGS%^
	%CORE%^
	%ANALYSIS%^
	%TESTNAME%.lib^
	-I"src"^
	-I"inifiled\source"^
	-I"libdparse\src"^
	-I"DCD\dsymbol\src"^
	-I"containers\src"^
	-I"libddoc\src"^
	-I"libddoc\common\source"^
	-I"dmd\compiler\src"^
	-I"dmd\compiler\src\dmd\res"^
	-unittest^
	%TESTFLAGS%^
	-of%TESTNAME%.exe
if exist %TESTNAME%.exe %TESTNAME%.exe

if exist %TESTNAME%.obj del %TESTNAME%.obj
if exist %TESTNAME%.lib del %TESTNAME%.lib
if exist %TESTNAME%.exe del %TESTNAME%.exe

:eof
