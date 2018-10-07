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

set DFLAGS=-O -release -inline -version=StdLoggerDisableWarning -Jbin
set TESTFLAGS=-g -w -version=StdLoggerDisableWarning
set CORE=
set LIBDPARSE=
set STD=
set ANALYSIS=
set INIFILED=
set DSYMBOL=
set CONTAINERS=
set LIBDDOC=
set STDXALLOCATOR=
set STDXALLOCATORBLOCKS=

for %%x in (src\dscanner\*.d) do set CORE=!CORE! %%x
for %%x in (src\dscanner\analysis\*.d) do set ANALYSIS=!ANALYSIS! %%x
for %%x in (libdparse\src\dparse\*.d) do set LIBDPARSE=!LIBDPARSE! %%x
for %%x in (libdparse\src\std\experimental\*.d) do set LIBDPARSE=!LIBDPARSE! %%x
for %%x in (libddoc\src\ddoc\*.d) do set LIBDDOC=!LIBDDOC! %%x
for %%x in (inifiled\source\*.d) do set INIFILED=!INIFILED! %%x
for %%x in (dsymbol\src\dsymbol\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (dsymbol\src\dsymbol\builtin\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (dsymbol\src\dsymbol\conversion\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (containers\src\containers\*.d) do set CONTAINERS=!CONTAINERS! %%x
for %%x in (containers\src\containers\internal\*.d) do set CONTAINERS=!CONTAINERS! %%x
for %%x in (stdx-allocator\source\stdx\allocator\*.d) do set STDXALLOCATOR=!STDXALLOCATOR! %%x
for %%x in (stdx-allocator\source\stdx\allocator\building_blocks\*.d) do set STDXALLOCATORBLOCKS=!STDXALLOCATORBLOCKS! %%x

if "%1" == "test" goto test_cmd

@echo on
%DC% %MFLAGS% %CORE% %STD% %LIBDPARSE% %LIBDDOC% %ANALYSIS% %INIFILED% %DSYMBOL% %CONTAINERS% %STDXALLOCATOR% %STDXALLOCATORBLOCKS% %DFLAGS% -I"libdparse\src" -I"dsymbol\src" -I"containers\src" -I"libddoc\src" -I"stdx-allocator\source" -ofbin\dscanner.exe
goto eof

:test_cmd
@echo on
set TESTNAME="bin\dscanner-unittest"
%DC% %MFLAGS% %STD% %LIBDPARSE% %LIBDDOC% %INIFILED% %DSYMBOL% %CONTAINERS% %STDXALLOCATOR% %STDXALLOCATORBLOCKS% -I"libdparse\src" -I"dsymbol\src" -I"containers\src" -I"libddoc\src" -I"stdx-allocator\source" -lib %TESTFLAGS% -of%TESTNAME%.lib
if exist %TESTNAME%.lib %DC% %MFLAGS% %CORE% %ANALYSIS% %TESTNAME%.lib -I"src" -I"inifiled\source" -I"libdparse\src" -I"dsymbol\src" -I"containers\src" -I"libddoc\src" -I"stdx-allocator\source" -unittest %TESTFLAGS% -of%TESTNAME%.exe
if exist %TESTNAME%.exe %TESTNAME%.exe

if exist %TESTNAME%.obj del %TESTNAME%.obj
if exist %TESTNAME%.lib del %TESTNAME%.lib
if exist %TESTNAME%.exe del %TESTNAME%.exe

:eof
