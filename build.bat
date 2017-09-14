@echo off
setlocal enabledelayedexpansion

set DFLAGS=-O -release -inline -version=StdLoggerDisableWarning
set TESTFLAGS=-g -w -version=StdLoggerDisableWarning
set CORE=
set LIBDPARSE=
set STD=
set ANALYSIS=
set INIFILED=
set DSYMBOL=
set CONTAINERS=
set LIBDDOC=

for %%x in (src\*.d) do set CORE=!CORE! %%x
for %%x in (src\analysis\*.d) do set ANALYSIS=!ANALYSIS! %%x
for %%x in (libdparse\src\dparse\*.d) do set LIBDPARSE=!LIBDPARSE! %%x
for %%x in (libdparse\src\std\experimental\*.d) do set LIBDPARSE=!LIBDPARSE! %%x
for %%x in (libddoc\src\ddoc\*.d) do set LIBDDOC=!LIBDDOC! %%x
for %%x in (inifiled\source\*.d) do set INIFILED=!INIFILED! %%x
for %%x in (dsymbol\src\dsymbol\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (dsymbol\src\dsymbol\builtin\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (dsymbol\src\dsymbol\conversion\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (containers\src\containers\*.d) do set CONTAINERS=!CONTAINERS! %%x
for %%x in (containers\src\containers\internal\*.d) do set CONTAINERS=!CONTAINERS! %%x

if "%1" == "test" goto test_cmd

@echo on
dmd %CORE% %STD% %LIBDPARSE% %LIBDDOC% %ANALYSIS% %INIFILED% %DSYMBOL% %CONTAINERS% %DFLAGS% -I"libdparse\src" -I"dsymbol\src" -I"containers\src" -I"libddoc\src" -ofbin\dscanner.exe
goto eof

:test_cmd
@echo on
set TESTNAME="bin\dscanner-unittest"
dmd %STD% %LIBDPARSE% %LIBDDOC% %INIFILED% %DSYMBOL% %CONTAINERS% -I"libdparse\src" -I"dsymbol\src" -I"containers\src" -I"libddoc\src" -lib %TESTFLAGS% -of%TESTNAME%.lib
if exist %TESTNAME%.lib dmd %CORE% %ANALYSIS% %TESTNAME%.lib -I"src" -I"inifiled\source" -I"libdparse\src" -I"dsymbol\src" -I"containers\src" -I"libddoc\src" -unittest %TESTFLAGS% -of%TESTNAME%.exe
if exist %TESTNAME%.exe %TESTNAME%.exe

if exist %TESTNAME%.obj del %TESTNAME%.obj
if exist %TESTNAME%.lib del %TESTNAME%.lib
if exist %TESTNAME%.exe del %TESTNAME%.exe

:eof
