@echo off
setlocal enabledelayedexpansion

set DFLAGS=-O -release -inline
set CORE=
set STD=
set STDD=
set ANALYSIS=
set INIFILED=

for %%x in (src\*.d) do set CORE=!CORE! %%x
for %%x in (src\analysis\*.d) do set ANALYSIS=!ANALYSIS! %%x
for %%x in (libdparse\src\std\*.d) do set STD=!STD! %%x
for %%x in (libdparse\src\std\d\*.d) do set STDD=!STDD! %%x
for %%x in (inifiled\source\*.d) do set INIFILED=!INIFILED! %%x

@echo on
dmd %CORE% %STD% %STDD% %ANALYSIS% %INIFILED% %DFLAGS% -ofdscanner.exe

