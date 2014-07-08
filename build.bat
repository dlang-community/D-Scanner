@echo off
setlocal enabledelayedexpansion

set DFLAGS=-O -release -inline
set CORE=
set STD=
set STDD=
set ANALYSIS=
set INIFILED=

for %%x in (*.d) do set CORE=!CORE! %%x
for %%x in (std/*.d) do set STD=!STD! std/%%x
for %%x in (std/d/*.d) do set STDD=!STDD! std/d/%%x
for %%x in (analysis/*.d) do set ANALYSIS=!ANALYSIS! analysis/%%x
for %%x in (inifiled/source/*.d) do set INIFILED=!INIFILED! inifiled/source/%%x

@echo on
dmd %CORE% %STD% %STDD% %ANALYSIS% %INIFILED% %DFLAGS% -ofdscanner.exe

