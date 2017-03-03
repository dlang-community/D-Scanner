@echo off
setlocal enabledelayedexpansion

set DFLAGS=-O -release -inline
set CORE=
set LIBDDOC=
set LIBDPARSE=
set STD=
set ANALYSIS=
set INIFILED=
set DSYMBOL=
set CONTAINERS=

for %%x in (src\*.d) do set CORE=!CORE! %%x
for %%x in (src\analysis\*.d) do set ANALYSIS=!ANALYSIS! %%x
for %%x in (libddoc\src\ddoc\*.d) do set LIBDDOC=!LIBDDOC! %%x
for %%x in (libdparse\experimental_allocator\src\std\experimental\allocator\*.d) do set STD=!STD! %%x
for %%x in (libdparse\experimental_allocator\src\std\experimental\allocator\building_blocks\*.d) do set STD=!STD! %%x
for %%x in (libdparse\src\dparse\*.d) do set LIBDPARSE=!LIBDPARSE! %%x
for %%x in (libdparse\src\std\experimental\*.d) do set LIBDPARSE=!LIBDPARSE! %%x
for %%x in (inifiled\source\*.d) do set INIFILED=!INIFILED! %%x
for %%x in (dsymbol\src\dsymbol\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (dsymbol\src\dsymbol\builtin\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (dsymbol\src\dsymbol\conversion\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (containers\src\containers\*.d) do set CONTAINERS=!CONTAINERS! %%x
for %%x in (containers\src\containers\internal\*.d) do set CONTAINERS=!CONTAINERS! %%x

@echo on
dmd %CORE% %STD% %LIBDDOC% %LIBDPARSE% %ANALYSIS% %INIFILED% %DSYMBOL% %CONTAINERS% %DFLAGS% -I"libddoc\src" -I"libdparse\src" -I"dsymbol\src" -I"containers\src" -ofdscanner.exe

