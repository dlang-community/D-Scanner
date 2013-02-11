#dmd *.d std/d/*.d -release -inline -noboundscheck -O -w -wi -m64 -property -ofdscanner-dmd
#dmd *.d std/d/*.d -g -m64 -w -wi -property -ofdscanner -unittest
ldc2 -O2 *.d std/d/*.d -of=dscanner-ldc -release -m64
#ldc2 *.d std/d/*.d -of=dscanner -unittest -m64 -g
#/opt/gdc/bin/gdc -O3 -odscanner-gdc -fno-bounds-check -frelease -m64 *.d std/d/*.d
