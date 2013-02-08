#dmd *.d std/d/*.d -release -inline -noboundscheck -O -w -wi -m64 -property -ofdscanner -L-lsqlite3 #-inline
#dmd *.d std/d/*.d -g -m64 -w -wi -property -ofdscanner -unittest
#ldc2 -O3 *.d std/d/*.d -of=dscanner -release -vectorize -m64
ldc2 *.d std/d/*.d -of=dscanner -unittest -m64 -g
