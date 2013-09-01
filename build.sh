#dmd *.d stdx/d/*.d -release -inline -noboundscheck -O -w -wi -m64 -property -ofdscanner-dmd
dmd main.d stats.d imports.d highlighter.d ctags.d astprinter.d formatter.d stdx/d/*.d -g -m64 -wi -ofdscanner
#ldc2 -O3 *.d stdx/d/*.d -of=dscanner-ldc -release -m64
#ldc2 *.d stdx/d/*.d -of=dscanner -unittest -m64 -g
#/opt/gdc/bin/gdc -O3 -odscanner-gdc -fno-bounds-check -frelease -m64 *.d stdx/d/*.d
