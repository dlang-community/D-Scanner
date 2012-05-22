#dmd *.d -release -noboundscheck -O -w -wi -m64 -property -ofdscanner #-inline 
dmd *.d -g -unittest -m64 -w -wi -property -ofdscanner
