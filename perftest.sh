echo -e "file\tstd.d.lexer dmd\tstd.d.lexer ldc\tstd.d.lexer gdc\tdmd"
for i in `ls ../phobos/std/*.d`; do
	f=`echo $i | sed s/.*phobos\\\\///`
	dmdt=`avgtime -q -r 200 ./dscanner-dmd --tokenCount $i | grep "Median" | sed "s/.*: //"`
	ldct=`avgtime -q -r 200 ./dscanner-ldc --tokenCount $i | grep "Median" | sed "s/.*: //"`
	gdct=`avgtime -q -r 200 ./dscanner-gdc --tokenCount $i | grep "Median" | sed "s/.*: //"`
	gcct=`avgtime -q -r 200 ~/src/dmd-lexer/src/dmd $i | grep "Median" | sed "s/.*: //"`
	echo -e "${f}\t${dmdt}\t${ldct}\t${gdct}\t${gcct}"
done
