if [ ! -d runs ]; then
    mkdir runs
fi
for file in /usr/include/d/std/*.d; do
    shortFile=$(basename $file)
    echo "Parsing" $shortFile "..."
    outFile=runs/$shortFile.txt
    ./tester $file > $outFile
done
echo
echo "Good files:"
grep -l "Parsing finished with 0 errors" runs/*.txt | sed -e "s/runs\///" -e "s/.txt//"
echo
echo "Bad files:"
grep -L "Parsing finished with 0 errors" runs/*.txt | sed -e "s/runs\///" -e "s/.txt//"
