#!/bin/bash
output=$(echo "digraph {")
for i in "$@"; do
    m=$(echo $i | sed -e "s/^\.\///" -e "s/\//\./g" -e "s/\.d$//")
    output=$output$(dscanner --imports $i 2>/dev/null | sort | uniq | xargs -I{} echo "\"" $m "\"->\"" {} "\";")
done
output=$output$(echo "}")
echo $output | unflatten -l 3 -f | dot -Tpng > out.png
display out.png
