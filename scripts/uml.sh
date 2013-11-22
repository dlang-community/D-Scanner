#!/bin/bash
dscanner $1 --ast | xsltproc uml.xsl - | dot -Tpng > $(basename $1 .d).png
#dscanner $1 --ast | xsltproc uml.xsl - | dot -Tpng | display -