#!/bin/bash

if [ $# -ne 2 ]
then
    echo >&2 "Usage: $0 <infile> <outfile>"
    exit 99
fi

if [ -e $2 ]
then
    echo >&2 "$0: ERROR: output file \`$2' already exists."
    exit 98
fi

if [ ! -r $1 ]
then
    echo >&2 "$0: ERROR: cannot read input file \`$1'."
    exit 97
fi

cat > $2 <<EOF
(cond (() '(
EOF

cat $1 - >> $2 <<EOF
)))
EOF
