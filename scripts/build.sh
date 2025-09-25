#!/bin/bash

source /opt/microfocus/EnterpriseBuildTools/bin/cobsetenv

for f in ../source/bms/*{bms,BMS}
do
    if [[ -e "$f" ]]; then
        echo "building $f"
        mfbmscl "$f"
    fi
done

mv *cpy ../source/bmscopy

## BATCH

srcfldr="../source/batch"
directives=$(ls $srcfldr/*.dir 2>/dev/null | head -n 1)

echo "Directives file: $directives"

shopt -s nullglob
for f in $srcfldr/*{cbl,CBL}
do
    echo "building $f"
    cob -zU "$f" -C "USE($directives)"
done
shopt -u nullglob

## ONLINE

srcfldr="../source/online"
directives=$(ls $srcfldr/*.dir 2>/dev/null | head -n 1)

echo "Directives file: $directives"

shopt -s nullglob
for f in $srcfldr/*{cbl,CBL}
do
    echo "building $f"
    cob -zU "$f" -C "USE($directives)"
done
shopt -u nullglob