#!/bin/bash

source /opt/microfocus/EnterpriseBuildTools/bin/cobsetenv
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
