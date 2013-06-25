#!/bin/bash

# parallelize build?
PARALLEL=$1
DIVIDER="******************************************************************************"

rm -f output.txt

echo $DIVIDER >> output.txt
echo "Cleaning: " >> output.txt
echo $DIVIDER >> output.txt
make clean >> output.txt 2>&1

echo $DIVIDER >> output.txt
echo "Main output: " >> output.txt
echo $DIVIDER >> output.txt
if [ -z "$PARALLEL" ]; then
    make >> output.txt 2>&1
else
    make -j $PARALLEL >> output.txt 2>&1
fi

#cat output.txt | grep -i CHECK
cat output.txt | grep -i warning
cat output.txt | grep -i -E "error |error:"
cat output.txt | grep -i "undefined reference"
cat output.txt | grep -i "implicit declaration"


echo "===================================="
tail -n 5 output.txt

# cat output.txt
