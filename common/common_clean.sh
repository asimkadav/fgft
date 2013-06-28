#!/bin/bash

echo "Removing ../compiled_images/$OUTPUT"
rm -rf ../compiled_images/$OUTPUT
mkdir ../compiled_images/$OUTPUT

make ARCH=um mrproper
make mrproper
