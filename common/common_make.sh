#!/bin/bash

PS3='Compilation mode?'
choicelist='gcc-remake gcc-make'

select selection in $choicelist; do
if [ $REPLY = "1" ]; then
    BUILDMODE="1"
    break;
elif [ $REPLY = "2" ]; then
    BUILDMODE="0"
    break;
fi
done

if [ $BUILDMODE = '1' ]; then
    ../common/common_clean.sh
    make ARCH=um O=$OUTPUT mrproper
    make ARCH=um V=1 O=$OUTPUT defconfig
    cp ./config_default $OUTPUT/.config
    make ARCH=um V=1 O=$OUTPUT oldconfig
    make ARCH=um V=1 O=$OUTPUT prepare
fi

make -j 8 ARCH=um V=1 O=$OUTPUT
