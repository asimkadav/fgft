#!/bin/bash

../scripts/reset_terminal.sh 1
$OUTPUT/vmlinux ubda=img mem=64M slub_debug=FZP
../scripts/reset_terminal.sh 2
