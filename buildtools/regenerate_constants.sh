#!/bin/sh

elf_location=$(whereis elf.h | awk '{print $2}')
current_dir=$(dirname $0)
touch runned
if [ "$current_dir" = "." ]; then
    cd ..
    current_dir="buildtools"
fi


runhaskell "$current_dir/GenHsc.hs" "$elf_location"

cp "$current_dir/ElfMacrosTemplate.c" src/cfiles/ElfMacros.c
cat /tmp/cdecls.txt >> src/cfiles/ElfMacros.c


cp "$current_dir/ElfHeaderTemplate.hsc" src/Elf/ElfHeaders.hsc
cat /tmp/hdecls.txt >> src/Elf/ElfHeaders.hsc


cp "$current_dir/ConstantsTemplate.hsc" src/Elf/Constants.hsc
cat /tmp/constants.txt >> src/Elf/Constants.hsc
