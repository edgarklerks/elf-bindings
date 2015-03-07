# elf-bindings

Bindings to elf.h including all the constants and macro's found. 

## Limitations  

At the moment, this is still work in progress, but with 64 bits elf headers, the raw bindings should work. 
32 bits headers are missing. A high level interface is almost non existing.

The constants and macros are generated from the elf.h. For structs this functionality needs to be build yet. 

## Installation

Run the following commands:

`cabal configure`
`cabal build`
`cabal install`

You should be able to import Elf.ElfHeaders and Elf.Constants. Elf.Types will contain the higher level interface, but 
this is incomplete yet. 
