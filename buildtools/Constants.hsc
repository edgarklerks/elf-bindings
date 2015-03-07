#define _POSIX_SOURCE
#include "config.h"
#include <bindings.dsl.h>
#include <elf.h>
#include <stddef.h>
module Elf.Constants where
#strict_import
 
-- | This file defines standard ELF types, structures, and macros.
 
#num _ELF_H
#num EI_NIDENT
 
-- | File identification byte 0 index 
#num EI_MAG0
 
-- | Magic number byte 0 
#num ELFMAG0
 
-- | File identification byte 1 index 
#num EI_MAG1
 
-- | Magic number byte 1 
#num ELFMAG1
 
-- | File identification byte 2 index 
#num EI_MAG2
 
-- | Magic number byte 2 
#num ELFMAG2
 
-- | File identification byte 3 index 
#num EI_MAG3
 
-- | Magic number byte 3 
#num ELFMAG3
#num ELFMAG
#num SELFMAG
 
-- | File class byte index 
#num EI_CLASS
 
-- | Invalid class 
#num ELFCLASSNONE
 
-- | 32-bit objects 
#num ELFCLASS32
 
-- | 64-bit objects 
#num ELFCLASS64
#num ELFCLASSNUM
 
-- | Data encoding byte index 
#num EI_DATA
 
-- | Invalid data encoding 
#num ELFDATANONE
 
-- | 2's complement, little endian 
#num ELFDATA2LSB
 
-- | 2's complement, big endian 
#num ELFDATA2MSB
#num ELFDATANUM
 
-- | File version byte index 
#num EI_VERSION
 
-- | OS ABI identification 
#num EI_OSABI
 
-- | UNIX System V ABI 
#num ELFOSABI_NONE
 
-- | Alias.  
#num ELFOSABI_SYSV
 
-- | HP-UX 
#num ELFOSABI_HPUX
 
-- | NetBSD.  
#num ELFOSABI_NETBSD
 
-- | Object uses GNU ELF extensions.  
#num ELFOSABI_GNU
 
-- | Compatibility alias.  
#num ELFOSABI_LINUX
 
-- | Sun Solaris.  
#num ELFOSABI_SOLARIS
 
-- | IBM AIX.  
#num ELFOSABI_AIX
 
-- | SGI Irix.  
#num ELFOSABI_IRIX
 
-- | FreeBSD.  
#num ELFOSABI_FREEBSD
 
-- | Compaq TRU64 UNIX.  
#num ELFOSABI_TRU64
 
-- | Novell Modesto.  
#num ELFOSABI_MODESTO
 
-- | OpenBSD.  
#num ELFOSABI_OPENBSD
 
-- | ARM EABI 
#num ELFOSABI_ARM_AEABI
 
-- | ARM 
#num ELFOSABI_ARM
 
-- | Standalone (embedded) application 
#num ELFOSABI_STANDALONE
 
-- | ABI version 
#num EI_ABIVERSION
 
-- | Byte index of padding bytes 
#num EI_PAD
 
-- | No file type 
#num ET_NONE
 
-- | Relocatable file 
#num ET_REL
 
-- | Executable file 
#num ET_EXEC
 
-- | Shared object file 
#num ET_DYN
 
-- | Core file 
#num ET_CORE
 
-- | Number of defined types 
#num ET_NUM
 
-- | OS-specific range start 
#num ET_LOOS
 
-- | OS-specific range end 
#num ET_HIOS
 
-- | Processor-specific range start 
#num ET_LOPROC
 
-- | Processor-specific range end 
#num ET_HIPROC
 
-- | No machine 
#num EM_NONE
 
-- | AT&T WE 32100 
#num EM_M32
 
-- | SUN SPARC 
#num EM_SPARC
 
-- | Intel 80386 
#num EM_386
 
-- | Motorola m68k family 
#num EM_68K
 
-- | Motorola m88k family 
#num EM_88K
 
-- | Intel 80860 
#num EM_860
 
-- | MIPS R3000 big-endian 
#num EM_MIPS
 
-- | IBM System/370 
#num EM_S370
 
-- | MIPS R3000 little-endian 
#num EM_MIPS_RS3_LE
 
-- | HPPA 
#num EM_PARISC
 
-- | Fujitsu VPP500 
#num EM_VPP500
 
-- | Sun's "v8plus" 
#num EM_SPARC32PLUS
 
-- | Intel 80960 
#num EM_960
 
-- | PowerPC 
#num EM_PPC
 
-- | PowerPC 64-bit 
#num EM_PPC64
 
-- | IBM S390 
#num EM_S390
 
-- | NEC V800 series 
#num EM_V800
 
-- | Fujitsu FR20 
#num EM_FR20
 
-- | TRW RH-32 
#num EM_RH32
 
-- | Motorola RCE 
#num EM_RCE
 
-- | ARM 
#num EM_ARM
 
-- | Digital Alpha 
#num EM_FAKE_ALPHA
 
-- | Hitachi SH 
#num EM_SH
 
-- | SPARC v9 64-bit 
#num EM_SPARCV9
 
-- | Siemens Tricore 
#num EM_TRICORE
 
-- | Argonaut RISC Core 
#num EM_ARC
 
-- | Hitachi H8/300 
#num EM_H8_300
 
-- | Hitachi H8/300H 
#num EM_H8_300H
 
-- | Hitachi H8S 
#num EM_H8S
 
-- | Hitachi H8/500 
#num EM_H8_500
 
-- | Intel Merced 
#num EM_IA_64
 
-- | Stanford MIPS-X 
#num EM_MIPS_X
 
-- | Motorola Coldfire 
#num EM_COLDFIRE
 
-- | Motorola M68HC12 
#num EM_68HC12
 
-- | Fujitsu MMA Multimedia Accelerator
#num EM_MMA
 
-- | Siemens PCP 
#num EM_PCP
 
-- | Sony nCPU embeeded RISC 
#num EM_NCPU
 
-- | Denso NDR1 microprocessor 
#num EM_NDR1
 
-- | Motorola Start*Core processor 
#num EM_STARCORE
 
-- | Toyota ME16 processor 
#num EM_ME16
 
-- | STMicroelectronic ST100 processor 
#num EM_ST100
 
-- | Advanced Logic Corp. Tinyj emb.fam
#num EM_TINYJ
 
-- | AMD x86-64 architecture 
#num EM_X86_64
 
-- | Sony DSP Processor 
#num EM_PDSP
 
-- | Siemens FX66 microcontroller 
#num EM_FX66
 
-- | STMicroelectronics ST9+ 8/16 mc 
#num EM_ST9PLUS
 
-- | STmicroelectronics ST7 8 bit mc 
#num EM_ST7
 
-- | Motorola MC68HC16 microcontroller 
#num EM_68HC16
 
-- | Motorola MC68HC11 microcontroller 
#num EM_68HC11
 
-- | Motorola MC68HC08 microcontroller 
#num EM_68HC08
 
-- | Motorola MC68HC05 microcontroller 
#num EM_68HC05
 
-- | Silicon Graphics SVx 
#num EM_SVX
 
-- | STMicroelectronics ST19 8 bit mc 
#num EM_ST19
 
-- | Digital VAX 
#num EM_VAX
 
-- | Axis Communications 32-bit embedded processor 
#num EM_CRIS
 
-- | Infineon Technologies 32-bit embedded processor 
#num EM_JAVELIN
 
-- | Element 14 64-bit DSP Processor 
#num EM_FIREPATH
 
-- | LSI Logic 16-bit DSP Processor 
#num EM_ZSP
 
-- | Donald Knuth's educational 64-bit processor 
#num EM_MMIX
 
-- | Harvard University machine-independent object files 
#num EM_HUANY
 
-- | SiTera Prism 
#num EM_PRISM
 
-- | Atmel AVR 8-bit microcontroller 
#num EM_AVR
 
-- | Fujitsu FR30 
#num EM_FR30
 
-- | Mitsubishi D10V 
#num EM_D10V
 
-- | Mitsubishi D30V 
#num EM_D30V
 
-- | NEC v850 
#num EM_V850
 
-- | Mitsubishi M32R 
#num EM_M32R
 
-- | Matsushita MN10300 
#num EM_MN10300
 
-- | Matsushita MN10200 
#num EM_MN10200
 
-- | picoJava 
#num EM_PJ
 
-- | OpenRISC 32-bit embedded processor 
#num EM_OPENRISC
 
-- | ARC Cores Tangent-A5 
#num EM_ARC_A5
 
-- | Tensilica Xtensa Architecture 
#num EM_XTENSA
 
-- | ARM AARCH64 
#num EM_AARCH64
 
-- | Tilera TILEPro 
#num EM_TILEPRO
 
-- | Xilinx MicroBlaze 
#num EM_MICROBLAZE
 
-- | Tilera TILE-Gx 
#num EM_TILEGX
#num EM_NUM
#num EM_ALPHA
 
-- | Invalid ELF version 
#num EV_NONE
 
-- | Current version 
#num EV_CURRENT
#num EV_NUM
 
-- | Undefined section 
#num SHN_UNDEF
 
-- | Start of reserved indices 
#num SHN_LORESERVE
 
-- | Start of processor-specific 
#num SHN_LOPROC
 
-- | Order section before all others
#num SHN_BEFORE
 
-- | Order section after all others
#num SHN_AFTER
 
-- | End of processor-specific 
#num SHN_HIPROC
 
-- | Start of OS-specific 
#num SHN_LOOS
 
-- | End of OS-specific 
#num SHN_HIOS
 
-- | Associated symbol is absolute 
#num SHN_ABS
 
-- | Associated symbol is common 
#num SHN_COMMON
 
-- | Index is in extra table.  
#num SHN_XINDEX
 
-- | End of reserved indices 
#num SHN_HIRESERVE
 
-- | Section header table entry unused 
#num SHT_NULL
 
-- | Program data 
#num SHT_PROGBITS
 
-- | Symbol table 
#num SHT_SYMTAB
 
-- | String table 
#num SHT_STRTAB
 
-- | Relocation entries with addends 
#num SHT_RELA
 
-- | Symbol hash table 
#num SHT_HASH
 
-- | Dynamic linking information 
#num SHT_DYNAMIC
 
-- | Notes 
#num SHT_NOTE
 
-- | Program space with no data (bss) 
#num SHT_NOBITS
 
-- | Relocation entries, no addends 
#num SHT_REL
 
-- | Reserved 
#num SHT_SHLIB
 
-- | Dynamic linker symbol table 
#num SHT_DYNSYM
 
-- | Array of constructors 
#num SHT_INIT_ARRAY
 
-- | Array of destructors 
#num SHT_FINI_ARRAY
 
-- | Array of pre-constructors 
#num SHT_PREINIT_ARRAY
 
-- | Section group 
#num SHT_GROUP
 
-- | Extended section indeces 
#num SHT_SYMTAB_SHNDX
 
-- | Number of defined types.  
#num SHT_NUM
 
-- | Start OS-specific.  
#num SHT_LOOS
 
-- | Object attributes.  
#num SHT_GNU_ATTRIBUTES
 
-- | GNU-style hash table.  
#num SHT_GNU_HASH
 
-- | Prelink library list 
#num SHT_GNU_LIBLIST
 
-- | Checksum for DSO content.  
#num SHT_CHECKSUM
 
-- | Sun-specific low bound.  
#num SHT_LOSUNW
#num SHT_SUNW_move
#num SHT_SUNW_COMDAT
#num SHT_SUNW_syminfo
 
-- | Version definition section.  
#num SHT_GNU_verdef
 
-- | Version needs section.  
#num SHT_GNU_verneed
 
-- | Version symbol table.  
#num SHT_GNU_versym
 
-- | Sun-specific high bound.  
#num SHT_HISUNW
 
-- | End OS-specific type 
#num SHT_HIOS
 
-- | Start of processor-specific 
#num SHT_LOPROC
 
-- | End of processor-specific 
#num SHT_HIPROC
 
-- | Start of application-specific 
#num SHT_LOUSER
 
-- | End of application-specific 
#num SHT_HIUSER
 
-- | Writable 
#num SHF_WRITE
 
-- | Occupies memory during execution 
#num SHF_ALLOC
 
-- | Executable 
#num SHF_EXECINSTR
 
-- | Might be merged 
#num SHF_MERGE
 
-- | Contains nul-terminated strings 
#num SHF_STRINGS
 
-- | `sh_info' contains SHT index 
#num SHF_INFO_LINK
 
-- | Preserve order after combining 
#num SHF_LINK_ORDER
 
-- | Non-standard OS specific handling
#num SHF_OS_NONCONFORMING
 
-- | Section is member of a group.  
#num SHF_GROUP
 
-- | Section hold thread-local data.  
#num SHF_TLS
 
-- | OS-specific.  
#num SHF_MASKOS
 
-- | Processor-specific 
#num SHF_MASKPROC
 
-- | Special ordering requirement
#num SHF_ORDERED
 
-- | Section is excluded unless
#num SHF_EXCLUDE
 
-- | Mark group as COMDAT.  
#num GRP_COMDAT
 
-- | Symbol bound to self 
#num SYMINFO_BT_SELF
 
-- | Symbol bound to parent 
#num SYMINFO_BT_PARENT
 
-- | Beginning of reserved entries 
#num SYMINFO_BT_LOWRESERVE
 
-- | Direct bound symbol 
#num SYMINFO_FLG_DIRECT
 
-- | Pass-thru symbol for translator 
#num SYMINFO_FLG_PASSTHRU
 
-- | Symbol is a copy-reloc 
#num SYMINFO_FLG_COPY
 
-- | Symbol bound to object to be lazy
#num SYMINFO_FLG_LAZYLOAD
#num SYMINFO_NONE
#num SYMINFO_CURRENT
#num SYMINFO_NUM
 
-- | Local symbol 
#num STB_LOCAL
 
-- | Global symbol 
#num STB_GLOBAL
 
-- | Weak symbol 
#num STB_WEAK
 
-- | Number of defined types.  
#num STB_NUM
 
-- | Start of OS-specific 
#num STB_LOOS
 
-- | Unique symbol.  
#num STB_GNU_UNIQUE
 
-- | End of OS-specific 
#num STB_HIOS
 
-- | Start of processor-specific 
#num STB_LOPROC
 
-- | End of processor-specific 
#num STB_HIPROC
 
-- | Symbol type is unspecified 
#num STT_NOTYPE
 
-- | Symbol is a data object 
#num STT_OBJECT
 
-- | Symbol is a code object 
#num STT_FUNC
 
-- | Symbol associated with a section 
#num STT_SECTION
 
-- | Symbol's name is file name 
#num STT_FILE
 
-- | Symbol is a common data object 
#num STT_COMMON
 
-- | Symbol is thread-local data object
#num STT_TLS
 
-- | Number of defined types.  
#num STT_NUM
 
-- | Start of OS-specific 
#num STT_LOOS
 
-- | Symbol is indirect code object 
#num STT_GNU_IFUNC
 
-- | End of OS-specific 
#num STT_HIOS
 
-- | Start of processor-specific 
#num STT_LOPROC
 
-- | End of processor-specific 
#num STT_HIPROC
 
-- | End of a chain.  
#num STN_UNDEF
 
-- | Default symbol visibility rules 
#num STV_DEFAULT
 
-- | Processor specific hidden class 
#num STV_INTERNAL
 
-- | Sym unavailable in other modules 
#num STV_HIDDEN
 
-- | Not preemptible, not exported 
#num STV_PROTECTED
#num PN_XNUM
 
-- | Program header table entry unused 
#num PT_NULL
 
-- | Loadable program segment 
#num PT_LOAD
 
-- | Dynamic linking information 
#num PT_DYNAMIC
 
-- | Program interpreter 
#num PT_INTERP
 
-- | Auxiliary information 
#num PT_NOTE
 
-- | Reserved 
#num PT_SHLIB
 
-- | Entry for header table itself 
#num PT_PHDR
 
-- | Thread-local storage segment 
#num PT_TLS
 
-- | Number of defined types 
#num PT_NUM
 
-- | Start of OS-specific 
#num PT_LOOS
 
-- | GCC .eh_frame_hdr segment 
#num PT_GNU_EH_FRAME
 
-- | Indicates stack executability 
#num PT_GNU_STACK
 
-- | Read-only after relocation 
#num PT_GNU_RELRO
#num PT_LOSUNW
 
-- | Sun Specific segment 
#num PT_SUNWBSS
 
-- | Stack segment 
#num PT_SUNWSTACK
#num PT_HISUNW
 
-- | End of OS-specific 
#num PT_HIOS
 
-- | Start of processor-specific 
#num PT_LOPROC
 
-- | End of processor-specific 
#num PT_HIPROC
 
-- | Segment is executable 
#num PF_X
 
-- | Segment is writable 
#num PF_W
 
-- | Segment is readable 
#num PF_R
 
-- | OS-specific 
#num PF_MASKOS
 
-- | Processor-specific 
#num PF_MASKPROC
 
-- | Contains copy of prstatus struct 
#num NT_PRSTATUS
 
-- | Contains copy of fpregset struct 
#num NT_FPREGSET
 
-- | Contains copy of prpsinfo struct 
#num NT_PRPSINFO
 
-- | Contains copy of prxregset struct 
#num NT_PRXREG
 
-- | Contains copy of task structure 
#num NT_TASKSTRUCT
 
-- | String from sysinfo(SI_PLATFORM) 
#num NT_PLATFORM
 
-- | Contains copy of auxv array 
#num NT_AUXV
 
-- | Contains copy of gwindows struct 
#num NT_GWINDOWS
 
-- | Contains copy of asrset struct 
#num NT_ASRS
 
-- | Contains copy of pstatus struct 
#num NT_PSTATUS
 
-- | Contains copy of psinfo struct 
#num NT_PSINFO
 
-- | Contains copy of prcred struct 
#num NT_PRCRED
 
-- | Contains copy of utsname struct 
#num NT_UTSNAME
 
-- | Contains copy of lwpstatus struct 
#num NT_LWPSTATUS
 
-- | Contains copy of lwpinfo struct 
#num NT_LWPSINFO
 
-- | Contains copy of fprxregset struct 
#num NT_PRFPXREG
 
-- | Contains copy of siginfo_t,
#num NT_SIGINFO
 
-- | Contains information about mapped
#num NT_FILE
 
-- | Contains copy of user_fxsr_struct 
#num NT_PRXFPREG
 
-- | PowerPC Altivec/VMX registers 
#num NT_PPC_VMX
 
-- | PowerPC SPE/EVR registers 
#num NT_PPC_SPE
 
-- | PowerPC VSX registers 
#num NT_PPC_VSX
 
-- | i386 TLS slots (struct user_desc) 
#num NT_386_TLS
 
-- | x86 io permission bitmap (1=deny) 
#num NT_386_IOPERM
 
-- | x86 extended state using xsave 
#num NT_X86_XSTATE
 
-- | s390 upper register halves 
#num NT_S390_HIGH_GPRS
 
-- | s390 timer register 
#num NT_S390_TIMER
 
-- | s390 TOD clock comparator register 
#num NT_S390_TODCMP
 
-- | s390 TOD programmable register 
#num NT_S390_TODPREG
 
-- | s390 control registers 
#num NT_S390_CTRS
 
-- | s390 prefix register 
#num NT_S390_PREFIX
 
-- | s390 breaking event address 
#num NT_S390_LAST_BREAK
 
-- | s390 system call restart data 
#num NT_S390_SYSTEM_CALL
 
-- | s390 transaction diagnostic block 
#num NT_S390_TDB
 
-- | ARM VFP/NEON registers 
#num NT_ARM_VFP
 
-- | ARM TLS register 
#num NT_ARM_TLS
 
-- | ARM hardware breakpoint registers 
#num NT_ARM_HW_BREAK
 
-- | ARM hardware watchpoint registers 
#num NT_ARM_HW_WATCH
 
-- | Contains a version string.  
#num NT_VERSION
 
-- | Marks end of dynamic section 
#num DT_NULL
 
-- | Name of needed library 
#num DT_NEEDED
 
-- | Size in bytes of PLT relocs 
#num DT_PLTRELSZ
 
-- | Processor defined value 
#num DT_PLTGOT
 
-- | Address of symbol hash table 
#num DT_HASH
 
-- | Address of string table 
#num DT_STRTAB
 
-- | Address of symbol table 
#num DT_SYMTAB
 
-- | Address of Rela relocs 
#num DT_RELA
 
-- | Total size of Rela relocs 
#num DT_RELASZ
 
-- | Size of one Rela reloc 
#num DT_RELAENT
 
-- | Size of string table 
#num DT_STRSZ
 
-- | Size of one symbol table entry 
#num DT_SYMENT
 
-- | Address of init function 
#num DT_INIT
 
-- | Address of termination function 
#num DT_FINI
 
-- | Name of shared object 
#num DT_SONAME
 
-- | Library search path (deprecated) 
#num DT_RPATH
 
-- | Start symbol search here 
#num DT_SYMBOLIC
 
-- | Address of Rel relocs 
#num DT_REL
 
-- | Total size of Rel relocs 
#num DT_RELSZ
 
-- | Size of one Rel reloc 
#num DT_RELENT
 
-- | Type of reloc in PLT 
#num DT_PLTREL
 
-- | For debugging; unspecified 
#num DT_DEBUG
 
-- | Reloc might modify .text 
#num DT_TEXTREL
 
-- | Address of PLT relocs 
#num DT_JMPREL
 
-- | Process relocations of object 
#num DT_BIND_NOW
 
-- | Array with addresses of init fct 
#num DT_INIT_ARRAY
 
-- | Array with addresses of fini fct 
#num DT_FINI_ARRAY
 
-- | Size in bytes of DT_INIT_ARRAY 
#num DT_INIT_ARRAYSZ
 
-- | Size in bytes of DT_FINI_ARRAY 
#num DT_FINI_ARRAYSZ
 
-- | Library search path 
#num DT_RUNPATH
 
-- | Flags for the object being loaded 
#num DT_FLAGS
 
-- | Start of encoded range 
#num DT_ENCODING
 
-- | Array with addresses of preinit fct
#num DT_PREINIT_ARRAY
 
-- | size in bytes of DT_PREINIT_ARRAY 
#num DT_PREINIT_ARRAYSZ
 
-- | Number used 
#num DT_NUM
 
-- | Start of OS-specific 
#num DT_LOOS
 
-- | End of OS-specific 
#num DT_HIOS
 
-- | Start of processor-specific 
#num DT_LOPROC
 
-- | End of processor-specific 
#num DT_HIPROC
 
-- | Most used by any processor 
#num DT_PROCNUM
#num DT_VALRNGLO
 
-- | Prelinking timestamp 
#num DT_GNU_PRELINKED
 
-- | Size of conflict section 
#num DT_GNU_CONFLICTSZ
 
-- | Size of library list 
#num DT_GNU_LIBLISTSZ
#num DT_CHECKSUM
#num DT_PLTPADSZ
#num DT_MOVEENT
#num DT_MOVESZ
 
-- | Feature selection (DTF_*).  
#num DT_FEATURE_1
 
-- | Flags for DT_* entries, effecting
#num DT_POSFLAG_1
 
-- | Size of syminfo table (in bytes) 
#num DT_SYMINSZ
 
-- | Entry size of syminfo 
#num DT_SYMINENT
#num DT_VALRNGHI
#num DT_VALNUM
#num DT_ADDRRNGLO
 
-- | GNU-style hash table.  
#num DT_GNU_HASH
#num DT_TLSDESC_PLT
#num DT_TLSDESC_GOT
 
-- | Start of conflict section 
#num DT_GNU_CONFLICT
 
-- | Library list 
#num DT_GNU_LIBLIST
 
-- | Configuration information.  
#num DT_CONFIG
 
-- | Dependency auditing.  
#num DT_DEPAUDIT
 
-- | Object auditing.  
#num DT_AUDIT
 
-- | PLT padding.  
#num DT_PLTPAD
 
-- | Move table.  
#num DT_MOVETAB
 
-- | Syminfo table.  
#num DT_SYMINFO
#num DT_ADDRRNGHI
#num DT_ADDRNUM
 
-- | The versioning entry types.  The next are defined as part of the
 
#num DT_VERSYM
#num DT_RELACOUNT
#num DT_RELCOUNT
 
-- | State flags, see DF_1_* below.  
#num DT_FLAGS_1
 
-- | Address of version definition
#num DT_VERDEF
 
-- | Number of version definitions 
#num DT_VERDEFNUM
 
-- | Address of table with needed
#num DT_VERNEED
 
-- | Number of needed versions 
#num DT_VERNEEDNUM
#num DT_VERSIONTAGNUM
 
-- | Shared object to load before self 
#num DT_AUXILIARY
 
-- | Shared object to get values from 
#num DT_FILTER
#num DT_EXTRANUM
 
-- | Object may use DF_ORIGIN 
#num DF_ORIGIN
 
-- | Symbol resolutions starts here 
#num DF_SYMBOLIC
 
-- | Object contains text relocations 
#num DF_TEXTREL
 
-- | No lazy binding for this object 
#num DF_BIND_NOW
 
-- | Module uses the static TLS model 
#num DF_STATIC_TLS
 
-- | Set RTLD_NOW for this object.  
#num DF_1_NOW
 
-- | Set RTLD_GLOBAL for this object.  
#num DF_1_GLOBAL
 
-- | Set RTLD_GROUP for this object.  
#num DF_1_GROUP
 
-- | Set RTLD_NODELETE for this object.
#num DF_1_NODELETE
 
-- | Trigger filtee loading at runtime.
#num DF_1_LOADFLTR
 
-- | Set RTLD_INITFIRST for this object
#num DF_1_INITFIRST
 
-- | Set RTLD_NOOPEN for this object.  
#num DF_1_NOOPEN
 
-- | $ORIGIN must be handled.  
#num DF_1_ORIGIN
 
-- | Direct binding enabled.  
#num DF_1_DIRECT
#num DF_1_TRANS
 
-- | Object is used to interpose.  
#num DF_1_INTERPOSE
 
-- | Ignore default lib search path.  
#num DF_1_NODEFLIB
 
-- | Object can't be dldump'ed.  
#num DF_1_NODUMP
 
-- | Configuration alternative created.
#num DF_1_CONFALT
 
-- | Filtee terminates filters search. 
#num DF_1_ENDFILTEE
 
-- | Disp reloc applied at build time. 
#num DF_1_DISPRELDNE
 
-- | Disp reloc applied at run-time.  
#num DF_1_DISPRELPND
 
-- | Object has no-direct binding. 
#num DF_1_NODIRECT
#num DF_1_IGNMULDEF
#num DF_1_NOKSYMS
#num DF_1_NOHDR
 
-- | Object is modified after built.  
#num DF_1_EDITED
#num DF_1_NORELOC
 
-- | Object has individual interposers.  
#num DF_1_SYMINTPOSE
 
-- | Global auditing required.  
#num DF_1_GLOBAUDIT
 
-- | Singleton symbols are used.  
#num DF_1_SINGLETON
#num DTF_1_PARINIT
#num DTF_1_CONFEXP
 
-- | Lazyload following object.  
#num DF_P1_LAZYLOAD
 
-- | Symbols from next object are not
#num DF_P1_GROUPPERM
 
-- | No version 
#num VER_DEF_NONE
 
-- | Current version 
#num VER_DEF_CURRENT
 
-- | Given version number 
#num VER_DEF_NUM
 
-- | Version definition of file itself 
#num VER_FLG_BASE
 
-- | Weak version identifier 
#num VER_FLG_WEAK
 
-- | Symbol is local.  
#num VER_NDX_LOCAL
 
-- | Symbol is global.  
#num VER_NDX_GLOBAL
 
-- | Beginning of reserved entries.  
#num VER_NDX_LORESERVE
 
-- | Symbol is to be eliminated.  
#num VER_NDX_ELIMINATE
 
-- | No version 
#num VER_NEED_NONE
 
-- | Current version 
#num VER_NEED_CURRENT
 
-- | Given version number 
#num VER_NEED_NUM
 
-- | Weak version identifier 
#num VER_FLG_WEAK
vector is
#num ELF_NOTE_SOLARIS
#num ELF_NOTE_GNU
#num ELF_NOTE_PAGESIZE_HINT
#num NT_GNU_ABI_TAG
 
-- | Old name.  
#num ELF_NOTE_ABI
#num ELF_NOTE_OS_LINUX
#num ELF_NOTE_OS_GNU
#num ELF_NOTE_OS_SOLARIS2
#num ELF_NOTE_OS_FREEBSD
#num NT_GNU_HWCAP
#num NT_GNU_BUILD_ID
#num NT_GNU_GOLD_VERSION
#num EF_CPU32
 
-- | No reloc 
#num R_68K_NONE
 
-- | Direct 32 bit  
#num R_68K_32
 
-- | Direct 16 bit  
#num R_68K_16
 
-- | Direct 8 bit  
#num R_68K_8
 
-- | PC relative 32 bit 
#num R_68K_PC32
 
-- | PC relative 16 bit 
#num R_68K_PC16
 
-- | PC relative 8 bit 
#num R_68K_PC8
 
-- | 32 bit PC relative GOT entry 
#num R_68K_GOT32
 
-- | 16 bit PC relative GOT entry 
#num R_68K_GOT16
 
-- | 8 bit PC relative GOT entry 
#num R_68K_GOT8
 
-- | 32 bit GOT offset 
#num R_68K_GOT32O
 
-- | 16 bit GOT offset 
#num R_68K_GOT16O
 
-- | 8 bit GOT offset 
#num R_68K_GOT8O
 
-- | 32 bit PC relative PLT address 
#num R_68K_PLT32
 
-- | 16 bit PC relative PLT address 
#num R_68K_PLT16
 
-- | 8 bit PC relative PLT address 
#num R_68K_PLT8
 
-- | 32 bit PLT offset 
#num R_68K_PLT32O
 
-- | 16 bit PLT offset 
#num R_68K_PLT16O
 
-- | 8 bit PLT offset 
#num R_68K_PLT8O
 
-- | Copy symbol at runtime 
#num R_68K_COPY
 
-- | Create GOT entry 
#num R_68K_GLOB_DAT
 
-- | Create PLT entry 
#num R_68K_JMP_SLOT
 
-- | Adjust by program base 
#num R_68K_RELATIVE
 
-- | 32 bit GOT offset for GD 
#num R_68K_TLS_GD32
 
-- | 16 bit GOT offset for GD 
#num R_68K_TLS_GD16
 
-- | 8 bit GOT offset for GD 
#num R_68K_TLS_GD8
 
-- | 32 bit GOT offset for LDM 
#num R_68K_TLS_LDM32
 
-- | 16 bit GOT offset for LDM 
#num R_68K_TLS_LDM16
 
-- | 8 bit GOT offset for LDM 
#num R_68K_TLS_LDM8
 
-- | 32 bit module-relative offset 
#num R_68K_TLS_LDO32
 
-- | 16 bit module-relative offset 
#num R_68K_TLS_LDO16
 
-- | 8 bit module-relative offset 
#num R_68K_TLS_LDO8
 
-- | 32 bit GOT offset for IE 
#num R_68K_TLS_IE32
 
-- | 16 bit GOT offset for IE 
#num R_68K_TLS_IE16
 
-- | 8 bit GOT offset for IE 
#num R_68K_TLS_IE8
 
-- | 32 bit offset relative to
#num R_68K_TLS_LE32
 
-- | 16 bit offset relative to
#num R_68K_TLS_LE16
 
-- | 8 bit offset relative to
#num R_68K_TLS_LE8
 
-- | 32 bit module number 
#num R_68K_TLS_DTPMOD32
 
-- | 32 bit module-relative offset 
#num R_68K_TLS_DTPREL32
 
-- | 32 bit TP-relative offset 
#num R_68K_TLS_TPREL32
#num R_68K_NUM
 
-- | No reloc 
#num R_386_NONE
 
-- | Direct 32 bit  
#num R_386_32
 
-- | PC relative 32 bit 
#num R_386_PC32
 
-- | 32 bit GOT entry 
#num R_386_GOT32
 
-- | 32 bit PLT address 
#num R_386_PLT32
 
-- | Copy symbol at runtime 
#num R_386_COPY
 
-- | Create GOT entry 
#num R_386_GLOB_DAT
 
-- | Create PLT entry 
#num R_386_JMP_SLOT
 
-- | Adjust by program base 
#num R_386_RELATIVE
 
-- | 32 bit offset to GOT 
#num R_386_GOTOFF
 
-- | 32 bit PC relative offset to GOT 
#num R_386_GOTPC
#num R_386_32PLT
 
-- | Offset in static TLS block 
#num R_386_TLS_TPOFF
 
-- | Address of GOT entry for static TLS
#num R_386_TLS_IE
 
-- | GOT entry for static TLS block
#num R_386_TLS_GOTIE
 
-- | Offset relative to static TLS
#num R_386_TLS_LE
 
-- | Direct 32 bit for GNU version of
#num R_386_TLS_GD
 
-- | Direct 32 bit for GNU version of
#num R_386_TLS_LDM
#num R_386_16
#num R_386_PC16
#num R_386_8
#num R_386_PC8
 
-- | Direct 32 bit for general dynamic
#num R_386_TLS_GD_32
 
-- | Tag for pushl in GD TLS code 
#num R_386_TLS_GD_PUSH
 
-- | Relocation for call to
#num R_386_TLS_GD_CALL
 
-- | Tag for popl in GD TLS code 
#num R_386_TLS_GD_POP
 
-- | Direct 32 bit for local dynamic
#num R_386_TLS_LDM_32
 
-- | Tag for pushl in LDM TLS code 
#num R_386_TLS_LDM_PUSH
 
-- | Relocation for call to
#num R_386_TLS_LDM_CALL
 
-- | Tag for popl in LDM TLS code 
#num R_386_TLS_LDM_POP
 
-- | Offset relative to TLS block 
#num R_386_TLS_LDO_32
 
-- | GOT entry for negated static TLS
#num R_386_TLS_IE_32
 
-- | Negated offset relative to static
#num R_386_TLS_LE_32
 
-- | ID of module containing symbol 
#num R_386_TLS_DTPMOD32
 
-- | Offset in TLS block 
#num R_386_TLS_DTPOFF32
 
-- | Negated offset in static TLS block 
#num R_386_TLS_TPOFF32
 
-- | 32-bit symbol size 
#num R_386_SIZE32
 
-- | GOT offset for TLS descriptor.  
#num R_386_TLS_GOTDESC
 
-- | Marker of call through TLS
#num R_386_TLS_DESC_CALL
 
-- | TLS descriptor containing
#num R_386_TLS_DESC
 
-- | Adjust indirectly by program base 
#num R_386_IRELATIVE
#num R_386_NUM
 
-- | Global register reserved to app. 
#num STT_SPARC_REGISTER
#num EF_SPARCV9_MM
#num EF_SPARCV9_TSO
#num EF_SPARCV9_PSO
#num EF_SPARCV9_RMO
 
-- | little endian data 
#num EF_SPARC_LEDATA
#num EF_SPARC_EXT_MASK
 
-- | generic V8+ features 
#num EF_SPARC_32PLUS
 
-- | Sun UltraSPARC1 extensions 
#num EF_SPARC_SUN_US1
 
-- | HAL R1 extensions 
#num EF_SPARC_HAL_R1
 
-- | Sun UltraSPARCIII extensions 
#num EF_SPARC_SUN_US3
 
-- | No reloc 
#num R_SPARC_NONE
 
-- | Direct 8 bit 
#num R_SPARC_8
 
-- | Direct 16 bit 
#num R_SPARC_16
 
-- | Direct 32 bit 
#num R_SPARC_32
 
-- | PC relative 8 bit 
#num R_SPARC_DISP8
 
-- | PC relative 16 bit 
#num R_SPARC_DISP16
 
-- | PC relative 32 bit 
#num R_SPARC_DISP32
 
-- | PC relative 30 bit shifted 
#num R_SPARC_WDISP30
 
-- | PC relative 22 bit shifted 
#num R_SPARC_WDISP22
 
-- | High 22 bit 
#num R_SPARC_HI22
 
-- | Direct 22 bit 
#num R_SPARC_22
 
-- | Direct 13 bit 
#num R_SPARC_13
 
-- | Truncated 10 bit 
#num R_SPARC_LO10
 
-- | Truncated 10 bit GOT entry 
#num R_SPARC_GOT10
 
-- | 13 bit GOT entry 
#num R_SPARC_GOT13
 
-- | 22 bit GOT entry shifted 
#num R_SPARC_GOT22
 
-- | PC relative 10 bit truncated 
#num R_SPARC_PC10
 
-- | PC relative 22 bit shifted 
#num R_SPARC_PC22
 
-- | 30 bit PC relative PLT address 
#num R_SPARC_WPLT30
 
-- | Copy symbol at runtime 
#num R_SPARC_COPY
 
-- | Create GOT entry 
#num R_SPARC_GLOB_DAT
 
-- | Create PLT entry 
#num R_SPARC_JMP_SLOT
 
-- | Adjust by program base 
#num R_SPARC_RELATIVE
 
-- | Direct 32 bit unaligned 
#num R_SPARC_UA32
 
-- | Direct 32 bit ref to PLT entry 
#num R_SPARC_PLT32
 
-- | High 22 bit PLT entry 
#num R_SPARC_HIPLT22
 
-- | Truncated 10 bit PLT entry 
#num R_SPARC_LOPLT10
 
-- | PC rel 32 bit ref to PLT entry 
#num R_SPARC_PCPLT32
 
-- | PC rel high 22 bit PLT entry 
#num R_SPARC_PCPLT22
 
-- | PC rel trunc 10 bit PLT entry 
#num R_SPARC_PCPLT10
 
-- | Direct 10 bit 
#num R_SPARC_10
 
-- | Direct 11 bit 
#num R_SPARC_11
 
-- | Direct 64 bit 
#num R_SPARC_64
 
-- | 10bit with secondary 13bit addend 
#num R_SPARC_OLO10
 
-- | Top 22 bits of direct 64 bit 
#num R_SPARC_HH22
 
-- | High middle 10 bits of ... 
#num R_SPARC_HM10
 
-- | Low middle 22 bits of ... 
#num R_SPARC_LM22
 
-- | Top 22 bits of pc rel 64 bit 
#num R_SPARC_PC_HH22
 
-- | High middle 10 bit of ... 
#num R_SPARC_PC_HM10
 
-- | Low miggle 22 bits of ... 
#num R_SPARC_PC_LM22
 
-- | PC relative 16 bit shifted 
#num R_SPARC_WDISP16
 
-- | PC relative 19 bit shifted 
#num R_SPARC_WDISP19
 
-- | was part of v9 ABI but was removed 
#num R_SPARC_GLOB_JMP
 
-- | Direct 7 bit 
#num R_SPARC_7
 
-- | Direct 5 bit 
#num R_SPARC_5
 
-- | Direct 6 bit 
#num R_SPARC_6
 
-- | PC relative 64 bit 
#num R_SPARC_DISP64
 
-- | Direct 64 bit ref to PLT entry 
#num R_SPARC_PLT64
 
-- | High 22 bit complemented 
#num R_SPARC_HIX22
 
-- | Truncated 11 bit complemented 
#num R_SPARC_LOX10
 
-- | Direct high 12 of 44 bit 
#num R_SPARC_H44
 
-- | Direct mid 22 of 44 bit 
#num R_SPARC_M44
 
-- | Direct low 10 of 44 bit 
#num R_SPARC_L44
 
-- | Global register usage 
#num R_SPARC_REGISTER
 
-- | Direct 64 bit unaligned 
#num R_SPARC_UA64
 
-- | Direct 16 bit unaligned 
#num R_SPARC_UA16
#num R_SPARC_TLS_GD_HI22
#num R_SPARC_TLS_GD_LO10
#num R_SPARC_TLS_GD_ADD
#num R_SPARC_TLS_GD_CALL
#num R_SPARC_TLS_LDM_HI22
#num R_SPARC_TLS_LDM_LO10
#num R_SPARC_TLS_LDM_ADD
#num R_SPARC_TLS_LDM_CALL
#num R_SPARC_TLS_LDO_HIX22
#num R_SPARC_TLS_LDO_LOX10
#num R_SPARC_TLS_LDO_ADD
#num R_SPARC_TLS_IE_HI22
#num R_SPARC_TLS_IE_LO10
#num R_SPARC_TLS_IE_LD
#num R_SPARC_TLS_IE_LDX
#num R_SPARC_TLS_IE_ADD
#num R_SPARC_TLS_LE_HIX22
#num R_SPARC_TLS_LE_LOX10
#num R_SPARC_TLS_DTPMOD32
#num R_SPARC_TLS_DTPMOD64
#num R_SPARC_TLS_DTPOFF32
#num R_SPARC_TLS_DTPOFF64
#num R_SPARC_TLS_TPOFF32
#num R_SPARC_TLS_TPOFF64
#num R_SPARC_GOTDATA_HIX22
#num R_SPARC_GOTDATA_LOX10
#num R_SPARC_GOTDATA_OP_HIX22
#num R_SPARC_GOTDATA_OP_LOX10
#num R_SPARC_GOTDATA_OP
#num R_SPARC_H34
#num R_SPARC_SIZE32
#num R_SPARC_SIZE64
#num R_SPARC_WDISP10
#num R_SPARC_JMP_IREL
#num R_SPARC_IRELATIVE
#num R_SPARC_GNU_VTINHERIT
#num R_SPARC_GNU_VTENTRY
#num R_SPARC_REV32
#num R_SPARC_NUM
#num DT_SPARC_REGISTER
#num DT_SPARC_NUM
 
-- | A .noreorder directive was used.  
#num EF_MIPS_NOREORDER
 
-- | Contains PIC code.  
#num EF_MIPS_PIC
 
-- | Uses PIC calling sequence.  
#num EF_MIPS_CPIC
#num EF_MIPS_XGOT
#num EF_MIPS_64BIT_WHIRL
#num EF_MIPS_ABI2
#num EF_MIPS_ABI_ON32
 
-- | Uses IEEE 754-2008 NaN encoding.  
#num EF_MIPS_NAN2008
 
-- | MIPS architecture level.  
#num EF_MIPS_ARCH
 
-- | -mips1 code.  
#num EF_MIPS_ARCH_1
 
-- | -mips2 code.  
#num EF_MIPS_ARCH_2
 
-- | -mips3 code.  
#num EF_MIPS_ARCH_3
 
-- | -mips4 code.  
#num EF_MIPS_ARCH_4
 
-- | -mips5 code.  
#num EF_MIPS_ARCH_5
 
-- | MIPS32 code.  
#num EF_MIPS_ARCH_32
 
-- | MIPS64 code.  
#num EF_MIPS_ARCH_64
 
-- | MIPS32r2 code.  
#num EF_MIPS_ARCH_32R2
 
-- | MIPS64r2 code.  
#num EF_MIPS_ARCH_64R2
#num E_MIPS_ARCH_1
#num E_MIPS_ARCH_2
#num E_MIPS_ARCH_3
#num E_MIPS_ARCH_4
#num E_MIPS_ARCH_5
#num E_MIPS_ARCH_32
#num E_MIPS_ARCH_64
 
-- | Allocated common symbols.  
#num SHN_MIPS_ACOMMON
 
-- | Allocated test symbols.  
#num SHN_MIPS_TEXT
 
-- | Allocated data symbols.  
#num SHN_MIPS_DATA
 
-- | Small common symbols.  
#num SHN_MIPS_SCOMMON
 
-- | Small undefined symbols.  
#num SHN_MIPS_SUNDEFINED
 
-- | Shared objects used in link.  
#num SHT_MIPS_LIBLIST
#num SHT_MIPS_MSYM
 
-- | Conflicting symbols.  
#num SHT_MIPS_CONFLICT
 
-- | Global data area sizes.  
#num SHT_MIPS_GPTAB
 
-- | Reserved for SGI/MIPS compilers 
#num SHT_MIPS_UCODE
 
-- | MIPS ECOFF debugging info.  
#num SHT_MIPS_DEBUG
 
-- | Register usage information.  
#num SHT_MIPS_REGINFO
#num SHT_MIPS_PACKAGE
#num SHT_MIPS_PACKSYM
#num SHT_MIPS_RELD
#num SHT_MIPS_IFACE
#num SHT_MIPS_CONTENT
 
-- | Miscellaneous options.  
#num SHT_MIPS_OPTIONS
#num SHT_MIPS_SHDR
#num SHT_MIPS_FDESC
#num SHT_MIPS_EXTSYM
#num SHT_MIPS_DENSE
#num SHT_MIPS_PDESC
#num SHT_MIPS_LOCSYM
#num SHT_MIPS_AUXSYM
#num SHT_MIPS_OPTSYM
#num SHT_MIPS_LOCSTR
#num SHT_MIPS_LINE
#num SHT_MIPS_RFDESC
#num SHT_MIPS_DELTASYM
#num SHT_MIPS_DELTAINST
#num SHT_MIPS_DELTACLASS
 
-- | DWARF debugging information.  
#num SHT_MIPS_DWARF
#num SHT_MIPS_DELTADECL
#num SHT_MIPS_SYMBOL_LIB
 
-- | Event section.  
#num SHT_MIPS_EVENTS
#num SHT_MIPS_TRANSLATE
#num SHT_MIPS_PIXIE
#num SHT_MIPS_XLATE
#num SHT_MIPS_XLATE_DEBUG
#num SHT_MIPS_WHIRL
#num SHT_MIPS_EH_REGION
#num SHT_MIPS_XLATE_OLD
#num SHT_MIPS_PDR_EXCEPTION
 
-- | Must be in global data area.  
#num SHF_MIPS_GPREL
#num SHF_MIPS_MERGE
#num SHF_MIPS_ADDR
#num SHF_MIPS_STRINGS
#num SHF_MIPS_NOSTRIP
#num SHF_MIPS_LOCAL
#num SHF_MIPS_NAMES
#num SHF_MIPS_NODUPE
#num STO_MIPS_DEFAULT
#num STO_MIPS_INTERNAL
#num STO_MIPS_HIDDEN
#num STO_MIPS_PROTECTED
#num STO_MIPS_PLT
#num STO_MIPS_SC_ALIGN_UNUSED
#num STB_MIPS_SPLIT_COMMON
 
-- | Undefined.  
#num ODK_NULL
 
-- | Register usage information.  
#num ODK_REGINFO
 
-- | Exception processing options.  
#num ODK_EXCEPTIONS
 
-- | Section padding options.  
#num ODK_PAD
 
-- | Hardware workarounds performed 
#num ODK_HWPATCH
 
-- | record the fill value used by the linker. 
#num ODK_FILL
 
-- | reserve space for desktop tools to write. 
#num ODK_TAGS
 
-- | HW workarounds.  'AND' bits when merging. 
#num ODK_HWAND
 
-- | HW workarounds.  'OR' bits when merging.  
#num ODK_HWOR
 
-- | FPE's which MUST be enabled.  
#num OEX_FPU_MIN
 
-- | FPE's which MAY be enabled.  
#num OEX_FPU_MAX
 
-- | page zero must be mapped.  
#num OEX_PAGE0
 
-- | Force sequential memory mode?  
#num OEX_SMM
 
-- | Force floating point debug mode?  
#num OEX_FPDBUG
#num OEX_PRECISEFP
 
-- | Dismiss invalid address faults?  
#num OEX_DISMISS
#num OEX_FPU_INVAL
#num OEX_FPU_DIV0
#num OEX_FPU_OFLO
#num OEX_FPU_UFLO
#num OEX_FPU_INEX
 
-- | R4000 end-of-page patch.  
#num OHW_R4KEOP
 
-- | may need R8000 prefetch patch.  
#num OHW_R8KPFETCH
 
-- | R5000 end-of-page patch.  
#num OHW_R5KEOP
 
-- | R5000 cvt.[ds].l bug.  clean=1.  
#num OHW_R5KCVTL
#num OPAD_PREFIX
#num OPAD_POSTFIX
#num OPAD_SYMBOL
#num OHWA0_R4KEOP_CHECKED
#num OHWA1_R4KEOP_CLEAN
 
-- | No reloc 
#num R_MIPS_NONE
 
-- | Direct 16 bit 
#num R_MIPS_16
 
-- | Direct 32 bit 
#num R_MIPS_32
 
-- | PC relative 32 bit 
#num R_MIPS_REL32
 
-- | Direct 26 bit shifted 
#num R_MIPS_26
 
-- | High 16 bit 
#num R_MIPS_HI16
 
-- | Low 16 bit 
#num R_MIPS_LO16
 
-- | GP relative 16 bit 
#num R_MIPS_GPREL16
 
-- | 16 bit literal entry 
#num R_MIPS_LITERAL
 
-- | 16 bit GOT entry 
#num R_MIPS_GOT16
 
-- | PC relative 16 bit 
#num R_MIPS_PC16
 
-- | 16 bit GOT entry for function 
#num R_MIPS_CALL16
 
-- | GP relative 32 bit 
#num R_MIPS_GPREL32
#num R_MIPS_SHIFT5
#num R_MIPS_SHIFT6
#num R_MIPS_64
#num R_MIPS_GOT_DISP
#num R_MIPS_GOT_PAGE
#num R_MIPS_GOT_OFST
#num R_MIPS_GOT_HI16
#num R_MIPS_GOT_LO16
#num R_MIPS_SUB
#num R_MIPS_INSERT_A
#num R_MIPS_INSERT_B
#num R_MIPS_DELETE
#num R_MIPS_HIGHER
#num R_MIPS_HIGHEST
#num R_MIPS_CALL_HI16
#num R_MIPS_CALL_LO16
#num R_MIPS_SCN_DISP
#num R_MIPS_REL16
#num R_MIPS_ADD_IMMEDIATE
#num R_MIPS_PJUMP
#num R_MIPS_RELGOT
#num R_MIPS_JALR
 
-- | Module number 32 bit 
#num R_MIPS_TLS_DTPMOD32
 
-- | Module-relative offset 32 bit 
#num R_MIPS_TLS_DTPREL32
 
-- | Module number 64 bit 
#num R_MIPS_TLS_DTPMOD64
 
-- | Module-relative offset 64 bit 
#num R_MIPS_TLS_DTPREL64
 
-- | 16 bit GOT offset for GD 
#num R_MIPS_TLS_GD
 
-- | 16 bit GOT offset for LDM 
#num R_MIPS_TLS_LDM
 
-- | Module-relative offset, high 16 bits 
#num R_MIPS_TLS_DTPREL_HI16
 
-- | Module-relative offset, low 16 bits 
#num R_MIPS_TLS_DTPREL_LO16
 
-- | 16 bit GOT offset for IE 
#num R_MIPS_TLS_GOTTPREL
 
-- | TP-relative offset, 32 bit 
#num R_MIPS_TLS_TPREL32
 
-- | TP-relative offset, 64 bit 
#num R_MIPS_TLS_TPREL64
 
-- | TP-relative offset, high 16 bits 
#num R_MIPS_TLS_TPREL_HI16
 
-- | TP-relative offset, low 16 bits 
#num R_MIPS_TLS_TPREL_LO16
#num R_MIPS_GLOB_DAT
#num R_MIPS_COPY
#num R_MIPS_JUMP_SLOT
#num R_MIPS_NUM
 
-- | Register usage information 
#num PT_MIPS_REGINFO
 
-- | Runtime procedure table. 
#num PT_MIPS_RTPROC
#num PT_MIPS_OPTIONS
#num PF_MIPS_LOCAL
 
-- | Runtime linker interface version 
#num DT_MIPS_RLD_VERSION
 
-- | Timestamp 
#num DT_MIPS_TIME_STAMP
 
-- | Checksum 
#num DT_MIPS_ICHECKSUM
 
-- | Version string (string tbl index) 
#num DT_MIPS_IVERSION
 
-- | Flags 
#num DT_MIPS_FLAGS
 
-- | Base address 
#num DT_MIPS_BASE_ADDRESS
#num DT_MIPS_MSYM
 
-- | Address of CONFLICT section 
#num DT_MIPS_CONFLICT
 
-- | Address of LIBLIST section 
#num DT_MIPS_LIBLIST
 
-- | Number of local GOT entries 
#num DT_MIPS_LOCAL_GOTNO
 
-- | Number of CONFLICT entries 
#num DT_MIPS_CONFLICTNO
 
-- | Number of LIBLIST entries 
#num DT_MIPS_LIBLISTNO
 
-- | Number of DYNSYM entries 
#num DT_MIPS_SYMTABNO
 
-- | First external DYNSYM 
#num DT_MIPS_UNREFEXTNO
 
-- | First GOT entry in DYNSYM 
#num DT_MIPS_GOTSYM
 
-- | Number of GOT page table entries 
#num DT_MIPS_HIPAGENO
 
-- | Address of run time loader map.  
#num DT_MIPS_RLD_MAP
 
-- | Delta C++ class definition.  
#num DT_MIPS_DELTA_CLASS
 
-- | Number of entries in
#num DT_MIPS_DELTA_CLASS_NO
 
-- | Delta C++ class instances.  
#num DT_MIPS_DELTA_INSTANCE
 
-- | Number of entries in
#num DT_MIPS_DELTA_INSTANCE_NO
 
-- | Delta relocations.  
#num DT_MIPS_DELTA_RELOC
 
-- | Number of entries in
#num DT_MIPS_DELTA_RELOC_NO
 
-- | Delta symbols that Delta
#num DT_MIPS_DELTA_SYM
 
-- | Number of entries in
#num DT_MIPS_DELTA_SYM_NO
 
-- | Delta symbols that hold the
#num DT_MIPS_DELTA_CLASSSYM
 
-- | Number of entries in
#num DT_MIPS_DELTA_CLASSSYM_NO
 
-- | Flags indicating for C++ flavor.  
#num DT_MIPS_CXX_FLAGS
#num DT_MIPS_PIXIE_INIT
#num DT_MIPS_SYMBOL_LIB
#num DT_MIPS_LOCALPAGE_GOTIDX
#num DT_MIPS_LOCAL_GOTIDX
#num DT_MIPS_HIDDEN_GOTIDX
#num DT_MIPS_PROTECTED_GOTIDX
 
-- | Address of .options.  
#num DT_MIPS_OPTIONS
 
-- | Address of .interface.  
#num DT_MIPS_INTERFACE
#num DT_MIPS_DYNSTR_ALIGN
 
-- | Size of the .interface section. 
#num DT_MIPS_INTERFACE_SIZE
 
-- | Address of rld_text_rsolve
#num DT_MIPS_RLD_TEXT_RESOLVE_ADDR
 
-- | Default suffix of dso to be added
#num DT_MIPS_PERF_SUFFIX
 
-- | (O32)Size of compact rel section. 
#num DT_MIPS_COMPACT_SIZE
 
-- | GP value for aux GOTs.  
#num DT_MIPS_GP_VALUE
 
-- | Address of aux .dynamic.  
#num DT_MIPS_AUX_DYNAMIC
#num DT_MIPS_PLTGOT
#num DT_MIPS_RWPLT
#num DT_MIPS_NUM
 
-- | No flags 
#num RHF_NONE
 
-- | Use quickstart 
#num RHF_QUICKSTART
 
-- | Hash size not power of 2 
#num RHF_NOTPOT
 
-- | Ignore LD_LIBRARY_PATH 
#num RHF_NO_LIBRARY_REPLACEMENT
#num RHF_NO_MOVE
#num RHF_SGI_ONLY
#num RHF_GUARANTEE_INIT
#num RHF_DELTA_C_PLUS_PLUS
#num RHF_GUARANTEE_START_INIT
#num RHF_PIXIE
#num RHF_DEFAULT_DELAY_LOAD
#num RHF_REQUICKSTART
#num RHF_REQUICKSTARTED
#num RHF_CORD
#num RHF_NO_UNRES_UNDEF
#num RHF_RLD_ORDER_SAFE
#num LL_NONE
 
-- | Require exact match 
#num LL_EXACT_MATCH
 
-- | Ignore interface version 
#num LL_IGNORE_INT_VER
#num LL_REQUIRE_MINOR
#num LL_EXPORTS
#num LL_DELAY_LOAD
#num LL_DELTA
 
-- | Trap nil pointer dereference.  
#num EF_PARISC_TRAPNIL
 
-- | Program uses arch. extensions. 
#num EF_PARISC_EXT
 
-- | Program expects little endian. 
#num EF_PARISC_LSB
 
-- | Program expects wide mode.  
#num EF_PARISC_WIDE
 
-- | No kernel assisted branch
#num EF_PARISC_NO_KABP
 
-- | Allow lazy swapping.  
#num EF_PARISC_LAZYSWAP
 
-- | Architecture version.  
#num EF_PARISC_ARCH
 
-- | PA-RISC 1.0 big-endian.  
#num EFA_PARISC_1_0
 
-- | PA-RISC 1.1 big-endian.  
#num EFA_PARISC_1_1
 
-- | PA-RISC 2.0 big-endian.  
#num EFA_PARISC_2_0
 
-- | Section for tenatively declared
#num SHN_PARISC_ANSI_COMMON
 
-- | Common blocks in huge model.  
#num SHN_PARISC_HUGE_COMMON
 
-- | Contains product specific ext. 
#num SHT_PARISC_EXT
 
-- | Unwind information.  
#num SHT_PARISC_UNWIND
 
-- | Debug info for optimized code. 
#num SHT_PARISC_DOC
 
-- | Section with short addressing. 
#num SHF_PARISC_SHORT
 
-- | Section far from gp.  
#num SHF_PARISC_HUGE
 
-- | Static branch prediction code. 
#num SHF_PARISC_SBP
 
-- | Millicode function entry point.  
#num STT_PARISC_MILLICODE
#num STT_HP_OPAQUE
#num STT_HP_STUB
 
-- | No reloc.  
#num R_PARISC_NONE
 
-- | Direct 32-bit reference.  
#num R_PARISC_DIR32
 
-- | Left 21 bits of eff. address.  
#num R_PARISC_DIR21L
 
-- | Right 17 bits of eff. address.  
#num R_PARISC_DIR17R
 
-- | 17 bits of eff. address.  
#num R_PARISC_DIR17F
 
-- | Right 14 bits of eff. address.  
#num R_PARISC_DIR14R
 
-- | 32-bit rel. address.  
#num R_PARISC_PCREL32
 
-- | Left 21 bits of rel. address.  
#num R_PARISC_PCREL21L
 
-- | Right 17 bits of rel. address.  
#num R_PARISC_PCREL17R
 
-- | 17 bits of rel. address.  
#num R_PARISC_PCREL17F
 
-- | Right 14 bits of rel. address.  
#num R_PARISC_PCREL14R
 
-- | Left 21 bits of rel. address.  
#num R_PARISC_DPREL21L
 
-- | Right 14 bits of rel. address.  
#num R_PARISC_DPREL14R
 
-- | GP-relative, left 21 bits.  
#num R_PARISC_GPREL21L
 
-- | GP-relative, right 14 bits.  
#num R_PARISC_GPREL14R
 
-- | LT-relative, left 21 bits.  
#num R_PARISC_LTOFF21L
 
-- | LT-relative, right 14 bits.  
#num R_PARISC_LTOFF14R
 
-- | 32 bits section rel. address.  
#num R_PARISC_SECREL32
 
-- | No relocation, set segment base.  
#num R_PARISC_SEGBASE
 
-- | 32 bits segment rel. address.  
#num R_PARISC_SEGREL32
 
-- | PLT rel. address, left 21 bits.  
#num R_PARISC_PLTOFF21L
 
-- | PLT rel. address, right 14 bits.  
#num R_PARISC_PLTOFF14R
 
-- | 32 bits LT-rel. function pointer. 
#num R_PARISC_LTOFF_FPTR32
 
-- | LT-rel. fct ptr, left 21 bits. 
#num R_PARISC_LTOFF_FPTR21L
 
-- | LT-rel. fct ptr, right 14 bits. 
#num R_PARISC_LTOFF_FPTR14R
 
-- | 64 bits function address.  
#num R_PARISC_FPTR64
 
-- | 32 bits function address.  
#num R_PARISC_PLABEL32
 
-- | Left 21 bits of fdesc address.  
#num R_PARISC_PLABEL21L
 
-- | Right 14 bits of fdesc address.  
#num R_PARISC_PLABEL14R
 
-- | 64 bits PC-rel. address.  
#num R_PARISC_PCREL64
 
-- | 22 bits PC-rel. address.  
#num R_PARISC_PCREL22F
 
-- | PC-rel. address, right 14 bits.  
#num R_PARISC_PCREL14WR
 
-- | PC rel. address, right 14 bits.  
#num R_PARISC_PCREL14DR
 
-- | 16 bits PC-rel. address.  
#num R_PARISC_PCREL16F
 
-- | 16 bits PC-rel. address.  
#num R_PARISC_PCREL16WF
 
-- | 16 bits PC-rel. address.  
#num R_PARISC_PCREL16DF
 
-- | 64 bits of eff. address.  
#num R_PARISC_DIR64
 
-- | 14 bits of eff. address.  
#num R_PARISC_DIR14WR
 
-- | 14 bits of eff. address.  
#num R_PARISC_DIR14DR
 
-- | 16 bits of eff. address.  
#num R_PARISC_DIR16F
 
-- | 16 bits of eff. address.  
#num R_PARISC_DIR16WF
 
-- | 16 bits of eff. address.  
#num R_PARISC_DIR16DF
 
-- | 64 bits of GP-rel. address.  
#num R_PARISC_GPREL64
 
-- | GP-rel. address, right 14 bits.  
#num R_PARISC_GPREL14WR
 
-- | GP-rel. address, right 14 bits.  
#num R_PARISC_GPREL14DR
 
-- | 16 bits GP-rel. address.  
#num R_PARISC_GPREL16F
 
-- | 16 bits GP-rel. address.  
#num R_PARISC_GPREL16WF
 
-- | 16 bits GP-rel. address.  
#num R_PARISC_GPREL16DF
 
-- | 64 bits LT-rel. address.  
#num R_PARISC_LTOFF64
 
-- | LT-rel. address, right 14 bits.  
#num R_PARISC_LTOFF14WR
 
-- | LT-rel. address, right 14 bits.  
#num R_PARISC_LTOFF14DR
 
-- | 16 bits LT-rel. address.  
#num R_PARISC_LTOFF16F
 
-- | 16 bits LT-rel. address.  
#num R_PARISC_LTOFF16WF
 
-- | 16 bits LT-rel. address.  
#num R_PARISC_LTOFF16DF
 
-- | 64 bits section rel. address.  
#num R_PARISC_SECREL64
 
-- | 64 bits segment rel. address.  
#num R_PARISC_SEGREL64
 
-- | PLT-rel. address, right 14 bits.  
#num R_PARISC_PLTOFF14WR
 
-- | PLT-rel. address, right 14 bits.  
#num R_PARISC_PLTOFF14DR
 
-- | 16 bits LT-rel. address.  
#num R_PARISC_PLTOFF16F
 
-- | 16 bits PLT-rel. address.  
#num R_PARISC_PLTOFF16WF
 
-- | 16 bits PLT-rel. address.  
#num R_PARISC_PLTOFF16DF
 
-- | 64 bits LT-rel. function ptr.  
#num R_PARISC_LTOFF_FPTR64
 
-- | LT-rel. fct. ptr., right 14 bits. 
#num R_PARISC_LTOFF_FPTR14WR
 
-- | LT-rel. fct. ptr., right 14 bits. 
#num R_PARISC_LTOFF_FPTR14DR
 
-- | 16 bits LT-rel. function ptr.  
#num R_PARISC_LTOFF_FPTR16F
 
-- | 16 bits LT-rel. function ptr.  
#num R_PARISC_LTOFF_FPTR16WF
 
-- | 16 bits LT-rel. function ptr.  
#num R_PARISC_LTOFF_FPTR16DF
#num R_PARISC_LORESERVE
 
-- | Copy relocation.  
#num R_PARISC_COPY
 
-- | Dynamic reloc, imported PLT 
#num R_PARISC_IPLT
 
-- | Dynamic reloc, exported PLT 
#num R_PARISC_EPLT
 
-- | 32 bits TP-rel. address.  
#num R_PARISC_TPREL32
 
-- | TP-rel. address, left 21 bits.  
#num R_PARISC_TPREL21L
 
-- | TP-rel. address, right 14 bits.  
#num R_PARISC_TPREL14R
 
-- | LT-TP-rel. address, left 21 bits. 
#num R_PARISC_LTOFF_TP21L
 
-- | LT-TP-rel. address, right 14 bits.
#num R_PARISC_LTOFF_TP14R
 
-- | 14 bits LT-TP-rel. address.  
#num R_PARISC_LTOFF_TP14F
 
-- | 64 bits TP-rel. address.  
#num R_PARISC_TPREL64
 
-- | TP-rel. address, right 14 bits.  
#num R_PARISC_TPREL14WR
 
-- | TP-rel. address, right 14 bits.  
#num R_PARISC_TPREL14DR
 
-- | 16 bits TP-rel. address.  
#num R_PARISC_TPREL16F
 
-- | 16 bits TP-rel. address.  
#num R_PARISC_TPREL16WF
 
-- | 16 bits TP-rel. address.  
#num R_PARISC_TPREL16DF
 
-- | 64 bits LT-TP-rel. address.  
#num R_PARISC_LTOFF_TP64
 
-- | LT-TP-rel. address, right 14 bits.
#num R_PARISC_LTOFF_TP14WR
 
-- | LT-TP-rel. address, right 14 bits.
#num R_PARISC_LTOFF_TP14DR
 
-- | 16 bits LT-TP-rel. address.  
#num R_PARISC_LTOFF_TP16F
 
-- | 16 bits LT-TP-rel. address.  
#num R_PARISC_LTOFF_TP16WF
 
-- | 16 bits LT-TP-rel. address.  
#num R_PARISC_LTOFF_TP16DF
#num R_PARISC_GNU_VTENTRY
#num R_PARISC_GNU_VTINHERIT
 
-- | GD 21-bit left.  
#num R_PARISC_TLS_GD21L
 
-- | GD 14-bit right.  
#num R_PARISC_TLS_GD14R
 
-- | GD call to __t_g_a.  
#num R_PARISC_TLS_GDCALL
 
-- | LD module 21-bit left.  
#num R_PARISC_TLS_LDM21L
 
-- | LD module 14-bit right.  
#num R_PARISC_TLS_LDM14R
 
-- | LD module call to __t_g_a.  
#num R_PARISC_TLS_LDMCALL
 
-- | LD offset 21-bit left.  
#num R_PARISC_TLS_LDO21L
 
-- | LD offset 14-bit right.  
#num R_PARISC_TLS_LDO14R
 
-- | DTP module 32-bit.  
#num R_PARISC_TLS_DTPMOD32
 
-- | DTP module 64-bit.  
#num R_PARISC_TLS_DTPMOD64
 
-- | DTP offset 32-bit.  
#num R_PARISC_TLS_DTPOFF32
 
-- | DTP offset 32-bit.  
#num R_PARISC_TLS_DTPOFF64
#num R_PARISC_TLS_LE21L
#num R_PARISC_TLS_LE14R
#num R_PARISC_TLS_IE21L
#num R_PARISC_TLS_IE14R
#num R_PARISC_TLS_TPREL32
#num R_PARISC_TLS_TPREL64
#num R_PARISC_HIRESERVE
#num PT_HP_TLS
#num PT_HP_CORE_NONE
#num PT_HP_CORE_VERSION
#num PT_HP_CORE_KERNEL
#num PT_HP_CORE_COMM
#num PT_HP_CORE_PROC
#num PT_HP_CORE_LOADABLE
#num PT_HP_CORE_STACK
#num PT_HP_CORE_SHM
#num PT_HP_CORE_MMF
#num PT_HP_PARALLEL
#num PT_HP_FASTBIND
#num PT_HP_OPT_ANNOT
#num PT_HP_HSL_ANNOT
#num PT_HP_STACK
#num PT_PARISC_ARCHEXT
#num PT_PARISC_UNWIND
#num PF_PARISC_SBP
#num PF_HP_PAGE_SIZE
#num PF_HP_FAR_SHARED
#num PF_HP_NEAR_SHARED
#num PF_HP_CODE
#num PF_HP_MODIFY
#num PF_HP_LAZYSWAP
#num PF_HP_SBP
 
-- | All addresses must be < 2GB.  
#num EF_ALPHA_32BIT
 
-- | Relocations for relaxing exist.  
#num EF_ALPHA_CANRELAX
#num SHT_ALPHA_DEBUG
#num SHT_ALPHA_REGINFO
#num SHF_ALPHA_GPREL
 
-- | No PV required.  
#num STO_ALPHA_NOPV
 
-- | PV only used for initial ldgp.  
#num STO_ALPHA_STD_GPLOAD
 
-- | No reloc 
#num R_ALPHA_NONE
 
-- | Direct 32 bit 
#num R_ALPHA_REFLONG
 
-- | Direct 64 bit 
#num R_ALPHA_REFQUAD
 
-- | GP relative 32 bit 
#num R_ALPHA_GPREL32
 
-- | GP relative 16 bit w/optimization 
#num R_ALPHA_LITERAL
 
-- | Optimization hint for LITERAL 
#num R_ALPHA_LITUSE
 
-- | Add displacement to GP 
#num R_ALPHA_GPDISP
 
-- | PC+4 relative 23 bit shifted 
#num R_ALPHA_BRADDR
 
-- | PC+4 relative 16 bit shifted 
#num R_ALPHA_HINT
 
-- | PC relative 16 bit 
#num R_ALPHA_SREL16
 
-- | PC relative 32 bit 
#num R_ALPHA_SREL32
 
-- | PC relative 64 bit 
#num R_ALPHA_SREL64
 
-- | GP relative 32 bit, high 16 bits 
#num R_ALPHA_GPRELHIGH
 
-- | GP relative 32 bit, low 16 bits 
#num R_ALPHA_GPRELLOW
 
-- | GP relative 16 bit 
#num R_ALPHA_GPREL16
 
-- | Copy symbol at runtime 
#num R_ALPHA_COPY
 
-- | Create GOT entry 
#num R_ALPHA_GLOB_DAT
 
-- | Create PLT entry 
#num R_ALPHA_JMP_SLOT
 
-- | Adjust by program base 
#num R_ALPHA_RELATIVE
#num R_ALPHA_TLS_GD_HI
#num R_ALPHA_TLSGD
#num R_ALPHA_TLS_LDM
#num R_ALPHA_DTPMOD64
#num R_ALPHA_GOTDTPREL
#num R_ALPHA_DTPREL64
#num R_ALPHA_DTPRELHI
#num R_ALPHA_DTPRELLO
#num R_ALPHA_DTPREL16
#num R_ALPHA_GOTTPREL
#num R_ALPHA_TPREL64
#num R_ALPHA_TPRELHI
#num R_ALPHA_TPRELLO
#num R_ALPHA_TPREL16
#num R_ALPHA_NUM
#num LITUSE_ALPHA_ADDR
#num LITUSE_ALPHA_BASE
#num LITUSE_ALPHA_BYTOFF
#num LITUSE_ALPHA_JSR
#num LITUSE_ALPHA_TLS_GD
#num LITUSE_ALPHA_TLS_LDM
#num DT_ALPHA_PLTRO
#num DT_ALPHA_NUM
 
-- | PowerPC embedded flag 
#num EF_PPC_EMB
 
-- | PowerPC -mrelocatable flag
#num EF_PPC_RELOCATABLE
 
-- | PowerPC -mrelocatable-lib
#num EF_PPC_RELOCATABLE_LIB
 
-- | PowerPC relocations defined by the ABIs 
 
#num R_PPC_NONE
 
-- | 32bit absolute address 
#num R_PPC_ADDR32
 
-- | 26bit address, 2 bits ignored.  
#num R_PPC_ADDR24
 
-- | 16bit absolute address 
#num R_PPC_ADDR16
 
-- | lower 16bit of absolute address 
#num R_PPC_ADDR16_LO
 
-- | high 16bit of absolute address 
#num R_PPC_ADDR16_HI
 
-- | adjusted high 16bit 
#num R_PPC_ADDR16_HA
 
-- | 16bit address, 2 bits ignored 
#num R_PPC_ADDR14
#num R_PPC_ADDR14_BRTAKEN
#num R_PPC_ADDR14_BRNTAKEN
 
-- | PC relative 26 bit 
#num R_PPC_REL24
 
-- | PC relative 16 bit 
#num R_PPC_REL14
#num R_PPC_REL14_BRTAKEN
#num R_PPC_REL14_BRNTAKEN
#num R_PPC_GOT16
#num R_PPC_GOT16_LO
#num R_PPC_GOT16_HI
#num R_PPC_GOT16_HA
#num R_PPC_PLTREL24
#num R_PPC_COPY
#num R_PPC_GLOB_DAT
#num R_PPC_JMP_SLOT
#num R_PPC_RELATIVE
#num R_PPC_LOCAL24PC
#num R_PPC_UADDR32
#num R_PPC_UADDR16
#num R_PPC_REL32
#num R_PPC_PLT32
#num R_PPC_PLTREL32
#num R_PPC_PLT16_LO
#num R_PPC_PLT16_HI
#num R_PPC_PLT16_HA
#num R_PPC_SDAREL16
#num R_PPC_SECTOFF
#num R_PPC_SECTOFF_LO
#num R_PPC_SECTOFF_HI
#num R_PPC_SECTOFF_HA
 
-- | PowerPC relocations defined for the TLS access ABI.  
 
 
-- | none	(sym+add)@tls 
#num R_PPC_TLS
 
-- | word32	(sym+add)@dtpmod 
#num R_PPC_DTPMOD32
 
-- | half16*	(sym+add)@tprel 
#num R_PPC_TPREL16
 
-- | half16	(sym+add)@tprel@l 
#num R_PPC_TPREL16_LO
 
-- | half16	(sym+add)@tprel@h 
#num R_PPC_TPREL16_HI
 
-- | half16	(sym+add)@tprel@ha 
#num R_PPC_TPREL16_HA
 
-- | word32	(sym+add)@tprel 
#num R_PPC_TPREL32
 
-- | half16*	(sym+add)@dtprel 
#num R_PPC_DTPREL16
 
-- | half16	(sym+add)@dtprel@l 
#num R_PPC_DTPREL16_LO
 
-- | half16	(sym+add)@dtprel@h 
#num R_PPC_DTPREL16_HI
 
-- | half16	(sym+add)@dtprel@ha 
#num R_PPC_DTPREL16_HA
 
-- | word32	(sym+add)@dtprel 
#num R_PPC_DTPREL32
 
-- | half16*	(sym+add)@got@tlsgd 
#num R_PPC_GOT_TLSGD16
 
-- | half16	(sym+add)@got@tlsgd@l 
#num R_PPC_GOT_TLSGD16_LO
 
-- | half16	(sym+add)@got@tlsgd@h 
#num R_PPC_GOT_TLSGD16_HI
 
-- | half16	(sym+add)@got@tlsgd@ha 
#num R_PPC_GOT_TLSGD16_HA
 
-- | half16*	(sym+add)@got@tlsld 
#num R_PPC_GOT_TLSLD16
 
-- | half16	(sym+add)@got@tlsld@l 
#num R_PPC_GOT_TLSLD16_LO
 
-- | half16	(sym+add)@got@tlsld@h 
#num R_PPC_GOT_TLSLD16_HI
 
-- | half16	(sym+add)@got@tlsld@ha 
#num R_PPC_GOT_TLSLD16_HA
 
-- | half16*	(sym+add)@got@tprel 
#num R_PPC_GOT_TPREL16
 
-- | half16	(sym+add)@got@tprel@l 
#num R_PPC_GOT_TPREL16_LO
 
-- | half16	(sym+add)@got@tprel@h 
#num R_PPC_GOT_TPREL16_HI
 
-- | half16	(sym+add)@got@tprel@ha 
#num R_PPC_GOT_TPREL16_HA
 
-- | half16*	(sym+add)@got@dtprel 
#num R_PPC_GOT_DTPREL16
 
-- | half16*	(sym+add)@got@dtprel@l 
#num R_PPC_GOT_DTPREL16_LO
 
-- | half16*	(sym+add)@got@dtprel@h 
#num R_PPC_GOT_DTPREL16_HI
 
-- | half16*	(sym+add)@got@dtprel@ha 
#num R_PPC_GOT_DTPREL16_HA
#num R_PPC_EMB_NADDR32
#num R_PPC_EMB_NADDR16
#num R_PPC_EMB_NADDR16_LO
#num R_PPC_EMB_NADDR16_HI
#num R_PPC_EMB_NADDR16_HA
#num R_PPC_EMB_SDAI16
#num R_PPC_EMB_SDA2I16
#num R_PPC_EMB_SDA2REL
 
-- | 16 bit offset in SDA 
#num R_PPC_EMB_SDA21
#num R_PPC_EMB_MRKREF
#num R_PPC_EMB_RELSEC16
#num R_PPC_EMB_RELST_LO
#num R_PPC_EMB_RELST_HI
#num R_PPC_EMB_RELST_HA
#num R_PPC_EMB_BIT_FLD
 
-- | 16 bit relative offset in SDA 
#num R_PPC_EMB_RELSDA
 
-- | like EMB_SDA21, but lower 16 bit 
#num R_PPC_DIAB_SDA21_LO
 
-- | like EMB_SDA21, but high 16 bit 
#num R_PPC_DIAB_SDA21_HI
 
-- | like EMB_SDA21, adjusted high 16 
#num R_PPC_DIAB_SDA21_HA
 
-- | like EMB_RELSDA, but lower 16 bit 
#num R_PPC_DIAB_RELSDA_LO
 
-- | like EMB_RELSDA, but high 16 bit 
#num R_PPC_DIAB_RELSDA_HI
 
-- | like EMB_RELSDA, adjusted high 16 
#num R_PPC_DIAB_RELSDA_HA
#num R_PPC_IRELATIVE
 
-- | half16   (sym+add-.) 
#num R_PPC_REL16
 
-- | half16   (sym+add-.)@l 
#num R_PPC_REL16_LO
 
-- | half16   (sym+add-.)@h 
#num R_PPC_REL16_HI
 
-- | half16   (sym+add-.)@ha 
#num R_PPC_REL16_HA
#num R_PPC_TOC16
#num DT_PPC_GOT
#num DT_PPC_NUM
 
-- | PowerPC64 relocations defined by the ABIs 
 
#num R_PPC64_NONE
 
-- | 32bit absolute address 
#num R_PPC64_ADDR32
 
-- | 26bit address, word aligned 
#num R_PPC64_ADDR24
 
-- | 16bit absolute address 
#num R_PPC64_ADDR16
 
-- | lower 16bits of address 
#num R_PPC64_ADDR16_LO
 
-- | high 16bits of address. 
#num R_PPC64_ADDR16_HI
 
-- | adjusted high 16bits.  
#num R_PPC64_ADDR16_HA
 
-- | 16bit address, word aligned 
#num R_PPC64_ADDR14
#num R_PPC64_ADDR14_BRTAKEN
#num R_PPC64_ADDR14_BRNTAKEN
 
-- | PC-rel. 26 bit, word aligned 
#num R_PPC64_REL24
 
-- | PC relative 16 bit 
#num R_PPC64_REL14
#num R_PPC64_REL14_BRTAKEN
#num R_PPC64_REL14_BRNTAKEN
#num R_PPC64_GOT16
#num R_PPC64_GOT16_LO
#num R_PPC64_GOT16_HI
#num R_PPC64_GOT16_HA
#num R_PPC64_COPY
#num R_PPC64_GLOB_DAT
#num R_PPC64_JMP_SLOT
#num R_PPC64_RELATIVE
#num R_PPC64_UADDR32
#num R_PPC64_UADDR16
#num R_PPC64_REL32
#num R_PPC64_PLT32
#num R_PPC64_PLTREL32
#num R_PPC64_PLT16_LO
#num R_PPC64_PLT16_HI
#num R_PPC64_PLT16_HA
#num R_PPC64_SECTOFF
#num R_PPC64_SECTOFF_LO
#num R_PPC64_SECTOFF_HI
#num R_PPC64_SECTOFF_HA
 
-- | word30 (S + A - P) >> 2 
#num R_PPC64_ADDR30
 
-- | doubleword64 S + A 
#num R_PPC64_ADDR64
 
-- | half16 #higher(S + A) 
#num R_PPC64_ADDR16_HIGHER
 
-- | half16 #highera(S + A) 
#num R_PPC64_ADDR16_HIGHERA
 
-- | half16 #highest(S + A) 
#num R_PPC64_ADDR16_HIGHEST
 
-- | half16 #highesta(S + A) 
#num R_PPC64_ADDR16_HIGHESTA
 
-- | doubleword64 S + A 
#num R_PPC64_UADDR64
 
-- | doubleword64 S + A - P 
#num R_PPC64_REL64
 
-- | doubleword64 L + A 
#num R_PPC64_PLT64
 
-- | doubleword64 L + A - P 
#num R_PPC64_PLTREL64
 
-- | half16* S + A - .TOC 
#num R_PPC64_TOC16
 
-- | half16 #lo(S + A - .TOC.) 
#num R_PPC64_TOC16_LO
 
-- | half16 #hi(S + A - .TOC.) 
#num R_PPC64_TOC16_HI
 
-- | half16 #ha(S + A - .TOC.) 
#num R_PPC64_TOC16_HA
 
-- | doubleword64 .TOC 
#num R_PPC64_TOC
 
-- | half16* M + A 
#num R_PPC64_PLTGOT16
 
-- | half16 #lo(M + A) 
#num R_PPC64_PLTGOT16_LO
 
-- | half16 #hi(M + A) 
#num R_PPC64_PLTGOT16_HI
 
-- | half16 #ha(M + A) 
#num R_PPC64_PLTGOT16_HA
 
-- | half16ds* (S + A) >> 2 
#num R_PPC64_ADDR16_DS
 
-- | half16ds  #lo(S + A) >> 2 
#num R_PPC64_ADDR16_LO_DS
 
-- | half16ds* (G + A) >> 2 
#num R_PPC64_GOT16_DS
 
-- | half16ds  #lo(G + A) >> 2 
#num R_PPC64_GOT16_LO_DS
 
-- | half16ds  #lo(L + A) >> 2 
#num R_PPC64_PLT16_LO_DS
 
-- | half16ds* (R + A) >> 2 
#num R_PPC64_SECTOFF_DS
 
-- | half16ds  #lo(R + A) >> 2 
#num R_PPC64_SECTOFF_LO_DS
 
-- | half16ds* (S + A - .TOC.) >> 2 
#num R_PPC64_TOC16_DS
 
-- | half16ds  #lo(S + A - .TOC.) >> 2 
#num R_PPC64_TOC16_LO_DS
 
-- | half16ds* (M + A) >> 2 
#num R_PPC64_PLTGOT16_DS
 
-- | half16ds  #lo(M + A) >> 2 
#num R_PPC64_PLTGOT16_LO_DS
 
-- | PowerPC64 relocations defined for the TLS access ABI.  
 
 
-- | none	(sym+add)@tls 
#num R_PPC64_TLS
 
-- | doubleword64 (sym+add)@dtpmod 
#num R_PPC64_DTPMOD64
 
-- | half16*	(sym+add)@tprel 
#num R_PPC64_TPREL16
 
-- | half16	(sym+add)@tprel@l 
#num R_PPC64_TPREL16_LO
 
-- | half16	(sym+add)@tprel@h 
#num R_PPC64_TPREL16_HI
 
-- | half16	(sym+add)@tprel@ha 
#num R_PPC64_TPREL16_HA
 
-- | doubleword64 (sym+add)@tprel 
#num R_PPC64_TPREL64
 
-- | half16*	(sym+add)@dtprel 
#num R_PPC64_DTPREL16
 
-- | half16	(sym+add)@dtprel@l 
#num R_PPC64_DTPREL16_LO
 
-- | half16	(sym+add)@dtprel@h 
#num R_PPC64_DTPREL16_HI
 
-- | half16	(sym+add)@dtprel@ha 
#num R_PPC64_DTPREL16_HA
 
-- | doubleword64 (sym+add)@dtprel 
#num R_PPC64_DTPREL64
 
-- | half16*	(sym+add)@got@tlsgd 
#num R_PPC64_GOT_TLSGD16
 
-- | half16	(sym+add)@got@tlsgd@l 
#num R_PPC64_GOT_TLSGD16_LO
 
-- | half16	(sym+add)@got@tlsgd@h 
#num R_PPC64_GOT_TLSGD16_HI
 
-- | half16	(sym+add)@got@tlsgd@ha 
#num R_PPC64_GOT_TLSGD16_HA
 
-- | half16*	(sym+add)@got@tlsld 
#num R_PPC64_GOT_TLSLD16
 
-- | half16	(sym+add)@got@tlsld@l 
#num R_PPC64_GOT_TLSLD16_LO
 
-- | half16	(sym+add)@got@tlsld@h 
#num R_PPC64_GOT_TLSLD16_HI
 
-- | half16	(sym+add)@got@tlsld@ha 
#num R_PPC64_GOT_TLSLD16_HA
 
-- | half16ds*	(sym+add)@got@tprel 
#num R_PPC64_GOT_TPREL16_DS
 
-- | half16ds (sym+add)@got@tprel@l 
#num R_PPC64_GOT_TPREL16_LO_DS
 
-- | half16	(sym+add)@got@tprel@h 
#num R_PPC64_GOT_TPREL16_HI
 
-- | half16	(sym+add)@got@tprel@ha 
#num R_PPC64_GOT_TPREL16_HA
 
-- | half16ds*	(sym+add)@got@dtprel 
#num R_PPC64_GOT_DTPREL16_DS
 
-- | half16ds (sym+add)@got@dtprel@l 
#num R_PPC64_GOT_DTPREL16_LO_DS
 
-- | half16	(sym+add)@got@dtprel@h 
#num R_PPC64_GOT_DTPREL16_HI
 
-- | half16	(sym+add)@got@dtprel@ha 
#num R_PPC64_GOT_DTPREL16_HA
 
-- | half16ds*	(sym+add)@tprel 
#num R_PPC64_TPREL16_DS
 
-- | half16ds	(sym+add)@tprel@l 
#num R_PPC64_TPREL16_LO_DS
 
-- | half16	(sym+add)@tprel@higher 
#num R_PPC64_TPREL16_HIGHER
 
-- | half16	(sym+add)@tprel@highera 
#num R_PPC64_TPREL16_HIGHERA
 
-- | half16	(sym+add)@tprel@highest 
#num R_PPC64_TPREL16_HIGHEST
 
-- | half16	(sym+add)@tprel@highesta 
#num R_PPC64_TPREL16_HIGHESTA
 
-- | half16ds* (sym+add)@dtprel 
#num R_PPC64_DTPREL16_DS
 
-- | half16ds	(sym+add)@dtprel@l 
#num R_PPC64_DTPREL16_LO_DS
 
-- | half16	(sym+add)@dtprel@higher 
#num R_PPC64_DTPREL16_HIGHER
 
-- | half16	(sym+add)@dtprel@highera 
#num R_PPC64_DTPREL16_HIGHERA
 
-- | half16	(sym+add)@dtprel@highest 
#num R_PPC64_DTPREL16_HIGHEST
 
-- | half16	(sym+add)@dtprel@highesta 
#num R_PPC64_DTPREL16_HIGHESTA
 
-- | none	(sym+add)@tlsgd 
#num R_PPC64_TLSGD
 
-- | none	(sym+add)@tlsld 
#num R_PPC64_TLSLD
 
-- | none 
#num R_PPC64_TOCSAVE
#num R_PPC64_ADDR16_HIGH
#num R_PPC64_ADDR16_HIGHA
#num R_PPC64_TPREL16_HIGH
#num R_PPC64_TPREL16_HIGHA
#num R_PPC64_DTPREL16_HIGH
#num R_PPC64_DTPREL16_HIGHA
#num R_PPC64_JMP_IREL
#num R_PPC64_IRELATIVE
 
-- | half16   (sym+add-.) 
#num R_PPC64_REL16
 
-- | half16   (sym+add-.)@l 
#num R_PPC64_REL16_LO
 
-- | half16   (sym+add-.)@h 
#num R_PPC64_REL16_HI
 
-- | half16   (sym+add-.)@ha 
#num R_PPC64_REL16_HA
#num EF_PPC64_ABI
#num DT_PPC64_GLINK
#num DT_PPC64_OPD
#num DT_PPC64_OPDSZ
#num DT_PPC64_OPT
#num DT_PPC64_NUM
#num PPC64_OPT_TLS
#num PPC64_OPT_MULTI_TOC
#num STO_PPC64_LOCAL_BIT
#num STO_PPC64_LOCAL_MASK
#num EF_ARM_RELEXEC
#num EF_ARM_HASENTRY
#num EF_ARM_INTERWORK
#num EF_ARM_APCS_26
#num EF_ARM_APCS_FLOAT
#num EF_ARM_PIC
 
-- | 8-bit structure alignment is in use 
#num EF_ARM_ALIGN8
#num EF_ARM_NEW_ABI
#num EF_ARM_OLD_ABI
#num EF_ARM_SOFT_FLOAT
#num EF_ARM_VFP_FLOAT
#num EF_ARM_MAVERICK_FLOAT
 
-- | NB conflicts with EF_ARM_SOFT_FLOAT 
#num EF_ARM_ABI_FLOAT_SOFT
 
-- | NB conflicts with EF_ARM_VFP_FLOAT 
#num EF_ARM_ABI_FLOAT_HARD
 
-- | Other constants defined in the ARM ELF spec. version B-01.  
 
 
-- | NB. These conflict with values defined above.  
 
#num EF_ARM_SYMSARESORTED
#num EF_ARM_DYNSYMSUSESEGIDX
#num EF_ARM_MAPSYMSFIRST
#num EF_ARM_EABIMASK
 
-- | Constants defined in AAELF.  
 
#num EF_ARM_BE8
#num EF_ARM_LE8
#num EF_ARM_EABI_UNKNOWN
#num EF_ARM_EABI_VER1
#num EF_ARM_EABI_VER2
#num EF_ARM_EABI_VER3
#num EF_ARM_EABI_VER4
#num EF_ARM_EABI_VER5
 
-- | A Thumb function.  
#num STT_ARM_TFUNC
 
-- | A Thumb label.  
#num STT_ARM_16BIT
 
-- | Section contains an entry point 
#num SHF_ARM_ENTRYSECT
 
-- | Section may be multiply defined
#num SHF_ARM_COMDEF
 
-- | Segment contains the location
#num PF_ARM_SB
 
-- | Position-independent segment.  
#num PF_ARM_PI
 
-- | Absolute segment.  
#num PF_ARM_ABS
 
-- | ARM unwind segment.  
#num PT_ARM_EXIDX
 
-- | ARM unwind section.  
#num SHT_ARM_EXIDX
 
-- | Preemption details.  
#num SHT_ARM_PREEMPTMAP
 
-- | ARM attributes section.  
#num SHT_ARM_ATTRIBUTES
 
-- | No relocation.  
#num R_AARCH64_NONE
 
-- | Direct 64 bit. 
#num R_AARCH64_ABS64
 
-- | Direct 32 bit.  
#num R_AARCH64_ABS32
 
-- | Direct 16-bit.  
#num R_AARCH64_ABS16
 
-- | PC-relative 64-bit.	
#num R_AARCH64_PREL64
 
-- | PC-relative 32-bit.	
#num R_AARCH64_PREL32
 
-- | PC-relative 16-bit.	
#num R_AARCH64_PREL16
 
-- | Dir. MOVZ imm. from bits 15:0.  
#num R_AARCH64_MOVW_UABS_G0
 
-- | Likewise for MOVK; no check.  
#num R_AARCH64_MOVW_UABS_G0_NC
 
-- | Dir. MOVZ imm. from bits 31:16.  
#num R_AARCH64_MOVW_UABS_G1
 
-- | Likewise for MOVK; no check.  
#num R_AARCH64_MOVW_UABS_G1_NC
 
-- | Dir. MOVZ imm. from bits 47:32.  
#num R_AARCH64_MOVW_UABS_G2
 
-- | Likewise for MOVK; no check.  
#num R_AARCH64_MOVW_UABS_G2_NC
 
-- | Dir. MOV{K,Z} imm. from 63:48.  
#num R_AARCH64_MOVW_UABS_G3
 
-- | Dir. MOV{N,Z} imm. from 15:0.  
#num R_AARCH64_MOVW_SABS_G0
 
-- | Dir. MOV{N,Z} imm. from 31:16.  
#num R_AARCH64_MOVW_SABS_G1
 
-- | Dir. MOV{N,Z} imm. from 47:32.  
#num R_AARCH64_MOVW_SABS_G2
 
-- | PC-rel. LD imm. from bits 20:2.  
#num R_AARCH64_LD_PREL_LO19
 
-- | PC-rel. ADR imm. from bits 20:0.  
#num R_AARCH64_ADR_PREL_LO21
 
-- | Page-rel. ADRP imm. from 32:12.  
#num R_AARCH64_ADR_PREL_PG_HI21
 
-- | Likewise; no overflow check.  
#num R_AARCH64_ADR_PREL_PG_HI21_NC
 
-- | Dir. ADD imm. from bits 11:0.  
#num R_AARCH64_ADD_ABS_LO12_NC
 
-- | Likewise for LD/ST; no check. 
#num R_AARCH64_LDST8_ABS_LO12_NC
 
-- | PC-rel. TBZ/TBNZ imm. from 15:2.  
#num R_AARCH64_TSTBR14
 
-- | PC-rel. cond. br. imm. from 20:2. 
#num R_AARCH64_CONDBR19
 
-- | PC-rel. B imm. from bits 27:2.  
#num R_AARCH64_JUMP26
 
-- | Likewise for CALL.  
#num R_AARCH64_CALL26
 
-- | Dir. ADD imm. from bits 11:1.  
#num R_AARCH64_LDST16_ABS_LO12_NC
 
-- | Likewise for bits 11:2.  
#num R_AARCH64_LDST32_ABS_LO12_NC
 
-- | Likewise for bits 11:3.  
#num R_AARCH64_LDST64_ABS_LO12_NC
 
-- | PC-rel. MOV{N,Z} imm. from 15:0.  
#num R_AARCH64_MOVW_PREL_G0
 
-- | Likewise for MOVK; no check.  
#num R_AARCH64_MOVW_PREL_G0_NC
 
-- | PC-rel. MOV{N,Z} imm. from 31:16. 
#num R_AARCH64_MOVW_PREL_G1
 
-- | Likewise for MOVK; no check.  
#num R_AARCH64_MOVW_PREL_G1_NC
 
-- | PC-rel. MOV{N,Z} imm. from 47:32. 
#num R_AARCH64_MOVW_PREL_G2
 
-- | Likewise for MOVK; no check.  
#num R_AARCH64_MOVW_PREL_G2_NC
 
-- | PC-rel. MOV{N,Z} imm. from 63:48. 
#num R_AARCH64_MOVW_PREL_G3
 
-- | Dir. ADD imm. from bits 11:4.  
#num R_AARCH64_LDST128_ABS_LO12_NC
 
-- | GOT-rel. off. MOV{N,Z} imm. 15:0. 
#num R_AARCH64_MOVW_GOTOFF_G0
 
-- | Likewise for MOVK; no check.  
#num R_AARCH64_MOVW_GOTOFF_G0_NC
 
-- | GOT-rel. o. MOV{N,Z} imm. 31:16.  
#num R_AARCH64_MOVW_GOTOFF_G1
 
-- | Likewise for MOVK; no check.  
#num R_AARCH64_MOVW_GOTOFF_G1_NC
 
-- | GOT-rel. o. MOV{N,Z} imm. 47:32.  
#num R_AARCH64_MOVW_GOTOFF_G2
 
-- | Likewise for MOVK; no check.  
#num R_AARCH64_MOVW_GOTOFF_G2_NC
 
-- | GOT-rel. o. MOV{N,Z} imm. 63:48.  
#num R_AARCH64_MOVW_GOTOFF_G3
 
-- | GOT-relative 64-bit.  
#num R_AARCH64_GOTREL64
 
-- | GOT-relative 32-bit.  
#num R_AARCH64_GOTREL32
 
-- | PC-rel. GOT off. load imm. 20:2.  
#num R_AARCH64_GOT_LD_PREL19
 
-- | GOT-rel. off. LD/ST imm. 14:3.  
#num R_AARCH64_LD64_GOTOFF_LO15
 
-- | P-page-rel. GOT off. ADRP 32:12.  
#num R_AARCH64_ADR_GOT_PAGE
 
-- | Dir. GOT off. LD/ST imm. 11:3.  
#num R_AARCH64_LD64_GOT_LO12_NC
 
-- | GOT-page-rel. GOT off. LD/ST 14:3 
#num R_AARCH64_LD64_GOTPAGE_LO15
 
-- | PC-relative ADR imm. 20:0.  
#num R_AARCH64_TLSGD_ADR_PREL21
 
-- | page-rel. ADRP imm. 32:12.  
#num R_AARCH64_TLSGD_ADR_PAGE21
 
-- | direct ADD imm. from 11:0.  
#num R_AARCH64_TLSGD_ADD_LO12_NC
 
-- | GOT-rel. MOV{N,Z} 31:16.  
#num R_AARCH64_TLSGD_MOVW_G1
 
-- | GOT-rel. MOVK imm. 15:0.  
#num R_AARCH64_TLSGD_MOVW_G0_NC
 
-- | Like 512; local dynamic model.  
#num R_AARCH64_TLSLD_ADR_PREL21
 
-- | Like 513; local dynamic model.  
#num R_AARCH64_TLSLD_ADR_PAGE21
 
-- | Like 514; local dynamic model.  
#num R_AARCH64_TLSLD_ADD_LO12_NC
 
-- | Like 515; local dynamic model.  
#num R_AARCH64_TLSLD_MOVW_G1
 
-- | Like 516; local dynamic model.  
#num R_AARCH64_TLSLD_MOVW_G0_NC
 
-- | TLS PC-rel. load imm. 20:2.  
#num R_AARCH64_TLSLD_LD_PREL19
 
-- | TLS DTP-rel. MOV{N,Z} 47:32.  
#num R_AARCH64_TLSLD_MOVW_DTPREL_G2
 
-- | TLS DTP-rel. MOV{N,Z} 31:16.  
#num R_AARCH64_TLSLD_MOVW_DTPREL_G1
 
-- | Likewise; MOVK; no check.  
#num R_AARCH64_TLSLD_MOVW_DTPREL_G1_NC
 
-- | TLS DTP-rel. MOV{N,Z} 15:0.  
#num R_AARCH64_TLSLD_MOVW_DTPREL_G0
 
-- | Likewise; MOVK; no check.  
#num R_AARCH64_TLSLD_MOVW_DTPREL_G0_NC
 
-- | DTP-rel. ADD imm. from 23:12. 
#num R_AARCH64_TLSLD_ADD_DTPREL_HI12
 
-- | DTP-rel. ADD imm. from 11:0.  
#num R_AARCH64_TLSLD_ADD_DTPREL_LO12
 
-- | Likewise; no ovfl. check.  
#num R_AARCH64_TLSLD_ADD_DTPREL_LO12_NC
 
-- | DTP-rel. LD/ST imm. 11:0.  
#num R_AARCH64_TLSLD_LDST8_DTPREL_LO12
 
-- | Likewise; no check.  
#num R_AARCH64_TLSLD_LDST8_DTPREL_LO12_NC
 
-- | DTP-rel. LD/ST imm. 11:1.  
#num R_AARCH64_TLSLD_LDST16_DTPREL_LO12
 
-- | Likewise; no check.  
#num R_AARCH64_TLSLD_LDST16_DTPREL_LO12_NC
 
-- | DTP-rel. LD/ST imm. 11:2.  
#num R_AARCH64_TLSLD_LDST32_DTPREL_LO12
 
-- | Likewise; no check.  
#num R_AARCH64_TLSLD_LDST32_DTPREL_LO12_NC
 
-- | DTP-rel. LD/ST imm. 11:3.  
#num R_AARCH64_TLSLD_LDST64_DTPREL_LO12
 
-- | Likewise; no check.  
#num R_AARCH64_TLSLD_LDST64_DTPREL_LO12_NC
 
-- | GOT-rel. MOV{N,Z} 31:16.  
#num R_AARCH64_TLSIE_MOVW_GOTTPREL_G1
 
-- | GOT-rel. MOVK 15:0.  
#num R_AARCH64_TLSIE_MOVW_GOTTPREL_G0_NC
 
-- | Page-rel. ADRP 32:12.  
#num R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21
 
-- | Direct LD off. 11:3.  
#num R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC
 
-- | PC-rel. load imm. 20:2.  
#num R_AARCH64_TLSIE_LD_GOTTPREL_PREL19
 
-- | TLS TP-rel. MOV{N,Z} 47:32.  
#num R_AARCH64_TLSLE_MOVW_TPREL_G2
 
-- | TLS TP-rel. MOV{N,Z} 31:16.  
#num R_AARCH64_TLSLE_MOVW_TPREL_G1
 
-- | Likewise; MOVK; no check.  
#num R_AARCH64_TLSLE_MOVW_TPREL_G1_NC
 
-- | TLS TP-rel. MOV{N,Z} 15:0.  
#num R_AARCH64_TLSLE_MOVW_TPREL_G0
 
-- | Likewise; MOVK; no check.  
#num R_AARCH64_TLSLE_MOVW_TPREL_G0_NC
 
-- | TP-rel. ADD imm. 23:12.  
#num R_AARCH64_TLSLE_ADD_TPREL_HI12
 
-- | TP-rel. ADD imm. 11:0.  
#num R_AARCH64_TLSLE_ADD_TPREL_LO12
 
-- | Likewise; no ovfl. check.  
#num R_AARCH64_TLSLE_ADD_TPREL_LO12_NC
 
-- | TP-rel. LD/ST off. 11:0.  
#num R_AARCH64_TLSLE_LDST8_TPREL_LO12
 
-- | Likewise; no ovfl. check. 
#num R_AARCH64_TLSLE_LDST8_TPREL_LO12_NC
 
-- | TP-rel. LD/ST off. 11:1.  
#num R_AARCH64_TLSLE_LDST16_TPREL_LO12
 
-- | Likewise; no check.  
#num R_AARCH64_TLSLE_LDST16_TPREL_LO12_NC
 
-- | TP-rel. LD/ST off. 11:2.  
#num R_AARCH64_TLSLE_LDST32_TPREL_LO12
 
-- | Likewise; no check.  
#num R_AARCH64_TLSLE_LDST32_TPREL_LO12_NC
 
-- | TP-rel. LD/ST off. 11:3.  
#num R_AARCH64_TLSLE_LDST64_TPREL_LO12
 
-- | Likewise; no check.  
#num R_AARCH64_TLSLE_LDST64_TPREL_LO12_NC
 
-- | PC-rel. load immediate 20:2.  
#num R_AARCH64_TLSDESC_LD_PREL19
 
-- | PC-rel. ADR immediate 20:0.  
#num R_AARCH64_TLSDESC_ADR_PREL21
 
-- | Page-rel. ADRP imm. 32:12.  
#num R_AARCH64_TLSDESC_ADR_PAGE21
 
-- | Direct LD off. from 11:3.  
#num R_AARCH64_TLSDESC_LD64_LO12
 
-- | Direct ADD imm. from 11:0.  
#num R_AARCH64_TLSDESC_ADD_LO12
 
-- | GOT-rel. MOV{N,Z} imm. 31:16.  
#num R_AARCH64_TLSDESC_OFF_G1
 
-- | GOT-rel. MOVK imm. 15:0; no ck.  
#num R_AARCH64_TLSDESC_OFF_G0_NC
 
-- | Relax LDR.  
#num R_AARCH64_TLSDESC_LDR
 
-- | Relax ADD.  
#num R_AARCH64_TLSDESC_ADD
 
-- | Relax BLR.  
#num R_AARCH64_TLSDESC_CALL
 
-- | TP-rel. LD/ST off. 11:4.  
#num R_AARCH64_TLSLE_LDST128_TPREL_LO12
 
-- | Likewise; no check.  
#num R_AARCH64_TLSLE_LDST128_TPREL_LO12_NC
 
-- | DTP-rel. LD/ST imm. 11:4. 
#num R_AARCH64_TLSLD_LDST128_DTPREL_LO12
 
-- | Likewise; no check.  
#num R_AARCH64_TLSLD_LDST128_DTPREL_LO12_NC
 
-- | Copy symbol at runtime.  
#num R_AARCH64_COPY
 
-- | Create GOT entry.  
#num R_AARCH64_GLOB_DAT
 
-- | Create PLT entry.  
#num R_AARCH64_JUMP_SLOT
 
-- | Adjust by program base.  
#num R_AARCH64_RELATIVE
 
-- | Module number, 64 bit.  
#num R_AARCH64_TLS_DTPMOD64
 
-- | Module-relative offset, 64 bit.  
#num R_AARCH64_TLS_DTPREL64
 
-- | TP-relative offset, 64 bit.  
#num R_AARCH64_TLS_TPREL64
 
-- | TLS Descriptor.  
#num R_AARCH64_TLSDESC
 
-- | STT_GNU_IFUNC relocation.  
#num R_AARCH64_IRELATIVE
 
-- | No reloc 
#num R_ARM_NONE
 
-- | Deprecated PC relative 26
#num R_ARM_PC24
 
-- | Direct 32 bit  
#num R_ARM_ABS32
 
-- | PC relative 32 bit 
#num R_ARM_REL32
#num R_ARM_PC13
 
-- | Direct 16 bit 
#num R_ARM_ABS16
 
-- | Direct 12 bit 
#num R_ARM_ABS12
 
-- | Direct & 0x7C (LDR, STR).  
#num R_ARM_THM_ABS5
 
-- | Direct 8 bit 
#num R_ARM_ABS8
#num R_ARM_SBREL32
 
-- | PC relative 24 bit (Thumb32 BL).  
#num R_ARM_THM_PC22
 
-- | PC relative & 0x3FC
#num R_ARM_THM_PC8
#num R_ARM_AMP_VCALL9
 
-- | Obsolete static relocation.  
#num R_ARM_SWI24
 
-- | Dynamic relocation.  
#num R_ARM_TLS_DESC
 
-- | Reserved.  
#num R_ARM_THM_SWI8
 
-- | Reserved.  
#num R_ARM_XPC25
 
-- | Reserved.  
#num R_ARM_THM_XPC22
 
-- | ID of module containing symbol 
#num R_ARM_TLS_DTPMOD32
 
-- | Offset in TLS block 
#num R_ARM_TLS_DTPOFF32
 
-- | Offset in static TLS block 
#num R_ARM_TLS_TPOFF32
 
-- | Copy symbol at runtime 
#num R_ARM_COPY
 
-- | Create GOT entry 
#num R_ARM_GLOB_DAT
 
-- | Create PLT entry 
#num R_ARM_JUMP_SLOT
 
-- | Adjust by program base 
#num R_ARM_RELATIVE
 
-- | 32 bit offset to GOT 
#num R_ARM_GOTOFF
 
-- | 32 bit PC relative offset to GOT 
#num R_ARM_GOTPC
 
-- | 32 bit GOT entry 
#num R_ARM_GOT32
 
-- | Deprecated, 32 bit PLT address.  
#num R_ARM_PLT32
 
-- | PC relative 24 bit (BL, BLX).  
#num R_ARM_CALL
 
-- | PC relative 24 bit
#num R_ARM_JUMP24
 
-- | PC relative 24 bit (Thumb32 B.W).  
#num R_ARM_THM_JUMP24
 
-- | Adjust by program base.  
#num R_ARM_BASE_ABS
 
-- | Obsolete.  
#num R_ARM_ALU_PCREL_7_0
 
-- | Obsolete.  
#num R_ARM_ALU_PCREL_15_8
 
-- | Obsolete.  
#num R_ARM_ALU_PCREL_23_15
 
-- | Deprecated, prog. base relative.  
#num R_ARM_LDR_SBREL_11_0
 
-- | Deprecated, prog. base relative.  
#num R_ARM_ALU_SBREL_19_12
 
-- | Deprecated, prog. base relative.  
#num R_ARM_ALU_SBREL_27_20
#num R_ARM_TARGET1
 
-- | Program base relative.  
#num R_ARM_SBREL31
#num R_ARM_V4BX
#num R_ARM_TARGET2
 
-- | 32 bit PC relative.  
#num R_ARM_PREL31
 
-- | Direct 16-bit (MOVW).  
#num R_ARM_MOVW_ABS_NC
 
-- | Direct high 16-bit (MOVT).  
#num R_ARM_MOVT_ABS
 
-- | PC relative 16-bit (MOVW).  
#num R_ARM_MOVW_PREL_NC
 
-- | PC relative (MOVT).  
#num R_ARM_MOVT_PREL
 
-- | Direct 16 bit (Thumb32 MOVW).  
#num R_ARM_THM_MOVW_ABS_NC
 
-- | Direct high 16 bit
#num R_ARM_THM_MOVT_ABS
 
-- | PC relative 16 bit
#num R_ARM_THM_MOVW_PREL_NC
 
-- | PC relative high 16 bit
#num R_ARM_THM_MOVT_PREL
 
-- | PC relative 20 bit
#num R_ARM_THM_JUMP19
 
-- | PC relative X & 0x7E
#num R_ARM_THM_JUMP6
 
-- | PC relative 12 bit
#num R_ARM_THM_ALU_PREL_11_0
 
-- | PC relative 12 bit
#num R_ARM_THM_PC12
 
-- | Direct 32-bit.  
#num R_ARM_ABS32_NOI
 
-- | PC relative 32-bit.  
#num R_ARM_REL32_NOI
 
-- | PC relative (ADD, SUB).  
#num R_ARM_ALU_PC_G0_NC
 
-- | PC relative (ADD, SUB).  
#num R_ARM_ALU_PC_G0
 
-- | PC relative (ADD, SUB).  
#num R_ARM_ALU_PC_G1_NC
 
-- | PC relative (ADD, SUB).  
#num R_ARM_ALU_PC_G1
 
-- | PC relative (ADD, SUB).  
#num R_ARM_ALU_PC_G2
 
-- | PC relative (LDR,STR,LDRB,STRB).  
#num R_ARM_LDR_PC_G1
 
-- | PC relative (LDR,STR,LDRB,STRB).  
#num R_ARM_LDR_PC_G2
 
-- | PC relative (STR{D,H},
#num R_ARM_LDRS_PC_G0
 
-- | PC relative (STR{D,H},
#num R_ARM_LDRS_PC_G1
 
-- | PC relative (STR{D,H},
#num R_ARM_LDRS_PC_G2
 
-- | PC relative (LDC, STC).  
#num R_ARM_LDC_PC_G0
 
-- | PC relative (LDC, STC).  
#num R_ARM_LDC_PC_G1
 
-- | PC relative (LDC, STC).  
#num R_ARM_LDC_PC_G2
 
-- | Program base relative (ADD,SUB).  
#num R_ARM_ALU_SB_G0_NC
 
-- | Program base relative (ADD,SUB).  
#num R_ARM_ALU_SB_G0
 
-- | Program base relative (ADD,SUB).  
#num R_ARM_ALU_SB_G1_NC
 
-- | Program base relative (ADD,SUB).  
#num R_ARM_ALU_SB_G1
 
-- | Program base relative (ADD,SUB).  
#num R_ARM_ALU_SB_G2
 
-- | Program base relative (LDR,
#num R_ARM_LDR_SB_G0
 
-- | Program base relative
#num R_ARM_LDR_SB_G1
 
-- | Program base relative
#num R_ARM_LDR_SB_G2
 
-- | Program base relative
#num R_ARM_LDRS_SB_G0
 
-- | Program base relative
#num R_ARM_LDRS_SB_G1
 
-- | Program base relative
#num R_ARM_LDRS_SB_G2
 
-- | Program base relative (LDC,STC).  
#num R_ARM_LDC_SB_G0
 
-- | Program base relative (LDC,STC).  
#num R_ARM_LDC_SB_G1
 
-- | Program base relative (LDC,STC).  
#num R_ARM_LDC_SB_G2
 
-- | Program base relative 16
#num R_ARM_MOVW_BREL_NC
 
-- | Program base relative high
#num R_ARM_MOVT_BREL
 
-- | Program base relative 16
#num R_ARM_MOVW_BREL
 
-- | Program base relative 16
#num R_ARM_THM_MOVW_BREL_NC
 
-- | Program base relative high
#num R_ARM_THM_MOVT_BREL
 
-- | Program base relative 16
#num R_ARM_THM_MOVW_BREL
#num R_ARM_TLS_GOTDESC
#num R_ARM_TLS_CALL
 
-- | TLS relaxation.  
#num R_ARM_TLS_DESCSEQ
#num R_ARM_THM_TLS_CALL
#num R_ARM_PLT32_ABS
 
-- | GOT entry.  
#num R_ARM_GOT_ABS
 
-- | PC relative GOT entry.  
#num R_ARM_GOT_PREL
 
-- | GOT entry relative to GOT
#num R_ARM_GOT_BREL12
 
-- | 12 bit, GOT entry relative
#num R_ARM_GOTOFF12
#num R_ARM_GOTRELAX
#num R_ARM_GNU_VTENTRY
#num R_ARM_GNU_VTINHERIT
 
-- | PC relative & 0xFFE (Thumb16 B).  
#num R_ARM_THM_PC11
 
-- | PC relative & 0x1FE
#num R_ARM_THM_PC9
 
-- | PC-rel 32 bit for global dynamic
#num R_ARM_TLS_GD32
 
-- | PC-rel 32 bit for local dynamic
#num R_ARM_TLS_LDM32
 
-- | 32 bit offset relative to TLS
#num R_ARM_TLS_LDO32
 
-- | PC-rel 32 bit for GOT entry of
#num R_ARM_TLS_IE32
 
-- | 32 bit offset relative to static
#num R_ARM_TLS_LE32
 
-- | 12 bit relative to TLS
#num R_ARM_TLS_LDO12
 
-- | 12 bit relative to static
#num R_ARM_TLS_LE12
 
-- | 12 bit GOT entry relative
#num R_ARM_TLS_IE12GP
 
-- | Obsolete.  
#num R_ARM_ME_TOO
#num R_ARM_THM_TLS_DESCSEQ
#num R_ARM_THM_TLS_DESCSEQ16
#num R_ARM_THM_TLS_DESCSEQ32
 
-- | GOT entry relative to GOT
#num R_ARM_THM_GOT_BREL12
#num R_ARM_IRELATIVE
#num R_ARM_RXPC25
#num R_ARM_RSBREL32
#num R_ARM_THM_RPC22
#num R_ARM_RREL32
#num R_ARM_RABS22
#num R_ARM_RPC24
#num R_ARM_RBASE
#num R_ARM_NUM
 
-- | os-specific flags 
#num EF_IA_64_MASKOS
 
-- | 64-bit ABI 
#num EF_IA_64_ABI64
 
-- | arch. version mask 
#num EF_IA_64_ARCH
 
-- | arch extension bits 
#num PT_IA_64_ARCHEXT
 
-- | ia64 unwind bits 
#num PT_IA_64_UNWIND
#num PT_IA_64_HP_OPT_ANOT
#num PT_IA_64_HP_HSL_ANOT
#num PT_IA_64_HP_STACK
 
-- | spec insns w/o recovery 
#num PF_IA_64_NORECOV
 
-- | extension bits 
#num SHT_IA_64_EXT
 
-- | unwind bits 
#num SHT_IA_64_UNWIND
 
-- | section near gp 
#num SHF_IA_64_SHORT
 
-- | spec insns w/o recovery 
#num SHF_IA_64_NORECOV
#num DT_IA_64_PLT_RESERVE
#num DT_IA_64_NUM
 
-- | none 
#num R_IA64_NONE
 
-- | symbol + addend, add imm14 
#num R_IA64_IMM14
 
-- | symbol + addend, add imm22 
#num R_IA64_IMM22
 
-- | symbol + addend, mov imm64 
#num R_IA64_IMM64
 
-- | symbol + addend, data4 MSB 
#num R_IA64_DIR32MSB
 
-- | symbol + addend, data4 LSB 
#num R_IA64_DIR32LSB
 
-- | symbol + addend, data8 MSB 
#num R_IA64_DIR64MSB
 
-- | symbol + addend, data8 LSB 
#num R_IA64_DIR64LSB
 
-- | @gprel(sym + add), add imm22 
#num R_IA64_GPREL22
 
-- | @gprel(sym + add), mov imm64 
#num R_IA64_GPREL64I
 
-- | @gprel(sym + add), data4 MSB 
#num R_IA64_GPREL32MSB
 
-- | @gprel(sym + add), data4 LSB 
#num R_IA64_GPREL32LSB
 
-- | @gprel(sym + add), data8 MSB 
#num R_IA64_GPREL64MSB
 
-- | @gprel(sym + add), data8 LSB 
#num R_IA64_GPREL64LSB
 
-- | @ltoff(sym + add), add imm22 
#num R_IA64_LTOFF22
 
-- | @ltoff(sym + add), mov imm64 
#num R_IA64_LTOFF64I
 
-- | @pltoff(sym + add), add imm22 
#num R_IA64_PLTOFF22
 
-- | @pltoff(sym + add), mov imm64 
#num R_IA64_PLTOFF64I
 
-- | @pltoff(sym + add), data8 MSB 
#num R_IA64_PLTOFF64MSB
 
-- | @pltoff(sym + add), data8 LSB 
#num R_IA64_PLTOFF64LSB
 
-- | @fptr(sym + add), mov imm64 
#num R_IA64_FPTR64I
 
-- | @fptr(sym + add), data4 MSB 
#num R_IA64_FPTR32MSB
 
-- | @fptr(sym + add), data4 LSB 
#num R_IA64_FPTR32LSB
 
-- | @fptr(sym + add), data8 MSB 
#num R_IA64_FPTR64MSB
 
-- | @fptr(sym + add), data8 LSB 
#num R_IA64_FPTR64LSB
 
-- | @pcrel(sym + add), brl 
#num R_IA64_PCREL60B
 
-- | @pcrel(sym + add), ptb, call 
#num R_IA64_PCREL21B
 
-- | @pcrel(sym + add), chk.s 
#num R_IA64_PCREL21M
 
-- | @pcrel(sym + add), fchkf 
#num R_IA64_PCREL21F
 
-- | @pcrel(sym + add), data4 MSB 
#num R_IA64_PCREL32MSB
 
-- | @pcrel(sym + add), data4 LSB 
#num R_IA64_PCREL32LSB
 
-- | @pcrel(sym + add), data8 MSB 
#num R_IA64_PCREL64MSB
 
-- | @pcrel(sym + add), data8 LSB 
#num R_IA64_PCREL64LSB
 
-- | @ltoff(@fptr(s+a)), imm22 
#num R_IA64_LTOFF_FPTR22
 
-- | @ltoff(@fptr(s+a)), imm64 
#num R_IA64_LTOFF_FPTR64I
 
-- | @ltoff(@fptr(s+a)), data4 MSB 
#num R_IA64_LTOFF_FPTR32MSB
 
-- | @ltoff(@fptr(s+a)), data4 LSB 
#num R_IA64_LTOFF_FPTR32LSB
 
-- | @ltoff(@fptr(s+a)), data8 MSB 
#num R_IA64_LTOFF_FPTR64MSB
 
-- | @ltoff(@fptr(s+a)), data8 LSB 
#num R_IA64_LTOFF_FPTR64LSB
 
-- | @segrel(sym + add), data4 MSB 
#num R_IA64_SEGREL32MSB
 
-- | @segrel(sym + add), data4 LSB 
#num R_IA64_SEGREL32LSB
 
-- | @segrel(sym + add), data8 MSB 
#num R_IA64_SEGREL64MSB
 
-- | @segrel(sym + add), data8 LSB 
#num R_IA64_SEGREL64LSB
 
-- | @secrel(sym + add), data4 MSB 
#num R_IA64_SECREL32MSB
 
-- | @secrel(sym + add), data4 LSB 
#num R_IA64_SECREL32LSB
 
-- | @secrel(sym + add), data8 MSB 
#num R_IA64_SECREL64MSB
 
-- | @secrel(sym + add), data8 LSB 
#num R_IA64_SECREL64LSB
 
-- | data 4 + REL 
#num R_IA64_REL32MSB
 
-- | data 4 + REL 
#num R_IA64_REL32LSB
 
-- | data 8 + REL 
#num R_IA64_REL64MSB
 
-- | data 8 + REL 
#num R_IA64_REL64LSB
 
-- | symbol + addend, data4 MSB 
#num R_IA64_LTV32MSB
 
-- | symbol + addend, data4 LSB 
#num R_IA64_LTV32LSB
 
-- | symbol + addend, data8 MSB 
#num R_IA64_LTV64MSB
 
-- | symbol + addend, data8 LSB 
#num R_IA64_LTV64LSB
 
-- | @pcrel(sym + add), 21bit inst 
#num R_IA64_PCREL21BI
 
-- | @pcrel(sym + add), 22bit inst 
#num R_IA64_PCREL22
 
-- | @pcrel(sym + add), 64bit inst 
#num R_IA64_PCREL64I
 
-- | dynamic reloc, imported PLT, MSB 
#num R_IA64_IPLTMSB
 
-- | dynamic reloc, imported PLT, LSB 
#num R_IA64_IPLTLSB
 
-- | copy relocation 
#num R_IA64_COPY
 
-- | Addend and symbol difference 
#num R_IA64_SUB
 
-- | LTOFF22, relaxable.  
#num R_IA64_LTOFF22X
 
-- | Use of LTOFF22X.  
#num R_IA64_LDXMOV
 
-- | @tprel(sym + add), imm14 
#num R_IA64_TPREL14
 
-- | @tprel(sym + add), imm22 
#num R_IA64_TPREL22
 
-- | @tprel(sym + add), imm64 
#num R_IA64_TPREL64I
 
-- | @tprel(sym + add), data8 MSB 
#num R_IA64_TPREL64MSB
 
-- | @tprel(sym + add), data8 LSB 
#num R_IA64_TPREL64LSB
 
-- | @ltoff(@tprel(s+a)), imm2 
#num R_IA64_LTOFF_TPREL22
 
-- | @dtpmod(sym + add), data8 MSB 
#num R_IA64_DTPMOD64MSB
 
-- | @dtpmod(sym + add), data8 LSB 
#num R_IA64_DTPMOD64LSB
 
-- | @ltoff(@dtpmod(sym + add)), imm22 
#num R_IA64_LTOFF_DTPMOD22
 
-- | @dtprel(sym + add), imm14 
#num R_IA64_DTPREL14
 
-- | @dtprel(sym + add), imm22 
#num R_IA64_DTPREL22
 
-- | @dtprel(sym + add), imm64 
#num R_IA64_DTPREL64I
 
-- | @dtprel(sym + add), data4 MSB 
#num R_IA64_DTPREL32MSB
 
-- | @dtprel(sym + add), data4 LSB 
#num R_IA64_DTPREL32LSB
 
-- | @dtprel(sym + add), data8 MSB 
#num R_IA64_DTPREL64MSB
 
-- | @dtprel(sym + add), data8 LSB 
#num R_IA64_DTPREL64LSB
 
-- | @ltoff(@dtprel(s+a)), imm22 
#num R_IA64_LTOFF_DTPREL22
#num EF_SH_MACH_MASK
#num EF_SH_UNKNOWN
#num EF_SH1
#num EF_SH2
#num EF_SH3
#num EF_SH_DSP
#num EF_SH3_DSP
#num EF_SH4AL_DSP
#num EF_SH3E
#num EF_SH4
#num EF_SH2E
#num EF_SH4A
#num EF_SH2A
#num EF_SH4_NOFPU
#num EF_SH4A_NOFPU
#num EF_SH4_NOMMU_NOFPU
#num EF_SH2A_NOFPU
#num EF_SH3_NOMMU
#num EF_SH2A_SH4_NOFPU
#num EF_SH2A_SH3_NOFPU
#num EF_SH2A_SH4
#num EF_SH2A_SH3E
#num R_SH_NONE
#num R_SH_DIR32
#num R_SH_REL32
#num R_SH_DIR8WPN
#num R_SH_IND12W
#num R_SH_DIR8WPL
#num R_SH_DIR8WPZ
#num R_SH_DIR8BP
#num R_SH_DIR8W
#num R_SH_DIR8L
#num R_SH_SWITCH16
#num R_SH_SWITCH32
#num R_SH_USES
#num R_SH_COUNT
#num R_SH_ALIGN
#num R_SH_CODE
#num R_SH_DATA
#num R_SH_LABEL
#num R_SH_SWITCH8
#num R_SH_GNU_VTINHERIT
#num R_SH_GNU_VTENTRY
#num R_SH_TLS_GD_32
#num R_SH_TLS_LD_32
#num R_SH_TLS_LDO_32
#num R_SH_TLS_IE_32
#num R_SH_TLS_LE_32
#num R_SH_TLS_DTPMOD32
#num R_SH_TLS_DTPOFF32
#num R_SH_TLS_TPOFF32
#num R_SH_GOT32
#num R_SH_PLT32
#num R_SH_COPY
#num R_SH_GLOB_DAT
#num R_SH_JMP_SLOT
#num R_SH_RELATIVE
#num R_SH_GOTOFF
#num R_SH_GOTPC
#num R_SH_NUM
 
-- | High GPRs kernel facility needed.  
#num EF_S390_HIGH_GPRS
 
-- | No reloc.  
#num R_390_NONE
 
-- | Direct 8 bit.  
#num R_390_8
 
-- | Direct 12 bit.  
#num R_390_12
 
-- | Direct 16 bit.  
#num R_390_16
 
-- | Direct 32 bit.  
#num R_390_32
 
-- | PC relative 32 bit.	
#num R_390_PC32
 
-- | 12 bit GOT offset.  
#num R_390_GOT12
 
-- | 32 bit GOT offset.  
#num R_390_GOT32
 
-- | 32 bit PC relative PLT address.  
#num R_390_PLT32
 
-- | Copy symbol at runtime.  
#num R_390_COPY
 
-- | Create GOT entry.  
#num R_390_GLOB_DAT
 
-- | Create PLT entry.  
#num R_390_JMP_SLOT
 
-- | Adjust by program base.  
#num R_390_RELATIVE
 
-- | 32 bit offset to GOT.	 
#num R_390_GOTOFF32
 
-- | 32 bit PC relative offset to GOT.  
#num R_390_GOTPC
 
-- | 16 bit GOT offset.  
#num R_390_GOT16
 
-- | PC relative 16 bit.	
#num R_390_PC16
 
-- | PC relative 16 bit shifted by 1.  
#num R_390_PC16DBL
 
-- | 16 bit PC rel. PLT shifted by 1.  
#num R_390_PLT16DBL
 
-- | PC relative 32 bit shifted by 1.  
#num R_390_PC32DBL
 
-- | 32 bit PC rel. PLT shifted by 1.  
#num R_390_PLT32DBL
 
-- | 32 bit PC rel. GOT shifted by 1.  
#num R_390_GOTPCDBL
 
-- | Direct 64 bit.  
#num R_390_64
 
-- | PC relative 64 bit.	
#num R_390_PC64
 
-- | 64 bit GOT offset.  
#num R_390_GOT64
 
-- | 64 bit PC relative PLT address.  
#num R_390_PLT64
 
-- | 32 bit PC rel. to GOT entry >> 1. 
#num R_390_GOTENT
 
-- | 16 bit offset to GOT. 
#num R_390_GOTOFF16
 
-- | 64 bit offset to GOT. 
#num R_390_GOTOFF64
 
-- | 12 bit offset to jump slot.	
#num R_390_GOTPLT12
 
-- | 16 bit offset to jump slot.	
#num R_390_GOTPLT16
 
-- | 32 bit offset to jump slot.	
#num R_390_GOTPLT32
 
-- | 64 bit offset to jump slot.	
#num R_390_GOTPLT64
 
-- | 32 bit rel. offset to jump slot.  
#num R_390_GOTPLTENT
 
-- | 16 bit offset from GOT to PLT. 
#num R_390_PLTOFF16
 
-- | 32 bit offset from GOT to PLT. 
#num R_390_PLTOFF32
 
-- | 16 bit offset from GOT to PLT. 
#num R_390_PLTOFF64
 
-- | Tag for load insn in TLS code.  
#num R_390_TLS_LOAD
 
-- | Tag for function call in general
#num R_390_TLS_GDCALL
 
-- | Tag for function call in local
#num R_390_TLS_LDCALL
 
-- | Direct 32 bit for general dynamic
#num R_390_TLS_GD32
 
-- | Direct 64 bit for general dynamic
#num R_390_TLS_GD64
 
-- | 12 bit GOT offset for static TLS
#num R_390_TLS_GOTIE12
 
-- | 32 bit GOT offset for static TLS
#num R_390_TLS_GOTIE32
 
-- | 64 bit GOT offset for static TLS
#num R_390_TLS_GOTIE64
 
-- | Direct 32 bit for local dynamic
#num R_390_TLS_LDM32
 
-- | Direct 64 bit for local dynamic
#num R_390_TLS_LDM64
 
-- | 32 bit address of GOT entry for
#num R_390_TLS_IE32
 
-- | 64 bit address of GOT entry for
#num R_390_TLS_IE64
 
-- | 32 bit rel. offset to GOT entry for
#num R_390_TLS_IEENT
 
-- | 32 bit negated offset relative to
#num R_390_TLS_LE32
 
-- | 64 bit negated offset relative to
#num R_390_TLS_LE64
 
-- | 32 bit offset relative to TLS
#num R_390_TLS_LDO32
 
-- | 64 bit offset relative to TLS
#num R_390_TLS_LDO64
 
-- | ID of module containing symbol.  
#num R_390_TLS_DTPMOD
 
-- | Offset in TLS block.	 
#num R_390_TLS_DTPOFF
 
-- | Negated offset in static TLS
#num R_390_TLS_TPOFF
 
-- | Direct 20 bit.  
#num R_390_20
 
-- | 20 bit GOT offset.  
#num R_390_GOT20
 
-- | 20 bit offset to jump slot.  
#num R_390_GOTPLT20
 
-- | 20 bit GOT offset for static TLS
#num R_390_TLS_GOTIE20
 
-- | STT_GNU_IFUNC relocation.  
#num R_390_IRELATIVE
#num R_390_NUM
#num R_CRIS_NONE
#num R_CRIS_8
#num R_CRIS_16
#num R_CRIS_32
#num R_CRIS_8_PCREL
#num R_CRIS_16_PCREL
#num R_CRIS_32_PCREL
#num R_CRIS_GNU_VTINHERIT
#num R_CRIS_GNU_VTENTRY
#num R_CRIS_COPY
#num R_CRIS_GLOB_DAT
#num R_CRIS_JUMP_SLOT
#num R_CRIS_RELATIVE
#num R_CRIS_16_GOT
#num R_CRIS_32_GOT
#num R_CRIS_16_GOTPLT
#num R_CRIS_32_GOTPLT
#num R_CRIS_32_GOTREL
#num R_CRIS_32_PLT_GOTREL
#num R_CRIS_32_PLT_PCREL
#num R_CRIS_NUM
 
-- | No reloc 
#num R_X86_64_NONE
 
-- | Direct 64 bit  
#num R_X86_64_64
 
-- | PC relative 32 bit signed 
#num R_X86_64_PC32
 
-- | 32 bit GOT entry 
#num R_X86_64_GOT32
 
-- | 32 bit PLT address 
#num R_X86_64_PLT32
 
-- | Copy symbol at runtime 
#num R_X86_64_COPY
 
-- | Create GOT entry 
#num R_X86_64_GLOB_DAT
 
-- | Create PLT entry 
#num R_X86_64_JUMP_SLOT
 
-- | Adjust by program base 
#num R_X86_64_RELATIVE
 
-- | 32 bit signed PC relative
#num R_X86_64_GOTPCREL
 
-- | Direct 32 bit zero extended 
#num R_X86_64_32
 
-- | Direct 32 bit sign extended 
#num R_X86_64_32S
 
-- | Direct 16 bit zero extended 
#num R_X86_64_16
 
-- | 16 bit sign extended pc relative 
#num R_X86_64_PC16
 
-- | Direct 8 bit sign extended  
#num R_X86_64_8
 
-- | 8 bit sign extended pc relative 
#num R_X86_64_PC8
 
-- | ID of module containing symbol 
#num R_X86_64_DTPMOD64
 
-- | Offset in module's TLS block 
#num R_X86_64_DTPOFF64
 
-- | Offset in initial TLS block 
#num R_X86_64_TPOFF64
 
-- | 32 bit signed PC relative offset
#num R_X86_64_TLSGD
 
-- | 32 bit signed PC relative offset
#num R_X86_64_TLSLD
 
-- | Offset in TLS block 
#num R_X86_64_DTPOFF32
 
-- | 32 bit signed PC relative offset
#num R_X86_64_GOTTPOFF
 
-- | Offset in initial TLS block 
#num R_X86_64_TPOFF32
 
-- | PC relative 64 bit 
#num R_X86_64_PC64
 
-- | 64 bit offset to GOT 
#num R_X86_64_GOTOFF64
 
-- | 32 bit signed pc relative
#num R_X86_64_GOTPC32
 
-- | 64-bit GOT entry offset 
#num R_X86_64_GOT64
 
-- | 64-bit PC relative offset
#num R_X86_64_GOTPCREL64
 
-- | 64-bit PC relative offset to GOT 
#num R_X86_64_GOTPC64
 
-- | like GOT64, says PLT entry needed 
#num R_X86_64_GOTPLT64
 
-- | 64-bit GOT relative offset
#num R_X86_64_PLTOFF64
 
-- | Size of symbol plus 32-bit addend 
#num R_X86_64_SIZE32
 
-- | Size of symbol plus 64-bit addend 
#num R_X86_64_SIZE64
 
-- | GOT offset for TLS descriptor.  
#num R_X86_64_GOTPC32_TLSDESC
 
-- | Marker for call through TLS
#num R_X86_64_TLSDESC_CALL
 
-- | TLS descriptor.  
#num R_X86_64_TLSDESC
 
-- | Adjust indirectly by program base 
#num R_X86_64_IRELATIVE
 
-- | 64-bit adjust by program base 
#num R_X86_64_RELATIVE64
#num R_X86_64_NUM
 
-- | No reloc.  
#num R_MN10300_NONE
 
-- | Direct 32 bit.  
#num R_MN10300_32
 
-- | Direct 16 bit.  
#num R_MN10300_16
 
-- | Direct 8 bit.  
#num R_MN10300_8
 
-- | PC-relative 32-bit.  
#num R_MN10300_PCREL32
 
-- | PC-relative 16-bit signed.  
#num R_MN10300_PCREL16
 
-- | PC-relative 8-bit signed.  
#num R_MN10300_PCREL8
 
-- | Ancient C++ vtable garbage... 
#num R_MN10300_GNU_VTINHERIT
 
-- | ... collection annotation.  
#num R_MN10300_GNU_VTENTRY
 
-- | Direct 24 bit.  
#num R_MN10300_24
 
-- | 32-bit PCrel offset to GOT.  
#num R_MN10300_GOTPC32
 
-- | 16-bit PCrel offset to GOT.  
#num R_MN10300_GOTPC16
 
-- | 32-bit offset from GOT.  
#num R_MN10300_GOTOFF32
 
-- | 24-bit offset from GOT.  
#num R_MN10300_GOTOFF24
 
-- | 16-bit offset from GOT.  
#num R_MN10300_GOTOFF16
 
-- | 32-bit PCrel to PLT entry.  
#num R_MN10300_PLT32
 
-- | 16-bit PCrel to PLT entry.  
#num R_MN10300_PLT16
 
-- | 32-bit offset to GOT entry.  
#num R_MN10300_GOT32
 
-- | 24-bit offset to GOT entry.  
#num R_MN10300_GOT24
 
-- | 16-bit offset to GOT entry.  
#num R_MN10300_GOT16
 
-- | Copy symbol at runtime.  
#num R_MN10300_COPY
 
-- | Create GOT entry.  
#num R_MN10300_GLOB_DAT
 
-- | Create PLT entry.  
#num R_MN10300_JMP_SLOT
 
-- | Adjust by program base.  
#num R_MN10300_RELATIVE
 
-- | 32-bit offset for global dynamic.  
#num R_MN10300_TLS_GD
 
-- | 32-bit offset for local dynamic.  
#num R_MN10300_TLS_LD
 
-- | Module-relative offset.  
#num R_MN10300_TLS_LDO
 
-- | GOT offset for static TLS block
#num R_MN10300_TLS_GOTIE
 
-- | GOT address for static TLS block
#num R_MN10300_TLS_IE
 
-- | Offset relative to static TLS
#num R_MN10300_TLS_LE
 
-- | ID of module containing symbol.  
#num R_MN10300_TLS_DTPMOD
 
-- | Offset in module TLS block.  
#num R_MN10300_TLS_DTPOFF
 
-- | Offset in static TLS block.  
#num R_MN10300_TLS_TPOFF
 
-- | Adjustment for next reloc as needed
#num R_MN10300_SYM_DIFF
 
-- | Alignment requirement for linker
#num R_MN10300_ALIGN
#num R_MN10300_NUM
 
-- | No reloc. 
#num R_M32R_NONE
 
-- | Direct 16 bit. 
#num R_M32R_16
 
-- | Direct 32 bit. 
#num R_M32R_32
 
-- | Direct 24 bit. 
#num R_M32R_24
 
-- | PC relative 10 bit shifted. 
#num R_M32R_10_PCREL
 
-- | PC relative 18 bit shifted. 
#num R_M32R_18_PCREL
 
-- | PC relative 26 bit shifted. 
#num R_M32R_26_PCREL
 
-- | High 16 bit with unsigned low. 
#num R_M32R_HI16_ULO
 
-- | High 16 bit with signed low. 
#num R_M32R_HI16_SLO
 
-- | Low 16 bit. 
#num R_M32R_LO16
 
-- | 16 bit offset in SDA. 
#num R_M32R_SDA16
#num R_M32R_GNU_VTINHERIT
#num R_M32R_GNU_VTENTRY
 
-- | Direct 16 bit. 
#num R_M32R_16_RELA
 
-- | Direct 32 bit. 
#num R_M32R_32_RELA
 
-- | Direct 24 bit. 
#num R_M32R_24_RELA
 
-- | PC relative 10 bit shifted. 
#num R_M32R_10_PCREL_RELA
 
-- | PC relative 18 bit shifted. 
#num R_M32R_18_PCREL_RELA
 
-- | PC relative 26 bit shifted. 
#num R_M32R_26_PCREL_RELA
 
-- | High 16 bit with unsigned low 
#num R_M32R_HI16_ULO_RELA
 
-- | High 16 bit with signed low 
#num R_M32R_HI16_SLO_RELA
 
-- | Low 16 bit 
#num R_M32R_LO16_RELA
 
-- | 16 bit offset in SDA 
#num R_M32R_SDA16_RELA
#num R_M32R_RELA_GNU_VTINHERIT
#num R_M32R_RELA_GNU_VTENTRY
 
-- | PC relative 32 bit.  
#num R_M32R_REL32
 
-- | 24 bit GOT entry 
#num R_M32R_GOT24
 
-- | 26 bit PC relative to PLT shifted 
#num R_M32R_26_PLTREL
 
-- | Copy symbol at runtime 
#num R_M32R_COPY
 
-- | Create GOT entry 
#num R_M32R_GLOB_DAT
 
-- | Create PLT entry 
#num R_M32R_JMP_SLOT
 
-- | Adjust by program base 
#num R_M32R_RELATIVE
 
-- | 24 bit offset to GOT 
#num R_M32R_GOTOFF
 
-- | 24 bit PC relative offset to GOT 
#num R_M32R_GOTPC24
 
-- | High 16 bit GOT entry with unsigned
#num R_M32R_GOT16_HI_ULO
 
-- | High 16 bit GOT entry with signed
#num R_M32R_GOT16_HI_SLO
 
-- | Low 16 bit GOT entry 
#num R_M32R_GOT16_LO
 
-- | High 16 bit PC relative offset to
#num R_M32R_GOTPC_HI_ULO
 
-- | High 16 bit PC relative offset to
#num R_M32R_GOTPC_HI_SLO
 
-- | Low 16 bit PC relative offset to
#num R_M32R_GOTPC_LO
 
-- | High 16 bit offset to GOT
#num R_M32R_GOTOFF_HI_ULO
 
-- | High 16 bit offset to GOT
#num R_M32R_GOTOFF_HI_SLO
 
-- | Low 16 bit offset to GOT 
#num R_M32R_GOTOFF_LO
 
-- | Keep this the last entry. 
#num R_M32R_NUM
 
-- | No reloc. 
#num R_MICROBLAZE_NONE
 
-- | Direct 32 bit. 
#num R_MICROBLAZE_32
 
-- | PC relative 32 bit. 
#num R_MICROBLAZE_32_PCREL
 
-- | PC relative 64 bit. 
#num R_MICROBLAZE_64_PCREL
 
-- | Low 16 bits of PCREL32. 
#num R_MICROBLAZE_32_PCREL_LO
 
-- | Direct 64 bit. 
#num R_MICROBLAZE_64
 
-- | Low 16 bit. 
#num R_MICROBLAZE_32_LO
 
-- | Read-only small data area. 
#num R_MICROBLAZE_SRO32
 
-- | Read-write small data area. 
#num R_MICROBLAZE_SRW32
 
-- | No reloc. 
#num R_MICROBLAZE_64_NONE
 
-- | Symbol Op Symbol relocation. 
#num R_MICROBLAZE_32_SYM_OP_SYM
 
-- | GNU C++ vtable hierarchy. 
#num R_MICROBLAZE_GNU_VTINHERIT
 
-- | GNU C++ vtable member usage. 
#num R_MICROBLAZE_GNU_VTENTRY
 
-- | PC-relative GOT offset.  
#num R_MICROBLAZE_GOTPC_64
 
-- | GOT entry offset.  
#num R_MICROBLAZE_GOT_64
 
-- | PLT offset (PC-relative).  
#num R_MICROBLAZE_PLT_64
 
-- | Adjust by program base.  
#num R_MICROBLAZE_REL
 
-- | Create PLT entry.  
#num R_MICROBLAZE_JUMP_SLOT
 
-- | Create GOT entry.  
#num R_MICROBLAZE_GLOB_DAT
 
-- | 64 bit offset to GOT. 
#num R_MICROBLAZE_GOTOFF_64
 
-- | 32 bit offset to GOT. 
#num R_MICROBLAZE_GOTOFF_32
 
-- | Runtime copy.  
#num R_MICROBLAZE_COPY
 
-- | TLS Reloc. 
#num R_MICROBLAZE_TLS
 
-- | TLS General Dynamic. 
#num R_MICROBLAZE_TLSGD
 
-- | TLS Local Dynamic. 
#num R_MICROBLAZE_TLSLD
 
-- | TLS Module ID. 
#num R_MICROBLAZE_TLSDTPMOD32
 
-- | TLS Offset Within TLS Block. 
#num R_MICROBLAZE_TLSDTPREL32
 
-- | TLS Offset Within TLS Block. 
#num R_MICROBLAZE_TLSDTPREL64
 
-- | TLS Offset From Thread Pointer. 
#num R_MICROBLAZE_TLSGOTTPREL32
 
-- | TLS Offset From Thread Pointer. 
#num R_MICROBLAZE_TLSTPREL32
 
-- | No reloc 
#num R_TILEPRO_NONE
 
-- | Direct 32 bit 
#num R_TILEPRO_32
 
-- | Direct 16 bit 
#num R_TILEPRO_16
 
-- | Direct 8 bit 
#num R_TILEPRO_8
 
-- | PC relative 32 bit 
#num R_TILEPRO_32_PCREL
 
-- | PC relative 16 bit 
#num R_TILEPRO_16_PCREL
 
-- | PC relative 8 bit 
#num R_TILEPRO_8_PCREL
 
-- | Low 16 bit 
#num R_TILEPRO_LO16
 
-- | High 16 bit 
#num R_TILEPRO_HI16
 
-- | High 16 bit, adjusted 
#num R_TILEPRO_HA16
 
-- | Copy relocation 
#num R_TILEPRO_COPY
 
-- | Create GOT entry 
#num R_TILEPRO_GLOB_DAT
 
-- | Create PLT entry 
#num R_TILEPRO_JMP_SLOT
 
-- | Adjust by program base 
#num R_TILEPRO_RELATIVE
 
-- | X1 pipe branch offset 
#num R_TILEPRO_BROFF_X1
 
-- | X1 pipe jump offset 
#num R_TILEPRO_JOFFLONG_X1
 
-- | X1 pipe jump offset to PLT 
#num R_TILEPRO_JOFFLONG_X1_PLT
 
-- | X0 pipe 8-bit 
#num R_TILEPRO_IMM8_X0
 
-- | Y0 pipe 8-bit 
#num R_TILEPRO_IMM8_Y0
 
-- | X1 pipe 8-bit 
#num R_TILEPRO_IMM8_X1
 
-- | Y1 pipe 8-bit 
#num R_TILEPRO_IMM8_Y1
 
-- | X1 pipe mtspr 
#num R_TILEPRO_MT_IMM15_X1
 
-- | X1 pipe mfspr 
#num R_TILEPRO_MF_IMM15_X1
 
-- | X0 pipe 16-bit 
#num R_TILEPRO_IMM16_X0
 
-- | X1 pipe 16-bit 
#num R_TILEPRO_IMM16_X1
 
-- | X0 pipe low 16-bit 
#num R_TILEPRO_IMM16_X0_LO
 
-- | X1 pipe low 16-bit 
#num R_TILEPRO_IMM16_X1_LO
 
-- | X0 pipe high 16-bit 
#num R_TILEPRO_IMM16_X0_HI
 
-- | X1 pipe high 16-bit 
#num R_TILEPRO_IMM16_X1_HI
 
-- | X0 pipe high 16-bit, adjusted 
#num R_TILEPRO_IMM16_X0_HA
 
-- | X1 pipe high 16-bit, adjusted 
#num R_TILEPRO_IMM16_X1_HA
 
-- | X0 pipe PC relative 16 bit 
#num R_TILEPRO_IMM16_X0_PCREL
 
-- | X1 pipe PC relative 16 bit 
#num R_TILEPRO_IMM16_X1_PCREL
 
-- | X0 pipe PC relative low 16 bit 
#num R_TILEPRO_IMM16_X0_LO_PCREL
 
-- | X1 pipe PC relative low 16 bit 
#num R_TILEPRO_IMM16_X1_LO_PCREL
 
-- | X0 pipe PC relative high 16 bit 
#num R_TILEPRO_IMM16_X0_HI_PCREL
 
-- | X1 pipe PC relative high 16 bit 
#num R_TILEPRO_IMM16_X1_HI_PCREL
 
-- | X0 pipe PC relative ha() 16 bit 
#num R_TILEPRO_IMM16_X0_HA_PCREL
 
-- | X1 pipe PC relative ha() 16 bit 
#num R_TILEPRO_IMM16_X1_HA_PCREL
 
-- | X0 pipe 16-bit GOT offset 
#num R_TILEPRO_IMM16_X0_GOT
 
-- | X1 pipe 16-bit GOT offset 
#num R_TILEPRO_IMM16_X1_GOT
 
-- | X0 pipe low 16-bit GOT offset 
#num R_TILEPRO_IMM16_X0_GOT_LO
 
-- | X1 pipe low 16-bit GOT offset 
#num R_TILEPRO_IMM16_X1_GOT_LO
 
-- | X0 pipe high 16-bit GOT offset 
#num R_TILEPRO_IMM16_X0_GOT_HI
 
-- | X1 pipe high 16-bit GOT offset 
#num R_TILEPRO_IMM16_X1_GOT_HI
 
-- | X0 pipe ha() 16-bit GOT offset 
#num R_TILEPRO_IMM16_X0_GOT_HA
 
-- | X1 pipe ha() 16-bit GOT offset 
#num R_TILEPRO_IMM16_X1_GOT_HA
 
-- | X0 pipe mm "start" 
#num R_TILEPRO_MMSTART_X0
 
-- | X0 pipe mm "end" 
#num R_TILEPRO_MMEND_X0
 
-- | X1 pipe mm "start" 
#num R_TILEPRO_MMSTART_X1
 
-- | X1 pipe mm "end" 
#num R_TILEPRO_MMEND_X1
 
-- | X0 pipe shift amount 
#num R_TILEPRO_SHAMT_X0
 
-- | X1 pipe shift amount 
#num R_TILEPRO_SHAMT_X1
 
-- | Y0 pipe shift amount 
#num R_TILEPRO_SHAMT_Y0
 
-- | Y1 pipe shift amount 
#num R_TILEPRO_SHAMT_Y1
 
-- | X1 pipe destination 8-bit 
#num R_TILEPRO_DEST_IMM8_X1
 
-- | Relocs 56-59 are currently not defined.  
 
 
-- | "jal" for TLS GD 
#num R_TILEPRO_TLS_GD_CALL
 
-- | X0 pipe "addi" for TLS GD 
#num R_TILEPRO_IMM8_X0_TLS_GD_ADD
 
-- | X1 pipe "addi" for TLS GD 
#num R_TILEPRO_IMM8_X1_TLS_GD_ADD
 
-- | Y0 pipe "addi" for TLS GD 
#num R_TILEPRO_IMM8_Y0_TLS_GD_ADD
 
-- | Y1 pipe "addi" for TLS GD 
#num R_TILEPRO_IMM8_Y1_TLS_GD_ADD
 
-- | "lw_tls" for TLS IE 
#num R_TILEPRO_TLS_IE_LOAD
 
-- | X0 pipe 16-bit TLS GD offset 
#num R_TILEPRO_IMM16_X0_TLS_GD
 
-- | X1 pipe 16-bit TLS GD offset 
#num R_TILEPRO_IMM16_X1_TLS_GD
 
-- | X0 pipe low 16-bit TLS GD offset 
#num R_TILEPRO_IMM16_X0_TLS_GD_LO
 
-- | X1 pipe low 16-bit TLS GD offset 
#num R_TILEPRO_IMM16_X1_TLS_GD_LO
 
-- | X0 pipe high 16-bit TLS GD offset 
#num R_TILEPRO_IMM16_X0_TLS_GD_HI
 
-- | X1 pipe high 16-bit TLS GD offset 
#num R_TILEPRO_IMM16_X1_TLS_GD_HI
 
-- | X0 pipe ha() 16-bit TLS GD offset 
#num R_TILEPRO_IMM16_X0_TLS_GD_HA
 
-- | X1 pipe ha() 16-bit TLS GD offset 
#num R_TILEPRO_IMM16_X1_TLS_GD_HA
 
-- | X0 pipe 16-bit TLS IE offset 
#num R_TILEPRO_IMM16_X0_TLS_IE
 
-- | X1 pipe 16-bit TLS IE offset 
#num R_TILEPRO_IMM16_X1_TLS_IE
 
-- | X0 pipe low 16-bit TLS IE offset 
#num R_TILEPRO_IMM16_X0_TLS_IE_LO
 
-- | X1 pipe low 16-bit TLS IE offset 
#num R_TILEPRO_IMM16_X1_TLS_IE_LO
 
-- | X0 pipe high 16-bit TLS IE offset 
#num R_TILEPRO_IMM16_X0_TLS_IE_HI
 
-- | X1 pipe high 16-bit TLS IE offset 
#num R_TILEPRO_IMM16_X1_TLS_IE_HI
 
-- | X0 pipe ha() 16-bit TLS IE offset 
#num R_TILEPRO_IMM16_X0_TLS_IE_HA
 
-- | X1 pipe ha() 16-bit TLS IE offset 
#num R_TILEPRO_IMM16_X1_TLS_IE_HA
 
-- | ID of module containing symbol 
#num R_TILEPRO_TLS_DTPMOD32
 
-- | Offset in TLS block 
#num R_TILEPRO_TLS_DTPOFF32
 
-- | Offset in static TLS block 
#num R_TILEPRO_TLS_TPOFF32
 
-- | X0 pipe 16-bit TLS LE offset 
#num R_TILEPRO_IMM16_X0_TLS_LE
 
-- | X1 pipe 16-bit TLS LE offset 
#num R_TILEPRO_IMM16_X1_TLS_LE
 
-- | X0 pipe low 16-bit TLS LE offset 
#num R_TILEPRO_IMM16_X0_TLS_LE_LO
 
-- | X1 pipe low 16-bit TLS LE offset 
#num R_TILEPRO_IMM16_X1_TLS_LE_LO
 
-- | X0 pipe high 16-bit TLS LE offset 
#num R_TILEPRO_IMM16_X0_TLS_LE_HI
 
-- | X1 pipe high 16-bit TLS LE offset 
#num R_TILEPRO_IMM16_X1_TLS_LE_HI
 
-- | X0 pipe ha() 16-bit TLS LE offset 
#num R_TILEPRO_IMM16_X0_TLS_LE_HA
 
-- | X1 pipe ha() 16-bit TLS LE offset 
#num R_TILEPRO_IMM16_X1_TLS_LE_HA
 
-- | GNU C++ vtable hierarchy 
#num R_TILEPRO_GNU_VTINHERIT
 
-- | GNU C++ vtable member usage 
#num R_TILEPRO_GNU_VTENTRY
#num R_TILEPRO_NUM
 
-- | No reloc 
#num R_TILEGX_NONE
 
-- | Direct 64 bit 
#num R_TILEGX_64
 
-- | Direct 32 bit 
#num R_TILEGX_32
 
-- | Direct 16 bit 
#num R_TILEGX_16
 
-- | Direct 8 bit 
#num R_TILEGX_8
 
-- | PC relative 64 bit 
#num R_TILEGX_64_PCREL
 
-- | PC relative 32 bit 
#num R_TILEGX_32_PCREL
 
-- | PC relative 16 bit 
#num R_TILEGX_16_PCREL
 
-- | PC relative 8 bit 
#num R_TILEGX_8_PCREL
 
-- | hword 0 16-bit 
#num R_TILEGX_HW0
 
-- | hword 1 16-bit 
#num R_TILEGX_HW1
 
-- | hword 2 16-bit 
#num R_TILEGX_HW2
 
-- | hword 3 16-bit 
#num R_TILEGX_HW3
 
-- | last hword 0 16-bit 
#num R_TILEGX_HW0_LAST
 
-- | last hword 1 16-bit 
#num R_TILEGX_HW1_LAST
 
-- | last hword 2 16-bit 
#num R_TILEGX_HW2_LAST
 
-- | Copy relocation 
#num R_TILEGX_COPY
 
-- | Create GOT entry 
#num R_TILEGX_GLOB_DAT
 
-- | Create PLT entry 
#num R_TILEGX_JMP_SLOT
 
-- | Adjust by program base 
#num R_TILEGX_RELATIVE
 
-- | X1 pipe branch offset 
#num R_TILEGX_BROFF_X1
 
-- | X1 pipe jump offset 
#num R_TILEGX_JUMPOFF_X1
 
-- | X1 pipe jump offset to PLT 
#num R_TILEGX_JUMPOFF_X1_PLT
 
-- | X0 pipe 8-bit 
#num R_TILEGX_IMM8_X0
 
-- | Y0 pipe 8-bit 
#num R_TILEGX_IMM8_Y0
 
-- | X1 pipe 8-bit 
#num R_TILEGX_IMM8_X1
 
-- | Y1 pipe 8-bit 
#num R_TILEGX_IMM8_Y1
 
-- | X1 pipe destination 8-bit 
#num R_TILEGX_DEST_IMM8_X1
 
-- | X1 pipe mtspr 
#num R_TILEGX_MT_IMM14_X1
 
-- | X1 pipe mfspr 
#num R_TILEGX_MF_IMM14_X1
 
-- | X0 pipe mm "start" 
#num R_TILEGX_MMSTART_X0
 
-- | X0 pipe mm "end" 
#num R_TILEGX_MMEND_X0
 
-- | X0 pipe shift amount 
#num R_TILEGX_SHAMT_X0
 
-- | X1 pipe shift amount 
#num R_TILEGX_SHAMT_X1
 
-- | Y0 pipe shift amount 
#num R_TILEGX_SHAMT_Y0
 
-- | Y1 pipe shift amount 
#num R_TILEGX_SHAMT_Y1
 
-- | X0 pipe hword 0 
#num R_TILEGX_IMM16_X0_HW0
 
-- | X1 pipe hword 0 
#num R_TILEGX_IMM16_X1_HW0
 
-- | X0 pipe hword 1 
#num R_TILEGX_IMM16_X0_HW1
 
-- | X1 pipe hword 1 
#num R_TILEGX_IMM16_X1_HW1
 
-- | X0 pipe hword 2 
#num R_TILEGX_IMM16_X0_HW2
 
-- | X1 pipe hword 2 
#num R_TILEGX_IMM16_X1_HW2
 
-- | X0 pipe hword 3 
#num R_TILEGX_IMM16_X0_HW3
 
-- | X1 pipe hword 3 
#num R_TILEGX_IMM16_X1_HW3
 
-- | X0 pipe last hword 0 
#num R_TILEGX_IMM16_X0_HW0_LAST
 
-- | X1 pipe last hword 0 
#num R_TILEGX_IMM16_X1_HW0_LAST
 
-- | X0 pipe last hword 1 
#num R_TILEGX_IMM16_X0_HW1_LAST
 
-- | X1 pipe last hword 1 
#num R_TILEGX_IMM16_X1_HW1_LAST
 
-- | X0 pipe last hword 2 
#num R_TILEGX_IMM16_X0_HW2_LAST
 
-- | X1 pipe last hword 2 
#num R_TILEGX_IMM16_X1_HW2_LAST
 
-- | X0 pipe PC relative hword 0 
#num R_TILEGX_IMM16_X0_HW0_PCREL
 
-- | X1 pipe PC relative hword 0 
#num R_TILEGX_IMM16_X1_HW0_PCREL
 
-- | X0 pipe PC relative hword 1 
#num R_TILEGX_IMM16_X0_HW1_PCREL
 
-- | X1 pipe PC relative hword 1 
#num R_TILEGX_IMM16_X1_HW1_PCREL
 
-- | X0 pipe PC relative hword 2 
#num R_TILEGX_IMM16_X0_HW2_PCREL
 
-- | X1 pipe PC relative hword 2 
#num R_TILEGX_IMM16_X1_HW2_PCREL
 
-- | X0 pipe PC relative hword 3 
#num R_TILEGX_IMM16_X0_HW3_PCREL
 
-- | X1 pipe PC relative hword 3 
#num R_TILEGX_IMM16_X1_HW3_PCREL
 
-- | X0 pipe PC-rel last hword 0 
#num R_TILEGX_IMM16_X0_HW0_LAST_PCREL
 
-- | X1 pipe PC-rel last hword 0 
#num R_TILEGX_IMM16_X1_HW0_LAST_PCREL
 
-- | X0 pipe PC-rel last hword 1 
#num R_TILEGX_IMM16_X0_HW1_LAST_PCREL
 
-- | X1 pipe PC-rel last hword 1 
#num R_TILEGX_IMM16_X1_HW1_LAST_PCREL
 
-- | X0 pipe PC-rel last hword 2 
#num R_TILEGX_IMM16_X0_HW2_LAST_PCREL
 
-- | X1 pipe PC-rel last hword 2 
#num R_TILEGX_IMM16_X1_HW2_LAST_PCREL
 
-- | X0 pipe hword 0 GOT offset 
#num R_TILEGX_IMM16_X0_HW0_GOT
 
-- | X1 pipe hword 0 GOT offset 
#num R_TILEGX_IMM16_X1_HW0_GOT
 
-- | X0 pipe PC-rel PLT hword 0 
#num R_TILEGX_IMM16_X0_HW0_PLT_PCREL
 
-- | X1 pipe PC-rel PLT hword 0 
#num R_TILEGX_IMM16_X1_HW0_PLT_PCREL
 
-- | X0 pipe PC-rel PLT hword 1 
#num R_TILEGX_IMM16_X0_HW1_PLT_PCREL
 
-- | X1 pipe PC-rel PLT hword 1 
#num R_TILEGX_IMM16_X1_HW1_PLT_PCREL
 
-- | X0 pipe PC-rel PLT hword 2 
#num R_TILEGX_IMM16_X0_HW2_PLT_PCREL
 
-- | X1 pipe PC-rel PLT hword 2 
#num R_TILEGX_IMM16_X1_HW2_PLT_PCREL
 
-- | X0 pipe last hword 0 GOT offset 
#num R_TILEGX_IMM16_X0_HW0_LAST_GOT
 
-- | X1 pipe last hword 0 GOT offset 
#num R_TILEGX_IMM16_X1_HW0_LAST_GOT
 
-- | X0 pipe last hword 1 GOT offset 
#num R_TILEGX_IMM16_X0_HW1_LAST_GOT
 
-- | X1 pipe last hword 1 GOT offset 
#num R_TILEGX_IMM16_X1_HW1_LAST_GOT
 
-- | X0 pipe PC-rel PLT hword 3 
#num R_TILEGX_IMM16_X0_HW3_PLT_PCREL
 
-- | X1 pipe PC-rel PLT hword 3 
#num R_TILEGX_IMM16_X1_HW3_PLT_PCREL
 
-- | X0 pipe hword 0 TLS GD offset 
#num R_TILEGX_IMM16_X0_HW0_TLS_GD
 
-- | X1 pipe hword 0 TLS GD offset 
#num R_TILEGX_IMM16_X1_HW0_TLS_GD
 
-- | X0 pipe hword 0 TLS LE offset 
#num R_TILEGX_IMM16_X0_HW0_TLS_LE
 
-- | X1 pipe hword 0 TLS LE offset 
#num R_TILEGX_IMM16_X1_HW0_TLS_LE
 
-- | X0 pipe last hword 0 LE off 
#num R_TILEGX_IMM16_X0_HW0_LAST_TLS_LE
 
-- | X1 pipe last hword 0 LE off 
#num R_TILEGX_IMM16_X1_HW0_LAST_TLS_LE
 
-- | X0 pipe last hword 1 LE off 
#num R_TILEGX_IMM16_X0_HW1_LAST_TLS_LE
 
-- | X1 pipe last hword 1 LE off 
#num R_TILEGX_IMM16_X1_HW1_LAST_TLS_LE
 
-- | X0 pipe last hword 0 GD off 
#num R_TILEGX_IMM16_X0_HW0_LAST_TLS_GD
 
-- | X1 pipe last hword 0 GD off 
#num R_TILEGX_IMM16_X1_HW0_LAST_TLS_GD
 
-- | X0 pipe last hword 1 GD off 
#num R_TILEGX_IMM16_X0_HW1_LAST_TLS_GD
 
-- | X1 pipe last hword 1 GD off 
#num R_TILEGX_IMM16_X1_HW1_LAST_TLS_GD
 
-- | Relocs 90-91 are currently not defined.  
 
 
-- | X0 pipe hword 0 TLS IE offset 
#num R_TILEGX_IMM16_X0_HW0_TLS_IE
 
-- | X1 pipe hword 0 TLS IE offset 
#num R_TILEGX_IMM16_X1_HW0_TLS_IE
 
-- | X0 pipe PC-rel PLT last hword 0 
#num R_TILEGX_IMM16_X0_HW0_LAST_PLT_PCREL
 
-- | X1 pipe PC-rel PLT last hword 0 
#num R_TILEGX_IMM16_X1_HW0_LAST_PLT_PCREL
 
-- | X0 pipe PC-rel PLT last hword 1 
#num R_TILEGX_IMM16_X0_HW1_LAST_PLT_PCREL
 
-- | X1 pipe PC-rel PLT last hword 1 
#num R_TILEGX_IMM16_X1_HW1_LAST_PLT_PCREL
 
-- | X0 pipe PC-rel PLT last hword 2 
#num R_TILEGX_IMM16_X0_HW2_LAST_PLT_PCREL
 
-- | X1 pipe PC-rel PLT last hword 2 
#num R_TILEGX_IMM16_X1_HW2_LAST_PLT_PCREL
 
-- | X0 pipe last hword 0 IE off 
#num R_TILEGX_IMM16_X0_HW0_LAST_TLS_IE
 
-- | X1 pipe last hword 0 IE off 
#num R_TILEGX_IMM16_X1_HW0_LAST_TLS_IE
 
-- | X0 pipe last hword 1 IE off 
#num R_TILEGX_IMM16_X0_HW1_LAST_TLS_IE
 
-- | X1 pipe last hword 1 IE off 
#num R_TILEGX_IMM16_X1_HW1_LAST_TLS_IE
 
-- | Relocs 104-105 are currently not defined.  
 
 
-- | 64-bit ID of symbol's module 
#num R_TILEGX_TLS_DTPMOD64
 
-- | 64-bit offset in TLS block 
#num R_TILEGX_TLS_DTPOFF64
 
-- | 64-bit offset in static TLS block 
#num R_TILEGX_TLS_TPOFF64
 
-- | 32-bit ID of symbol's module 
#num R_TILEGX_TLS_DTPMOD32
 
-- | 32-bit offset in TLS block 
#num R_TILEGX_TLS_DTPOFF32
 
-- | 32-bit offset in static TLS block 
#num R_TILEGX_TLS_TPOFF32
 
-- | "jal" for TLS GD 
#num R_TILEGX_TLS_GD_CALL
 
-- | X0 pipe "addi" for TLS GD 
#num R_TILEGX_IMM8_X0_TLS_GD_ADD
 
-- | X1 pipe "addi" for TLS GD 
#num R_TILEGX_IMM8_X1_TLS_GD_ADD
 
-- | Y0 pipe "addi" for TLS GD 
#num R_TILEGX_IMM8_Y0_TLS_GD_ADD
 
-- | Y1 pipe "addi" for TLS GD 
#num R_TILEGX_IMM8_Y1_TLS_GD_ADD
 
-- | "ld_tls" for TLS IE 
#num R_TILEGX_TLS_IE_LOAD
 
-- | X0 pipe "addi" for TLS GD/IE 
#num R_TILEGX_IMM8_X0_TLS_ADD
 
-- | X1 pipe "addi" for TLS GD/IE 
#num R_TILEGX_IMM8_X1_TLS_ADD
 
-- | Y0 pipe "addi" for TLS GD/IE 
#num R_TILEGX_IMM8_Y0_TLS_ADD
 
-- | Y1 pipe "addi" for TLS GD/IE 
#num R_TILEGX_IMM8_Y1_TLS_ADD
 
-- | GNU C++ vtable hierarchy 
#num R_TILEGX_GNU_VTINHERIT
 
-- | GNU C++ vtable member usage 
#num R_TILEGX_GNU_VTENTRY
#num R_TILEGX_NUM
