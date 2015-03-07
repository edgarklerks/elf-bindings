#define _POSIX_SOURCE
#include <bindings.dsl.h>
#include <elf.h>
#include <stddef.h>

module Elf.ElfHeaders where
#strict_import



-- | Elf header definitions
#num EI_NIDENT
#integral_t uint16_t
#integral_t uint32_t
#integral_t uint64_t
#integral_t Elf64_Off
#integral_t Elf64_Section
#integral_t Elf64_Versym
-- #integral_t Elf_Byte
#integral_t Elf64_Half
#integral_t Elf64_Sword
#integral_t Elf64_Word
#integral_t Elf64_Sxword
#integral_t Elf64_Xword
#integral_t Elf64_Addr

#starttype Elf64_Ehdr
#array_field e_ident, CUChar
#field e_type,<Elf64_Half>
#field e_machine,<Elf64_Half>
#field e_version,<Elf64_Word>
#field e_entry,<Elf64_Addr>
#field e_phoff,<Elf64_Off>
#field e_shoff,<Elf64_Off>
#field e_flags,<Elf64_Word>
#field e_ehsize,<Elf64_Half>
#field e_phentsize,<Elf64_Half>
#field e_phnum,<Elf64_Half>
#field e_shentsize,<Elf64_Half>
#field e_shnum,<Elf64_Half>
#field e_shstrndx,<Elf64_Half>
#stoptype Elf64_Ehdr

#starttype Elf64_Shdr
#field sh_name,<Elf64_Word>
#field sh_type,<Elf64_Word>
#field sh_flags,<Elf64_Xword>
#field sh_addr,<Elf64_Addr>
#field sh_offset,<Elf64_Off>
#field sh_size,<Elf64_Xword>
#field sh_link,<Elf64_Word>
#field sh_info,<Elf64_Word>
#field sh_addralign,<Elf64_Xword>
#field sh_entsize,<Elf64_Xword>
#stoptype Elf64_Shdr

#starttype Elf64_Sym
#field st_name,<Elf64_Word>
#array_field st_info, CUChar
#array_field st_other, CUChar
#field st_shndx,<Elf64_Section>
#field st_value,<Elf64_Addr>
#field st_size,<Elf64_Xword>
#stoptype Elf64_Sym

#starttype Elf64_Rel
#field r_offset,<Elf64_Addr>
#field r_info,<Elf64_Xword>
#stoptype Elf64_Rel

#starttype Elf64_Dyn
#field d_tag,<Elf64_Sxword>
#field d_un,<Elf64_Xword>
#stoptype Elf64_Dyn

#starttype Elf64_Rela
#field r_offset,<Elf64_Addr>
#field r_info,<Elf64_Xword>
#field r_addend,<Elf64_Sxword>
#stoptype Elf64_Rela

#starttype Elf64_Syminfo
#field si_boundto,<Elf64_Half>
#field si_flags,<Elf64_Half>
#stoptype Elf64_Syminfo

#starttype Elf64_Phdr
#field p_type,<Elf64_Word>
#field p_flags,<Elf64_Word>
#field p_offset,<Elf64_Off>
#field p_vaddr,<Elf64_Addr>
#field p_paddr,<Elf64_Addr>
#field p_filesz,<Elf64_Xword>
#field p_memsz,<Elf64_Xword>
#field p_align,<Elf64_Xword>
#stoptype Elf64_Phdr

#starttype Elf64_Verdef
#field vd_version,<Elf64_Half>
#field vd_flags,<Elf64_Half>
#field vd_ndx,<Elf64_Half>
#field vd_cnt,<Elf64_Half>
#field vd_hash,<Elf64_Word>
#field vd_aux,<Elf64_Word>
#field vd_next,<Elf64_Word>
#stoptype Elf64_Verdef

#starttype Elf64_Verdaux
#field vda_name,<Elf64_Word>
#field vda_next,<Elf64_Word>
#stoptype Elf64_Verdaux

#starttype Elf64_Verneed
#field vn_version,<Elf64_Half>
#field vn_cnt,<Elf64_Half>
#field vn_file,<Elf64_Word>
#field vn_aux,<Elf64_Word>
#field vn_next,<Elf64_Word>
#stoptype Elf64_Verneed

#starttype Elf64_Vernaux
#field vna_hash,<Elf64_Word>
#field vna_flags,<Elf64_Half>
#field vna_other,<Elf64_Half>
#field vna_name,<Elf64_Word>
#field vna_next,<Elf64_Word>
#stoptype Elf64_Vernaux


#starttype Elf64_Move
#field m_value,<Elf64_Xword>
#field m_info,<Elf64_Xword>
#field m_poffset,<Elf64_Xword>
#field m_repeat,<Elf64_Half>
#field m_stride,<Elf64_Half>
#stoptype Elf64_Move
