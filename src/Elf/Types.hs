{-# LANGUAGE RankNTypes, GADTs, PolyKinds, UndecidableInstances, DataKinds, KindSignatures, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, StandaloneDeriving #-}
module Elf.Types where
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Concurrent
import Control.Monad.Catch as Catch
import System.IO

import Foreign.Storable
import Elf.Constants
import Elf.ElfHeaders
import Data.Word
import Data.Int
import qualified Data.Bits as B
import Foreign.C.Types

-- | This specifies what the number of bits of a type or structure is.
-- There is a Elf16 format (Compact Elf), so it could be that B16
-- will be added to the list.
-- Bx says the number of bits of this structure is unknown or is some
-- local custom type.
-- This type may not be parametrized, because it is used as kind.

data Bits = B32 | B64 | Bx
     deriving (Show, Read, Eq, Ord)

-- | The reflected c types
data ElfTypes = ElfAddr
              | ElfOff
              | ElfSection
              | ElfVerSym
              | ElfByte
              | ElfHalf
              | ElfSword
              | ElfWord
              | ElfSxword
              | ElfXword
              | ElfString
    deriving (Show, Read, Eq)

-- | The class of elf files, Unknown is added to facilitate
-- newer values, it is pretty easy to add another class as third party
data ElfClass = ClassInvalid
              | Class32
              | Class64
              | ClassUnknown Word8
     deriving (Show, Read,Eq)

-- | The direction of bytes, Least significant byte, most significant byte.
-- There are probably no other models, but added it anyway.
data ElfData = DataUnknown
             | Data2LSB
             | Data2MSB
             | DataOther Word8
     deriving (Show, Read,Eq)

-- | The version of a Elf.
data ElfVersion = VersionInvalid
                | VersionCurrent
                | VersionOther Word8
     deriving (Show, Read, Eq)

-- | The OS ABI of the Elf, some platforms are added.
-- Others could be matched by comparing the value with
-- a constant.
data ElfAbi = AbiNone
            | AbiSysV
            | AbiHPUX
            | AbiNetBSD
            | AbiLinux
            | AbiSolaris
            | AbiIrix
            | AbiFreeBSD
            | AbiTru64
            | AbiArm
            | AbiStandAlone
            | AbiUnknown Word8
     deriving (Show, Eq, Read)

-- | A type family, which translates between the c types and the haskell types.
type family ElfType (b :: Bits) (c  :: ElfTypes)
type instance ElfType B64 ElfAddr = Word64
type instance ElfType B64 ElfOff = Word64
type instance ElfType b ElfSection = Word16
type instance ElfType b ElfVerSym = Word16
type instance ElfType b ElfByte = Word8
type instance ElfType b ElfHalf = Word16
type instance ElfType b ElfSword = Int32
type instance ElfType b ElfWord = Word32
type instance ElfType b ElfSxword = Int64
type instance ElfType b ElfXword = Word64
type instance ElfType b ElfString = [ElfType b ElfByte]

-- | Todo: should be removed.
type family NumBits x :: Bits
type instance NumBits (Ehdr B64 n ) = B64

-- | A version of MachineType, which is used to parametrize a type family, therefore it
-- contains only two values at the moment. More will be added.
data MachineType = X86_64
                 | I386
    deriving (Show, Eq, Read)
-- | Extension of MachineType to be used on the value level.
data Machine = KnownMachine MachineType
             | OtherMachine Word16
     deriving (Show, Eq, Read)

-- | The type of the file. Elf is a versatelite format.
data FileType = Rel
              | Exec
              | Dyn
              | Core
              | UnknownType
              | OtherType Word16
     deriving (Show, Eq, Read)

data ShType = ShNull
            | ShProgBits
            | ShSymTab
            | ShStrTab
            | ShRelA
            | ShHash
            | ShDynamic
            | ShNote
            | ShNobits
            | ShRel
            | ShShLib
            | ShDynSym
            | ShInitArray
            | ShFiniArray
            | ShPreInitArray
            | ShGroup
            | ShSymTabIndex
            | ShNum
            | ShLoos
            | ShGnuAttributes
            | ShGnuHash
            | ShGnuLibList
            | ShChecksum
            | ShLosunw
            | ShSunwMove
            | ShSunwComdat
            | ShSunwSyminfo
            | ShGnuVerdef
            | ShGnuVerneed
            | ShGnuVersym
            | ShHisunw
            | ShHios
            | ShLoproc
            | ShHiproc
            | ShLouser
            | ShHiuser
        deriving (Eq, Show)


instance Convertible s ShType where
  type From b ShType = ElfType b ElfWord
  toElf _ ShNull = c'SHT_NULL
  toElf _ ShProgBits = c'SHT_PROGBITS
  toElf _ ShSymTab = c'SHT_SYMTAB
  toElf _ ShStrTab = c'SHT_STRTAB
  toElf _ ShRelA = c'SHT_RELA
  toElf _ ShHash = c'SHT_HASH
  toElf _ ShDynamic = c'SHT_DYNAMIC
  toElf _ ShNote = c'SHT_NOTE
  toElf _ ShNobits = c'SHT_NOBITS
  toElf _ ShRel = c'SHT_REL
  toElf _ ShShLib = c'SHT_SHLIB
  toElf _ ShDynSym = c'SHT_DYNSYM
  toElf _ ShInitArray = c'SHT_INIT_ARRAY
  toElf _ ShFiniArray = c'SHT_FINI_ARRAY
  toElf _ ShPreInitArray = c'SHT_PREINIT_ARRAY
  toElf _ ShGroup = c'SHT_GROUP
  toElf _ ShSymTabIndex = c'SHT_SYMTAB_SHNDX
  toElf _ ShNum = c'SHT_NUM
  toElf _ ShLoos = c'SHT_LOOS
  toElf _ ShGnuAttributes = c'SHT_GNU_ATTRIBUTES
  toElf _ ShGnuHash = c'SHT_GNU_HASH
  toElf _ ShGnuLibList = c'SHT_GNU_LIBLIST
  toElf _ ShChecksum = c'SHT_CHECKSUM
  toElf _ ShHisunw = c'SHT_HISUNW
  toElf _ ShLosunw = c'SHT_LOSUNW
  toElf _ ShHios = c'SHT_HIOS
  toElf _ ShSunwMove = c'SHT_SUNW_move
  toElf _ ShSunwComdat = c'SHT_SUNW_COMDAT
  toElf _ ShSunwSyminfo = c'SHT_SUNW_syminfo
  toElf _ ShGnuVerdef = c'SHT_GNU_verdef
  toElf _ ShGnuVerneed = c'SHT_GNU_verneed
  toElf _ ShGnuVersym = c'SHT_GNU_versym
  toElf _ ShHiproc = c'SHT_HIPROC
  toElf _ ShLoproc = c'SHT_LOPROC
  toElf _ ShLouser = c'SHT_LOUSER
  toElf _ ShHiuser = c'SHT_HIUSER
  fromElf _ c | c == c'SHT_NULL = ShNull
              | c == c'SHT_PROGBITS = ShProgBits
              | c == c'SHT_SYMTAB = ShSymTab
              | c == c'SHT_STRTAB = ShStrTab
              | c == c'SHT_RELA = ShRelA
              | c == c'SHT_HASH = ShHash
              | c == c'SHT_DYNAMIC = ShDynamic
              | c == c'SHT_NOTE = ShNote
              | c == c'SHT_NOBITS = ShNobits
              | c == c'SHT_REL = ShRel
              | c == c'SHT_SHLIB = ShShLib
              | c == c'SHT_DYNSYM = ShDynSym
              | c == c'SHT_INIT_ARRAY = ShInitArray
              | c == c'SHT_FINI_ARRAY = ShFiniArray
              | c == c'SHT_PREINIT_ARRAY = ShPreInitArray
              | c == c'SHT_GROUP = ShGroup
              | c == c'SHT_SYMTAB_SHNDX = ShSymTabIndex
              | c == c'SHT_NUM = ShNum
              | c == c'SHT_LOOS = ShLoos
              | c == c'SHT_GNU_ATTRIBUTES = ShGnuAttributes
              | c == c'SHT_GNU_HASH = ShGnuHash
              | c == c'SHT_GNU_LIBLIST = ShGnuLibList
              | c == c'SHT_CHECKSUM = ShChecksum
              | c == c'SHT_LOSUNW = ShLosunw
              | c == c'SHT_SUNW_move = ShSunwMove
              | c == c'SHT_SUNW_COMDAT = ShSunwComdat
              | c == c'SHT_SUNW_syminfo = ShSunwSyminfo
              | c == c'SHT_GNU_verdef = ShGnuVerdef
              | c == c'SHT_GNU_verneed = ShGnuVerneed
              | c == c'SHT_GNU_versym = ShGnuVersym
              | c == c'SHT_HISUNW = ShHisunw
              | c == c'SHT_HIOS = ShHios
              | c == c'SHT_LOPROC = ShLoproc
              | c == c'SHT_HIPROC = ShHiproc
              | c == c'SHT_LOUSER = ShLouser
              | c == c'SHT_HIUSER = ShHiuser


-- | Type class with an associated type family, so we can translate between the c side and the haskell side.
class Eq c => Convertible s c where
      type From (b :: Bits) c
      -- | Go from the elf type to a common haskell type
      fromElf :: s -> From (NumBits s) c -> c
      -- | Go from a common haskell type to the c type
      toElf :: s -> c -> From (NumBits s) c
      -- | Use a low level operation to read a value from a structure and transform it into a haskell value.
      fromStructure ::  s -> (s -> From (NumBits s) c) -> c
      -- | Use a low level operation to read a value from a structure and transform it into a elf value.
      -- This operation seems somewhat strange, but in some cases, bindings-dsl chooses to represent
      -- a value as a CUInt, while the elf type is prefered.
      fromCoStructure :: s -> (s -> c) -> From (NumBits s) c
      fromStructure s f = fromElf s (f s)
      fromCoStructure s f = toElf s (f s)


instance Convertible s [CUChar] where
        type From b [CUChar] = [ElfType b ElfByte]
        fromElf s = fmap ( fromElf s)
        toElf s = fmap (toElf s)

instance Convertible s CUChar where
      type From b CUChar = Word8
      fromElf _ = CUChar
      toElf _ (CUChar w) = w

instance Convertible s ElfData where
     type From b ElfData = ElfType b ElfByte
     fromElf _ x | x == c'ELFDATANONE = DataUnknown
                 | x == c'ELFDATA2LSB = Data2LSB
                 | x == c'ELFDATA2MSB = Data2MSB
                 | otherwise = DataOther $ fromIntegral x
     toElf _ DataUnknown = c'ELFDATANONE
     toElf _ Data2LSB = c'ELFDATA2LSB
     toElf _ Data2MSB = c'ELFDATA2MSB
     toElf _ (DataOther x) = fromIntegral x

instance Convertible s ElfAbi where
     type From b ElfAbi = ElfType b ElfByte
     fromElf _ x | x == c'ELFOSABI_NONE = AbiNone
                 | x == c'ELFOSABI_SYSV = AbiSysV
                 | x == c'ELFOSABI_HPUX = AbiHPUX
                 | x == c'ELFOSABI_NETBSD = AbiNetBSD
                 | x == c'ELFOSABI_LINUX = AbiLinux
                 | x == c'ELFOSABI_SOLARIS = AbiSolaris
                 | x == c'ELFOSABI_IRIX = AbiIrix
                 | x == c'ELFOSABI_FREEBSD = AbiFreeBSD
                 | x == c'ELFOSABI_TRU64 = AbiTru64
                 | x == c'ELFOSABI_ARM = AbiArm
                 | x == c'ELFOSABI_STANDALONE = AbiStandAlone
                 | otherwise = AbiUnknown x
     toElf _ AbiNone = c'ELFOSABI_NONE
     toElf _ AbiSysV = c'ELFOSABI_SYSV
     toElf _ AbiHPUX = c'ELFOSABI_HPUX
     toElf _ AbiNetBSD = c'ELFOSABI_NETBSD
     toElf _ AbiLinux = c'ELFOSABI_LINUX
     toElf _ AbiSolaris = c'ELFOSABI_SOLARIS
     toElf _ AbiIrix = c'ELFOSABI_IRIX
     toElf _ AbiTru64 = c'ELFOSABI_TRU64
     toElf _ AbiArm = c'ELFOSABI_ARM
     toElf _ AbiStandAlone = c'ELFOSABI_STANDALONE
     toElf _ (AbiUnknown x) = x


instance Convertible s FileType where
  type From b FileType = ElfType B64 ElfHalf
  fromElf _ x | x == c'ET_NONE = UnknownType
              | x == c'ET_REL = Rel
                   | x == c'ET_EXEC = Exec
                   | x == c'ET_DYN = Dyn
                   | x == c'ET_CORE = Core
  toElf _ UnknownType = c'ET_NONE
  toElf _ Rel =  c'ET_REL
  toElf _ Exec =  c'ET_EXEC
  toElf _  Dyn =  c'ET_DYN
  toElf _ Core =  c'ET_CORE


instance Convertible s ElfVersion where
         type From b ElfVersion = ElfType b ElfByte
         fromElf _ x | x == c'EV_NONE = VersionInvalid
                     | x == c'EV_CURRENT = VersionCurrent
                     | otherwise = VersionOther x
         toElf _ VersionInvalid = c'EV_NONE
         toElf _ VersionCurrent = c'EV_CURRENT
         toElf _ (VersionOther x) = x

instance Convertible s ElfClass where
         type From b ElfClass = ElfType b ElfByte
         fromElf _ x | x == c'ELFCLASSNONE = ClassInvalid
                     | x == c'ELFCLASS32 = Class32
                     | x == c'ELFCLASS64 = Class64
                     | otherwise = ClassUnknown x
         toElf _ ClassInvalid = c'ELFCLASSNONE
         toElf _ Class32 = c'ELFCLASS32
         toElf _ Class64 = c'ELFCLASS64
         toElf _ (ClassUnknown x) = x
         fromStructure s f = fromElf s (f s)
         fromCoStructure s f = toElf s (f s)




instance Convertible s Word32 where
  type From b Word32 = ElfType b ElfWord
  fromElf _ n = n
  toElf _ n = n

instance Convertible s Int32 where
  type From b Int32 = ElfType b ElfSword
  fromElf _ s = s
  toElf _ s = s

instance Convertible s Word64 where
  type From b Word64 = ElfType b ElfXword
  fromElf _ = id
  toElf _ = id

instance Convertible s Int64 where
  type From b Int64 = ElfType b ElfSxword
  fromElf _ = id
  toElf _ = id
instance Convertible s Word16 where
  type From b Word16 = ElfType b ElfHalf
  fromElf _ = id
  toElf _ = id

instance Convertible s Word8 where
  type From b Word8 = ElfType b ElfByte
  fromElf _ = id
  toElf _ = id
instance Convertible s CUInt where
  type From b CUInt = ElfType b ElfWord
  fromElf _  = CUInt
  toElf _ (CUInt x) = x


instance Convertible s Machine where
       type From b Machine = ElfType b ElfHalf
       fromElf _ x | x == c'EM_X86_64 = KnownMachine X86_64
                   | x == c'EM_386 = KnownMachine I386
                   | otherwise = OtherMachine $ fromIntegral x
       toElf _ (KnownMachine X86_64) =  c'EM_X86_64
       toElf _ (KnownMachine I386) =  c'EM_386
       toElf _ (OtherMachine x) =  fromIntegral x



-- | The Ehdr structure is the entry header, which contain offsets
-- and pointers to all other structures.
-- It provides a somewhat higher level interface, than the
-- raw bindings.
class EhdrStructGetter (b :: Bits) (c :: MachineType) where
  data Ehdr b c :: *
  e_ident :: Ehdr b c -> ElfType b ElfString
  e_type :: Ehdr b c -> FileType
  e_machine :: Ehdr b c -> Machine
  e_version :: Ehdr b c -> ElfType b ElfWord
  e_entry :: Ehdr b c -> ElfType b ElfAddr
  e_phoff :: Ehdr b c -> ElfType b ElfOff
  e_shoff :: Ehdr b c -> ElfType b ElfOff
  e_flags :: Ehdr b c -> ElfType b ElfWord
  e_ehsize :: Ehdr b c -> ElfType b ElfHalf
  e_phentsize :: Ehdr b c -> ElfType b ElfHalf
  e_phnum :: Ehdr b c -> ElfType b ElfHalf
  e_shentsize :: Ehdr b c -> ElfType b ElfHalf
  e_shnum :: Ehdr b c -> ElfType b ElfHalf
  e_shstrndx :: Ehdr b c -> ElfType b ElfHalf

  -- further break down
  e_class :: Ehdr b c -> ElfClass
  e_magic_valid :: Ehdr b c -> Bool
  e_data :: Ehdr b c -> ElfData
  e_elf_version :: Ehdr b c -> ElfVersion
  e_abi :: Ehdr b c -> ElfAbi
  e_sections :: ShdrStructGetter b c => Ehdr b c -> [Shdr b c]


-- | First implementation of a  `EhdrStructGetter`
instance EhdrStructGetter B64 X86_64 where
  data Ehdr B64 X86_64 = X8664 C'Elf64_Ehdr
  e_ident (X8664 p) = fromCoStructure p c'Elf64_Ehdr'e_ident
  e_type (X8664 p) = fromStructure p c'Elf64_Ehdr'e_type
  e_machine (X8664 p) = fromStructure p c'Elf64_Ehdr'e_machine
  e_version(X8664 p) =  fromCoStructure p c'Elf64_Ehdr'e_version
  e_entry (X8664 p) = fromCoStructure p c'Elf64_Ehdr'e_entry
  e_phoff (X8664 p) = fromCoStructure p c'Elf64_Ehdr'e_phoff
  e_shoff (X8664 p) = fromCoStructure p c'Elf64_Ehdr'e_shoff
  e_flags (X8664 p) = fromCoStructure p c'Elf64_Ehdr'e_flags
  e_ehsize (X8664 p) = fromCoStructure p c'Elf64_Ehdr'e_ehsize
  e_phentsize (X8664 p) = fromCoStructure p c'Elf64_Ehdr'e_phentsize
  e_phnum (X8664 p) = fromCoStructure p c'Elf64_Ehdr'e_phnum
  e_shentsize (X8664 p) = fromCoStructure p c'Elf64_Ehdr'e_shentsize
  e_shnum (X8664 p) = fromCoStructure p c'Elf64_Ehdr'e_shnum
  e_shstrndx (X8664 p) = fromCoStructure p c'Elf64_Ehdr'e_shstrndx
  e_class x = fromElf x ( e_ident x !! c'EI_CLASS)
  e_magic_valid x = b0 == 0x7f && b1 == 0x45 && b2 == 0x4c && b3 == 0x46
      where (b0, b1,b2,b3) = let xs = e_ident x in (xs !! c'EI_MAG0, xs !! c'EI_MAG1, xs !! c'EI_MAG2, xs !! c'EI_MAG3)
  e_data x = fromElf x (e_ident x !! c'EI_DATA)
  e_elf_version x = fromElf x (e_ident x !! c'EI_VERSION)
  e_abi x = fromElf x (e_ident x !! c'EI_OSABI)

-- | Section header stub
class ShdrStructGetter (b :: Bits) (m :: MachineType) where
  data Shdr b m :: *
  sh_name :: Shdr b m -> ElfType b ElfString
  sh_type :: Shdr b m -> ShType

-- | Program segment header stub.
class PhdrStructGetter (b :: Bits) (m :: MachineType) where
  data Phdr b m :: *



-- | A stub class for a monadic interface on elf files.
class ElfOp (m :: * -> * -> *) where
  withHandle :: (Handle -> m r a) -> m r a
  loadElf :: FilePath -> m r ()
  runElf :: MonadIO t => m r a -> t a

instance ElfOp (ElfMonad) where
  loadElf fp = undefined
  runElf = undefined
  withHandle f = do
            m <- asks filehandle
            Catch.bracket (liftIO $ takeMVar m) (liftIO . putMVar m) f



data ElfEnv = ElfEnv {
            filehandle :: MVar Handle
        }

newtype ElfMonad s a = ElfMonad {
                   runElfMonad :: ReaderT ElfEnv IO a
                        }
instance Functor (ElfMonad s) where
        fmap f = ElfMonad . fmap f . runElfMonad

instance Applicative (ElfMonad s) where
         pure = return
         (<*>) = ap

instance Alternative (ElfMonad s) where
         empty = mzero
         (<|>) = mplus

instance Monad (ElfMonad s) where
         return = ElfMonad . return
         (>>=) m f = ElfMonad (runElfMonad m >>= (runElfMonad . f))

instance MonadPlus (ElfMonad s) where
         mzero = ElfMonad mzero
         mplus m n = ElfMonad $ runElfMonad m `mplus` runElfMonad n

instance MonadIO (ElfMonad s) where
         liftIO  = ElfMonad . liftIO

instance MonadReader ElfEnv (ElfMonad s) where
         ask = ElfMonad ask
         local f = ElfMonad . local f . runElfMonad

instance MonadThrow (ElfMonad s) where
        throwM = ElfMonad . throwM

instance MonadCatch (ElfMonad s) where
        catch m f = ElfMonad (catch (runElfMonad m) (runElfMonad . f))

instance MonadMask (ElfMonad s) where
    mask f = ElfMonad (mask $ \u -> runElfMonad (f ( ElfMonad . u . runElfMonad)))
    uninterruptibleMask f = ElfMonad (uninterruptibleMask $ \u -> runElfMonad (f ( ElfMonad . u . runElfMonad)))
