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

data Bits = B32 | B64 | Bx
     deriving (Show, Read, Eq, Ord)
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

data ElfClass = ClassInvalid
              | Class32
              | Class64
              | ClassUnknown Word8
     deriving (Show, Read,Eq)

data ElfData = DataUnknown
             | Data2LSB
             | Data2MSB
             | DataOther Word8
     deriving (Show, Read,Eq)

data ElfVersion = VersionInvalid
                | VersionCurrent
                | VersionOther Word8
     deriving (Show, Read, Eq)

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

type family NumBits x :: Bits
type instance NumBits (Ehdr B64 n ) = B64

data MachineType = X86_64
                 | I386
    deriving (Show, Eq, Read)
type family BitsExtract c :: Bits

data Machine = KnownMachine MachineType
             | OtherMachine Word16

deriving instance Show Machine
deriving instance Eq Machine

data FileType = Rel
              | Exec
              | Dyn
              | Core
              | UnknownType
              | OtherType Word16
     deriving (Show, Eq, Read)
-- class was:
-- class Convertible s c (b :: Bits) (n :: ElfByte) | s -> c, s -> b, c -> n
--
class Eq c => Convertible s c where
      type From (b :: Bits) c
      fromElf :: s -> From (NumBits s) c -> c
      toElf :: s -> c -> From (NumBits s) c
      fromStructure ::  s -> (s -> From (NumBits s) c) -> c
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
  fromElf _ n = undefined
  toElf _ = undefined

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


class ShdrStructGetter (b :: Bits) (m :: MachineType) where
  data Shdr b m :: *


-- Elf monad

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
