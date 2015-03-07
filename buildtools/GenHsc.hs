module Main where
-- = Preprocessor for elf headers
--
-- | This is a simple preprocessor for c header files.
-- It will accept a header, and try to differentiate between constants and macro's,
-- then it will create three files, which can be concatenated with a template file,
-- which loads the appropiate binding-dsl headers:
--
--       1. For the constants. This is a rather simple conversion.
--       2. For the macros, on the haskell side.
--       3. For the macros, on the c side.
--
-- regenerate_constants.sh will merge them together with the appropiate template and
-- copy them to the src dir, where cabal can find them.
--
-- == /Potential pitfalls/
--
-- It makes some assumptions (and therefore can break):
--
--       1. Macros only deal with numbers (in this particular case, quite reasonable)
--       2. The macros won't rely on overflow (doomed to fail)
--       3. Word64 is fine choice for all macro parameters and return types (probably not).
--
-- If assumption 2 and 3 are causing trouble, it is difficult to fix, because a macro is untyped and
-- it is not easy to type them from the comments or by other means.
--
-- Platform specific code is probably needed, but for now, lets hope for the best.
-- If assumption 1 is causing trouble, it will break the type system (yikes), but little can be done
-- about it. Tests are needed to ensure the safety of this library.

import Language.Preprocessor.Cpphs
import System.Environment
import Control.Monad
import Control.Applicative hiding (many, some)
import Data.List
import Control.Arrow
import Text.Parsec hiding (newline)
import Text.Parsec.String

data SimpleExpr = Grouped [SimpleExpr]
                | Number Integer
                | BinOp String SimpleExpr SimpleExpr
                | Op String SimpleExpr
                | Parameter String
     deriving Show
type Arg = Name
type Args = [Arg]
data FuncSpec = FuncSpec Name Args
     deriving (Show, Read)

parseFuncName :: Name -> Either ParseError FuncSpec
parseFuncName nm = parse funcSpec fn nm
              where fn = case parse funcName nm nm  of
                           Left _ -> nm
                           Right x -> x

funcSpec :: Parser FuncSpec
funcSpec = FuncSpec <$> funcName <*> between (char '(' <*  spaces) (spaces *> char ')') argsP

funcName :: Parser Name
funcName = (:) <$> oneOf alpha <*> (many alphaNumP)

alphaP :: Parser Char
alphaP = oneOf alpha
numP :: Parser Char
numP = oneOf num
alphaNumP = oneOf (alpha ++ num)


argsP :: Parser Args
argsP = argP `sepBy` (spaces *> char ',' <* spaces)


argP :: Parser Arg
argP = funcName

alpha = '_' : (['A' .. 'Z'] ++ ['a' .. 'z'])
num = ['0' .. '9']

type Name = String
type Body = String
main :: IO ()
main = do
   xs <- getArgs
   case xs of
     [fp] -> do
        ts <- readFile fp
        (_, ts) <- macroPassReturningSymTab [] (defaultBoolOptions {lang = False}) $ addPosns fp ts
        let (cst, mst) = transform ts
        -- printing constants
        let constants = (uncurry constantsParser) <$> cst
        let cdecls = (uncurry macroParserC) <$> mst
        let hdecls = (uncurry macroParserHs) <$> mst
        writeFile "/tmp/constants.txt" (unlines $ sort $ nub constants)
        writeFile "/tmp/cdecls.txt" (unlines cdecls)
        writeFile "/tmp/hdecls.txt" (unlines hdecls)


transform :: [(String, String)] -> ([(Name, Body)], [(Name, Body)])
transform = foldr step ([],[])
               where step x (cst, mcr) | '(' `elem` fst x = (cst, x:mcr)
                                       | otherwise = (x : cst, mcr)

addPosns fn  = fst .  foldr step ([],newfile fn) . lines
             where step x (rs,pos) = ((pos, x):rs, newline pos)
macroParserHs :: Name -> Body -> String
macroParserHs nm bd = newHsInline fn
        where fn = case parseFuncName nm of
                        Left e -> error (show e)
                        Right a -> a

macroParserC :: Name -> Body -> String
macroParserC nm bdy = newInline fn
        where fn = case parseFuncName nm of
                        Left e -> error (show e)
                        Right a -> a

newHsInline (FuncSpec nm xs) = "#cinline " ++ nm ++ ", " ++ intercalate " -> " (replicate ln "Word64")

            where ln = length xs + 1
newInline (FuncSpec nm xs) = "BC_INLINE" ++ (show ln) ++ "(" ++ nm ++ ", " ++ intercalate ", " (replicate (succ ln) "uint64_t") ++ ")"
          where ln = length xs

constantsParser :: Name -> Body -> String
constantsParser nm bdy = "#num " ++ nm
