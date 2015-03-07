module Main where

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
