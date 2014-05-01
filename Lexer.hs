-- Lexer.hs
--
-- Triggs, Gabriel
-- CS 3515
-- 2014-5-1

module Lexer where

import Types
import Data.Char

keyword :: String -> Bool
keyword "IF"    = True
keyword "THEN"  = True
keyword "ELSE"  = True
keyword "ENDIF" = True
keyword "WHILE" = True
keyword "DO"    = True
keyword "END"   = True
keyword _		= False

keycheck :: String -> Token
keycheck s
		| keyword s = Symbol s
		| otherwise = Ident s

letDigEtc :: Char -> Bool
letDigEtc '\'' = True
letDigEtc '_'  = True
letDigEtc c    = isLetter c || isDigit c

layout :: Char -> Bool
layout ' '  = True
layout '\n' = True
layout '\t' = True
layout _ 	= False

symbolChar :: Char -> Bool
symbolChar '*' = True
symbolChar '-' = True
symbolChar '>' = True
symbolChar ':' = True
symbolChar '=' = True
symbolChar ';' = True
symbolChar _   = False

intOfDigit :: Char -> Int
intOfDigit = digitToInt

lexer :: String -> [Token]
lexer [] = []
lexer (a:x) = if layout a then lexer x else
				if a == '(' then Symbol "(" : (lexer x) else
				  if a == ')' then Symbol ")" : (lexer x) else
				    if isLetter a then getword [a] x else
				      if isDigit a then getnum (intOfDigit a) x else
				      	if symbolChar a then getsymbol [a] x else
				      	  error ("Lexical error : unrecognized token " ++ (a:x))

getword :: String -> String -> [Token]
getword l [] = [keycheck (reverse l)]
getword l (a:x) = if letDigEtc a then getword (a:l) x
					else (keycheck (reverse l)) : (lexer (a:x))

getsymbol :: String -> String -> [Token]
getsymbol l [] = [keycheck (reverse l)]
getsymbol l (a:x) = if symbolChar a then getsymbol (a:l) x
					  else (keycheck (reverse l)) : (lexer (a:x))

getnum :: Int -> String -> [Token]
getnum n [] = [Number n]
getnum n (a:x)
			| isDigit a = getnum (n * 10 + (intOfDigit a)) x
			| otherwise = Number n : (lexer (a:x))