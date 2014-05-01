-- Interpreter.hs
--
-- Triggs, Gabriel
-- CS 3515
-- 2014-5-1

import Combinators
import Types
import Data.Char

main :: IO ()
main = print (foo "X")

fetch :: Store -> Variable -> Val
fetch s x = s x

update :: Store -> Variable -> Val -> Store
update s x v = \y -> if y == x then v else fetch s y

initial :: Store
initial = \_ -> 0

eval :: Expr -> Store -> Val
eval (Const v) _       = v
eval (Var x) s         = fetch s x
eval (Minus e1 e2) s   = eval e1 s - eval e2 s
eval (Times e1 e2) s   = eval e1 s * eval e2 s
eval (Greater e1 e2) s = if eval e1 s > eval e2 s then 1 else 0

interpret :: Command -> Store -> Store
interpret (Assign x e) s   = update s x (eval e s)
interpret (Seq c1 c2) s    = interpret c2 (interpret c1 s)
interpret (Cond e c1 c2) s = switch (eval e s)
                                (interpret c1) (interpret c2) s
interpret (While e c) s    = switch (eval e s) (interpret (Seq c (While e c))) 
								id s

switch :: Val -> (Store -> Store) -> (Store -> Store) -> Store -> Store
switch 1 f _ s = f s
switch 0 _ g s = g s

number :: Parser Val
number (Number n : s) = Just (n, s)
number _			  = Nothing

variable :: Parser Variable
variable (Ident v : s) = Just (v, s)
variable _			   = Nothing

literal :: String -> Parser String
literal w (Symbol x : s) = if w == x then Just (x, s) else Nothing
literal w (Ident x : s) = if w == x then Just (x, s) else Nothing
literal _ _ = Nothing

-- maybe use where definitions for optFoo
expr :: Parser Expr
expr = (aexp <&> optional (literal ">" <&> aexp)) `modify` optGreater
		where
			optGreater :: (Expr,[(a,Expr)]) -> Expr
			optGreater (e1, []) 		= e1
			optGreater (e1, [(gt, e2)]) = Greater e1 e2
			optGreater _ 				= error "impossible"

aexp :: Parser Expr
aexp = (bexp <&> optional (literal "-" <&> aexp)) `modify` optSub
		where
			optSub :: (Expr,[(a,Expr)]) -> Expr
			optSub (e1, []) 	   = e1
			optSub (e1, [(m, e2)]) = Minus e1 e2
			optSub _ 			   = error "impossible"

bexp :: Parser Expr
bexp = (cexp <&> optional (literal "*" <&> bexp)) `modify` optMul
		where
			optMul :: (Expr,[(a,Expr)]) -> Expr
			optMul (e1, []) 	   = e1
			optMul (e1, [(m, e2)]) = Times e1 e2
			optMul _ 			   = error "impossible"

cexp :: Parser Expr
cexp = (literal "(" <&> (expr <&> literal ")")) `modify` unparenth
	   <|> (number `modify` (\n -> (Const n)))
	   <|> (variable `modify` (\x -> (Var x)))
	   	where 
			unparenth :: (a,(b,c)) -> b
			unparenth (op, (e, cp)) = e
			-- unparenth _ 			= error "impossible"

command :: Parser Command
command = (unitcom <&> optional (literal ";" <&> command)) `modify` optSeq
			where
				unitcom :: Parser Command
				unitcom = whilecom <|> (ifcom <|> assign)

				optSeq :: (Command,[(a,Command)]) -> Command
				optSeq (c1, []) 			 = c1
				optSeq (c1, [(semicol, c2)]) = Seq c1 c2
				optSeq _ 					 = error "impossible"

whilecom :: Parser Command
whilecom = (literal "WHILE" <&> (expr <&> (literal "DO" 
				<&> (command <&> (literal "END"))))) `modify` mkWhileNode
			where
				mkWhileNode :: (String, (Expr, (String, (Command, String)))) 
									-> Command
				mkWhileNode (_, (e, (_, (c, _)))) = While e c

ifcom :: Parser Command
ifcom = (literal "IF" <&> (expr <&> (literal "THEN" <&> (command
				<&> (literal "ELSE" <&> (command <&> (literal "ENDIF")))))))
					`modify` mkIfNode
			where
				mkIfNode :: (String, (Expr, (String, (Command, 
								(String, (Command, String)))))) -> Command
				mkIfNode (_, (e, (_, (c1, (_, (c2, _)))))) = Cond e c1 c2

assign :: Parser Command
assign = (variable <&> (literal ":=" <&> expr)) `modify` mkAssignNode
			where
				mkAssignNode :: (Variable, (String, Expr)) -> Command
				mkAssignNode (v, (s, e)) = Assign v e

lit :: Token -> String
lit (Ident s)  = s ++ " "
lit (Symbol s) = s ++ " "
lit (Number s) = show s ++ " "

report :: Maybe (a, [Token]) -> a
report Nothing = error "Parse error"
report (Just (c, [])) = c
report (Just (c, xs)) = error (stringwith
								("Syntax error \n Unparsed:-\n",
								 " ",
								 "\n")
								 (map lit xs))
						where
							stringwith (front, sep, back) ls =
								let 
									sepback [] 	   = back
									sepback [a]    = a ++ back
									sepback (a:xs) = a ++ sep ++ sepback xs
								in 
									front ++ sepback ls

mainParser :: [Token] -> Command
mainParser = report . command

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

run :: String -> Store -> Store
run = interpret . mainParser . lexer

foo = run "X := 2; Y := 0; Z := 2; WHILE X > Y DO Z := Z * Z; X := X - 1 END" initial
bar = run "X := 1; Y := 2; IF X > Y THEN Z := X ELSE Z := Y ENDIF" initial
baz = run "X := 4; X := 2 * X" initial
quux = run "X := 2; X := X - X * 2" initial
quiz = run "X := 2; X := X - X - X" initial