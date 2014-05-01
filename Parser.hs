-- Parser.hs
--
-- Triggs, Gabriel
-- CS 3515
-- 2014-5-1

module Parser where

import Combinators
import Types

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