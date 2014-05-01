-- Tests.hs
--
-- Triggs, Gabriel
-- CS 3515
-- 2014-5-1

module Tests where

import Interpreter
import Parser
import Lexer
import Types

-- Interpreter Tests
testInterpreter :: IO ()
testInterpreter = do
					putStrLn "Interpreter Test 1"
					putStrLn "Expected: X = 0, Y = 0, Z = 16"
					putStr "Actual:   "
					putStr ("X = " ++ ((show . interpreter1) "X"))
					putStr (", Y = " ++ ((show . interpreter1) "Y"))
					putStrLn (", Z = " ++ ((show . interpreter1) "Z"))
					putStrLn "\nInterpreter Test 2"
					putStrLn "Expected: X = 2, Y = 2"
					putStr "Actual:   "
					putStr ("X = " ++ ((show . interpreter2) "X"))
					putStrLn (", Y = " ++ ((show . interpreter2) "Y"))
					putStrLn "\nInterpreter Test 3"
					putStrLn "Expected: X = 2, Y = 1, Z = 5"
					putStr "Actual:   "
					putStr ("X = " ++ ((show . interpreter3) "X"))
					putStr (", Y = " ++ ((show . interpreter3) "Y"))
					putStrLn (", Z = " ++ ((show . interpreter3) "Z"))
					putStrLn "\nInterpreter Test 4"
					putStrLn "Expected: X = 1, Y = -2"
					putStr "Actual:   "
					putStr ("X = " ++ ((show . interpreter4) "X"))
					putStrLn (", Y = " ++ ((show . interpreter4) "Y"))
					putStrLn "\nInterpreter Test 5"
					putStrLn "Expected: X = 0, Y = 0, Z = 100"
					putStr "Actual:   "
					putStr ("X = " ++ ((show . interpreter5) "X"))
					putStr (", Y = " ++ ((show . interpreter5) "Y"))
					putStrLn (", Z = " ++ ((show . interpreter5) "Z"))
					putStrLn "\n"

interpreter1 :: Store
interpreter1 = interpret 
				(Seq (Assign "X" (Const 2))
	 				 (Seq (Assign "Y" (Const 0))
		  				  (Seq (Assign "Z" (Const 2))
			   				   (While (Greater (Var "X") (Var "Y"))
				  					  (Seq (Assign "Z" (Times (Var "Z") 
				  					 						  (Var "Z")))
						   				   (Assign "X" (Minus (Var "X") 
						   				  					  (Const 1))))))))
				initial

interpreter2 :: Store
interpreter2 = interpret
				(Seq (Assign "X" (Const 1))
	 				 (Seq (Assign "Y" (Const 2))
		  				  (Cond (Greater (Var "X") (Var "Y"))
			    				(Assign "Z" (Var "X"))
			    				(Assign "X" (Var "Y")))))
				initial

interpreter3 :: Store
interpreter3 = interpret
				(Seq (Assign "X" (Const 2))
	 				 (Seq (Assign "Y" (Const 1))
		  				  (Seq (Assign "Z" (Const 3))
			   				   (Cond (Greater (Var "X") (Var "Y"))
				  	 				 (Cond (Greater (Var "Y") (Var "Z"))
				 	       				   (Assign "Z" (Const 4))
				 	       				   (Assign "Z" (Const 5)))
				     				 (Assign "Z" (Const 6))))))
				initial

interpreter4 :: Store
interpreter4 = interpret
				(Seq (Assign "X" (Const 1))
	 				 (Assign "Y" (Times (Minus (Const 0) (Var "X")) 
	 									(Const 2))))
				initial

interpreter5 :: Store
interpreter5 = interpret
				(Seq (Assign "X" (Const 10))
	 				 (While (Greater (Var "X") (Const 0))
	 						(Seq (Assign "Y" (Const 10))
	 			 				 (Seq (While (Greater (Var "Y") (Const 0))
	 			 					  (Seq (Assign "Z" (Minus (Var "Z") 
	 			 					  				   (Minus (Const 0) 
	 			 					  				   		  (Const 1))))
	 			 			 			   (Assign "Y" (Minus (Var "Y") 
	 			 			 			   					  (Const 1)))))
	 			 	  			 	  (Assign "X" (Minus (Var "X") 
	 			 	  			 	  					 (Const 1)))))))
				initial

-- Parser Tests
testParser :: IO ()
testParser = do
				putStrLn "Parser Test 1"
				putStrLn "Command generated from token list 1:"
				putStrLn (show parser1)
				putStrLn "\nParser Test 2"
				putStrLn "Command generated from token list 2:"
				putStrLn (show parser2)
				putStrLn "\nParser Test 3"
				putStrLn "Command generated from token list 3:"
				putStrLn (show parser3)
				putStrLn "\nParser Test 4"
				putStrLn "Command generated from token list 4:"
				putStrLn (show parser4)
				putStrLn "\nParser Test 5"
				putStrLn "Command generated from token list 5:"
				putStrLn (show parser5)
				putStrLn "\n"

parser1 :: Command
parser1 = mainParser [Ident "X", Ident ":=", Number 2, Ident ";", 
						Ident "Y", Ident ":=", Number 0, Ident ";", 
						Ident "Z", Ident ":=", Number 2, Ident ";", 
						Symbol "WHILE", Ident "X", Ident ">", Ident "Y", 
						Symbol "DO", Ident "Z", Ident ":=", Ident "Z", 
						Ident "*", Ident "Z", Ident ";", Ident "X", 
						Ident ":=", Ident "X", Ident "-", Number 1, 
						Symbol "END"]

parser2 :: Command
parser2 = mainParser [Ident "X", Ident ":=", Number 1, Ident ";", Ident "Y", 
						Ident ":=", Number 2, Ident ";", Symbol "IF", 
						Ident "X", Ident ">", Ident "Y", Symbol "THEN", 
						Ident "Z", Ident ":=", Ident "X", Symbol "ELSE", 
						Ident "X", Ident ":=", Ident "Y", Symbol "ENDIF"]

parser3 :: Command
parser3 = mainParser [Ident "X", Ident ":=", Number 2, Ident ";", Ident "Y", 
						Ident ":=", Number 1, Ident ";", Ident "Z", 
						Ident ":=", Number 3, Ident ";", Symbol "IF", 
						Ident "X", Ident ">", Ident "Y", Symbol "THEN", 
						Symbol "IF", Ident "Y", Ident ">", Ident "Z", 
						Symbol "THEN", Ident "Z", Ident ":=", Number 4, 
						Symbol "ELSE", Ident "Z", Ident ":=", Number 5, 
						Symbol "ENDIF", Symbol "ELSE", Ident "Z", Ident ":=", 
						Number 6, Symbol "ENDIF"]

parser4 :: Command
parser4 = mainParser [Ident "X", Ident ":=", Number 1, Ident ";", Ident "Y", 
						Ident ":=", Symbol "(", Number 0, Ident "-", 
						Ident "X", Symbol ")", Ident "*", Number 2]

parser5 :: Command
parser5 = mainParser [Ident "X", Ident ":=", Number 10, Ident ";", 
						Symbol "WHILE", Ident "X", Ident ">", Number 0, 
						Symbol "DO", Ident "Y", Ident ":=", Number 10, 
						Ident ";", Symbol "WHILE", Ident "Y", Ident ">", 
						Number 0, Symbol "DO", Ident "Z", Ident ":=", 
						Ident "Z", Ident "-", Symbol "(", Number 0, 
						Ident "-", Number 1, Symbol ")", Ident ";", Ident "Y", 
						Ident ":=", Ident "Y", Ident "-", Number 1, 
						Symbol "END", Ident ";", Ident "X", Ident ":=", 
						Ident "X", Ident "-", Number 1, Symbol "END"]

-- Lexer Tests
testLexer :: IO ()
testLexer = do
				putStrLn "Lexer Test 1"
				putStrLn "Token list generated from program string 1:"
				putStrLn (show lexer1)
				putStrLn "\nLexer Test 2"
				putStrLn "Token list generated from program string 2:"
				putStrLn (show lexer2)
				putStrLn "\nLexer Test 3"
				putStrLn "Token list generated from program string 3:"
				putStrLn (show lexer3)
				putStrLn "\nLexer Test 4"
				putStrLn "Token list generated from program string 4:"
				putStrLn (show lexer4)
				putStrLn "\nLexer Test 5"
				putStrLn "Token list generated from program string 5:"
				putStrLn (show lexer5)
				putStrLn "\nLexer Test 6"
				putStrLn "Token list generated from program string 6:"
				putStrLn (show lexer6)
				putStrLn "\n"

lexer1 :: [Token]
lexer1 = lexer program1

lexer2 :: [Token]
lexer2 = lexer program2

lexer3 :: [Token]
lexer3 = lexer program3

lexer4 :: [Token]
lexer4 = lexer program4

lexer5 :: [Token]
lexer5 = lexer program5

lexer6 :: [Token]
lexer6 = lexer program6

-- Program strings
program1 :: String
program1 = "X := 2;        \
		   \Y := 0; 	   \
		   \Z := 2;        \
		   \WHILE X > Y DO \
	       \    Z := Z * Z;\
		   \    X := X - 1 \
		   \END"

program2 :: String
program2 = "X := 1;			\
		   \Y := 2;		    \
		   \IF X > Y		\ 
		   \	THEN Z := X \
		   \	ELSE X := Y \
		   \ENDIF"

program3 :: String
program3 = "X := 2;					 \
		   \Y := 1;					 \
		   \Z := 3;					 \
		   \IF X > Y				 \
		   \	THEN IF Y > Z		 \
		   \			 THEN Z := 4 \
		   \			 ELSE Z := 5 \
		   \		 ENDIF			 \
		   \    ELSE Z := 6			 \
		   \ENDIF"

program4 :: String
program4 = "X := 1; 		\
		   \Y := (0 - X) * 2"

program5 :: String
program5 = "X := 10;				  \
		   \WHILE X > 0 DO 			  \
		   \	Y := 10;			  \
		   \	WHILE Y > 0 DO 		  \
		   \		Z := Z - (0 - 1); \
		   \ 		Y := Y - 1 		  \
		   \	END;				  \
		   \	X := X - 1 			  \
		   \END"

program6 :: String
program6 = "WHILE X > Y DO X := X - 1; Z := Z * Z END"