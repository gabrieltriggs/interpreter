-- Main.hs
--
-- Triggs, Gabriel
-- CS 3515
-- 2014-5-1

import Lexer
import Parser
import Interpreter
import Tests
import Types

main :: IO ()
main = do
		testInterpreter
		testParser
		testLexer
		testWhole

run :: String -> Store -> Store
run = interpret . mainParser . lexer

-- Tests of entire program
testWhole :: IO ()
testWhole = do
				putStrLn "Complete Test 1"
				putStrLn "Expected: X = 0, Y = 0, Z = 16"
				putStr "Actual:   "
				putStr ("X = " ++ (show (run program1 initial "X")))
				putStr (", Y = " ++ (show (run program1 initial "Y")))
				putStrLn (", Z = " ++ (show (run program1 initial "Z")))
				putStrLn "\nComplete Test 2"
				putStrLn "Expected: X = 2, Y = 2"
				putStr "Actual:   "
				putStr ("X = " ++ (show (run program2 initial "X")))
				putStrLn (", Y = " ++ (show (run program2 initial "Y")))
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