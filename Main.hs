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