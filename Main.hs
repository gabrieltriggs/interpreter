-- Main.hs
--
-- Triggs, Gabriel
-- CS 3515
-- 2014-5-1

import Lexer
import Parser
import Interpreter
import Types

main :: IO ()
main = print (foo "X")

run :: String -> Store -> Store
run = interpret . mainParser . lexer