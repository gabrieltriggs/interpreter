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

foo = run "X := 2; Y := 0; Z := 2; WHILE X > Y DO Z := Z * Z; X := X - 1 END" initial
bar = run "X := 1; Y := 2; IF X > Y THEN Z := X ELSE Z := Y ENDIF" initial
baz = run "X := 4; X := 2 * X" initial
quux = run "X := 2; X := X - X * 2" initial
quiz = run "X := 2; X := X - X - X" initial