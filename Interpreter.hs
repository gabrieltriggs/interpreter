-- Interpreter.hs
--
-- Triggs, Gabriel
-- CS 3515
-- 2014-5-1
-- Contains code relevant to the interpreting phase.

module Interpreter where

import Types

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