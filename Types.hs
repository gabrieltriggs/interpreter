-- Types.hs
--
-- Triggs, Gabriel
-- CS 3515
-- 2014-5-1

module Types where

type Variable = String

type Val      = Int

data Expr     = Const Val
              | Var Variable
              | Minus Expr Expr
              | Times Expr Expr
              | Greater Expr Expr
                     deriving Show
                     
data Command  = Assign Variable Expr
              | Seq Command Command
              | Cond Expr Command Command
              | While Expr Command
                     deriving Show

type Store    = Variable -> Val

data Token    = Ident String
			  | Number Int
			  | Symbol String
			  	deriving Show
			  	
type Parser a = [Token] -> Maybe (a, [Token])