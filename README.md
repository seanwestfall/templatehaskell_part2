Template Haskell Part 2
=====================

This is the second part to the [Template Haskell guide](https://github.com/seanwestfall/templatehaskell/blob/master/README.md) I wrote for Oliver Charles' [24 Days of GHC Extensions](https://ocharles.org.uk/blog/).

In the first part, I only gave an overfew of the syntax and Template Haskell's basic approach to meta-programming. In this second part, I'll cover quasi-quoting and also some more practical examples of how to use TH to create syntax extension -- including DSLs. I'll also cover Multi-stage Programming, an entire programming paradigm dedicated to meta-programming and Meta-oCaml -- a version of oCaml geared towards multi-stage programming and meta-programming.

---
_All code in this guide was executed with GHCi version 7.6.3 and Template Haskell version 2.9.0.0_

#### Quasi-Quotations
*Main.hs*
```haskell
{- Main.hs -}
module Main where

import Expr

main :: IO ()
main = do { print $ eval [$expr|1 + 2|]
          ; case IntExpr 1 of
              { [$expr|'int:n|] -> print n
              ;  _              -> return ()
              }
          }
```

*Expr.hs*
```haskell
{- Expr.hs -}
module Expr where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quasi

data Expr  =  IntExpr Integer
           |  AntiIntExpr String
           |  BinopExpr BinOp Expr Expr
           |  AntiExpr String
    deriving(Show, Typeable, Data)

data BinOp  =  AddOp
            |  SubOp
            |  MulOp
            |  DivOp
    deriving(Show, Typeable, Data)

eval :: Expr -> Integer
eval (IntExpr n)        = n
eval (BinopExpr op x y) = (opToFun op) (eval x) (eval y)
  where
    opToFun AddOp = (+)
    opToFun SubOp = (-)
    opToFun MulOp = (*)
    opToFun DivOp = div

expr = QuasiQuoter parseExprExp parseExprPat

-- Parse an Expr, returning its representation as
-- either a Q Exp or a Q Pat. See the referenced paper
-- for how to use SYB to do this by writing a single
-- parser of type String -> Expr instead of two
-- separate parsers.

parseExprExp :: String -> Q Exp
parseExprExp ...

parseExprPat :: String -> Q Pat
parseExprPat ...
```

An example of MetaHaskell using the power function:
```haskell
{-# LANGUAGE MetaHaskell #-}

import MiniML
import Language.MiniML.Pretty
import Text.PrettyPrint.Mainland

main :: IO ()
main = do
    print $ ppr $ erase $ power 11 [exp|x|]
    print $ ppr $ erase $ power' 11 [exp|x|]

power :: Int -> [exp|a :> Int|] -> [exp|a :> Int|]
power n x
    | n == 0     = [exp|1|]
    | n == 1     = [exp|$x|]
    | even n     = square (power (n `div` 2) x)
    | otherwise  = [exp|$x * $(power (n-1) x)|]
  where
    square :: [exp|a :> Int|] -> [exp|a :> Int|]
    square x = [exp|$(x) * $(x)|]

power' :: Int -> [exp|a :> Int|] -> [exp|a :> Int|]
power' n x
    | n == 0     = [exp|1|]
    | n == 1     = [exp|$x|]
    | even n     = square (power' (n `div` 2) x)
    | otherwise  = [exp|$x * $(power' (n-1) x)|]
  where
    square :: [exp|a :> Int|] -> [exp|a :> Int|]
    square x = [exp|let { y = $(x) } in y * y|]
```
