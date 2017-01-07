{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module BoTox.Utils.DiceParser ( Expr(..)
                              , parseDice
                              , evalDice
                              , evalDiceStr
                              , diceParser) where

import BoTox.Utils
import Control.Applicative ((<$>), (<*), (*>), (<|>))
import Control.Monad (foldM, liftM2, liftM)
import Control.Monad.State (State, get, put, runState, replicateM)
import Data.List (intercalate)
import System.Random (Random, StdGen, randomR)
import Text.Parsec (many1, digit, char, parse, try, Stream(..), ParsecT, ParseError, eof, optionMaybe)
import Text.Parsec.Char (oneOf, string)
import Text.Parsec.Expr (Assoc(..), Operator(..), buildExpressionParser)

-- Randomness setup for dice roll --

getRandomR :: Random a => (a, a) -> State StdGen a
getRandomR limits = do
  gen <- get
  let (val, gen') = randomR limits gen
  put gen'
  return val

-- AST --

-- | Dice Type
data Dice = Dice Int  | -- ^ Regular dice (0, n)
            UDice Int   -- ^ Fudge dice (-n, n)
            deriving (Show)

-- | Expression AST types
data Expr = Lit Int       | -- ^ An integer literal
            Add Expr Expr | -- ^ Binary addition
            Sub Expr Expr | -- ^ Binary subtraction
            Mul Expr Expr | -- ^ Binary multiplication
            Div Expr Expr | -- ^ Binary integer division
            Rol Dice Int    -- ^ Dice roll
            deriving (Show)

-- | Recursively evaluates the AST to its value inside a 'State' monad with
-- a random generator
eval :: Expr -> State StdGen Int
eval (Lit i)            = return i
eval (Add e1 e2)        = liftM2 (+) (eval e1) (eval e2)
eval (Sub e1 e2)        = liftM2 (-) (eval e1) (eval e2)
eval (Mul e1 e2)        = liftM2 (*) (eval e1) (eval e2)
eval (Div e1 e2)        = liftM2 div (eval e1) (eval e2)

-- Evaluates dices and sides and accumulates over choosing random numbers between
-- 1 and sides, dice times
eval (Rol d num)     = foldM (\s _ -> liftM (s +) $ roll d) 0 [1..num]

evalStr :: Expr -> State StdGen String
evalStr (Lit i)     = return $ show i
evalStr (Add e1 e2) = liftM2 ((++) . (++ " + ")) (evalStr e1) (evalStr e2)
evalStr (Sub e1 e2) = liftM2 ((++) . (++ " - ")) (evalStr e1) (evalStr e2)
evalStr (Mul e1 e2) = liftM2 ((++) . (++ " * ")) (evalStr e1) (evalStr e2)
evalStr (Div e1 e2) = liftM2 ((++) . (++ " / ")) (evalStr e1) (evalStr e2)

evalStr (Rol d num) = do
  l <- replicateM num (roll d >>= return . show)
  return $ "[ " ++ intercalate " + " l ++ " ]"

roll :: Dice -> State StdGen Int
roll (Dice s)  = getRandomR (1, s)
roll (UDice s) = getRandomR (-s, s)

-- Parsers --

-- A parser to parse the integer literals
literal :: Stream s m Char => ParsecT s u m Expr
literal = (Lit . read) <$> spaced (many1 digit)

toDice :: Char -> String -> Dice
toDice 'u' = (UDice . read)
toDice 'd' = (Dice . read)
toDice _ = undefined

-- A parser to parse single die notation
die :: Stream s m Char => ParsecT s u m Dice
die = try (const (UDice 1) <$> string "dF") <|> (toDice <$> oneOf "du" <*> (many1 digit))

-- A parser to parse multiple dice
dice :: Stream s m Char => ParsecT s u m Expr
dice = (flip Rol . maybe 1 read) <$> optionMaybe (many1 digit) <*> die

-- A parse to parse a factor, where a factor is either a literal or
-- a factor preceded by an unary operator or an expression enclosed in brackets
factor :: Stream s m Char => ParsecT s u m Expr
factor  = spaced (char '(') *> spaced diceParser <* spaced (char ')')
          <|> try dice
          <|> literal

-- Operators table in descending order of precedence
table :: Stream s m Char => [[Operator s u m Expr]]
table = [[bop '*' Mul AssocLeft, bop '/' Div AssocLeft],    -- multiplication and division
         [bop '+' Add AssocLeft, bop '-' Sub AssocLeft]]    -- addition and subtraction
  where bop c f = Infix (spaced (char c) *> return f)       -- binary operators

-- | A parser to parse the dice roll expression
diceParser :: Stream s m Char => ParsecT s u m Expr
diceParser = buildExpressionParser table factor

parseDice :: String -> Either ParseError Expr
parseDice s = parse (diceParser <* eof) "DiceRollParser" s

evalDice :: Expr -> StdGen -> (Int, StdGen)
evalDice e g = runState (eval e) g

evalDiceStr :: Expr -> StdGen -> (String, StdGen)
evalDiceStr e g = runState (evalStr e) g
