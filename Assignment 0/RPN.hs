module RPN where

import Data.Char

data Token = TNum Int | TPlus | TMulti
  deriving (Show)

-- check operators from string
judgePlusMul :: String -> Token
judgePlusMul c
  | c == "+" = TPlus
  | c == "*" = TMulti
  | otherwise = error "unrecognised operation"

lexer :: String -> [Token]
lexer [] = []
lexer input
  | fst t /= "" = TNum (read (fst t)) : lexer (snd t)  -- first part is non-null means it is number!
  | fst t == "" && snd t /= "" && (take 1 (snd t)) == " " = lexer (drop 1 (snd t))  -- remove SPACE from string
  | fst t == "" && snd t /= "" && (take 1 (snd t)) /= " " = judgePlusMul (take 1 (snd t)) : lexer (drop 1 (snd t))  -- extract operator
  | fst t == "" && snd t == "" = []  -- end of string
  where
    t = span isDigit input  -- split string into the first longest number and others, SPACE will be recognise as ""

-- check the type of token
isTNum :: Token -> Bool
isTPlus :: Token -> Bool
isTMulti :: Token -> Bool
isTNum (TNum _) = True
isTNum _ = False
isTPlus (TPlus) = True
isTPlus _ = False
isTMulti (TMulti) = True
isTMulti _ = False

-- get number from token
getValue :: Token -> Int
getValue (TNum i) = i

data Expr = ENum Int | EAdd Expr Expr | EMul Expr Expr
  deriving (Show)

parser :: [Token] -> Expr
parser t_input = parser' [] t_input

parser' :: [Expr] -> [Token] -> Expr
parser' e_input t_input
  | null t_input = last e_input  -- if all elements are parsed into expr, output the last expr in list
  | isTPlus (t_input !! 0) && length e_input == 1 = parser' ((init e_input) ++ [EAdd (last e_input) (ENum 0)]) (drop 1 t_input)  -- if less than two numbers are passed to a operator, use zero to make it paired
  | isTPlus (t_input !! 0) && length e_input == 0 = parser' (e_input ++ [EAdd (ENum 0) (ENum 0)]) (drop 1 t_input)
  | isTPlus (t_input !! 0) = parser' (init(init e_input) ++ [EAdd (last e_input) (last (init e_input))]) (drop 1 t_input)
  | isTMulti (t_input !! 0) && length e_input == 1 = parser' ((init e_input) ++ [EMul (last e_input) (ENum 0)]) (drop 1 t_input)
  | isTMulti (t_input !! 0) && length e_input == 0 = parser' (e_input ++ [EMul (ENum 0) (ENum 0)]) (drop 1 t_input)
  | isTMulti (t_input !! 0) = parser' (init(init e_input) ++ [EMul (last e_input) (last (init e_input))]) (drop 1 t_input)
  | isTNum (t_input !! 0) = parser' (e_input ++ [ENum (getValue (t_input !! 0))]) (drop 1 t_input)  -- directly add number to the list

eval :: Expr -> Int
eval e_input
  | isENum e_input = getExprNumValue e_input
  | isEAdd e_input = eval (getFstExpr e_input) + eval (getSndExpr e_input)
  | isEMul e_input = eval (getFstExpr e_input) * eval (getSndExpr e_input)

-- check the type of expr
isENum :: Expr -> Bool
isEAdd :: Expr -> Bool
isEMul :: Expr -> Bool
isENum (ENum _) = True
isENum _ = False
isEAdd (EAdd _ _) = True
isEAdd _ = False
isEMul (EMul _ _) = True
isEMul _ = False

-- get number or first/second expr in a expr
getExprNumValue :: Expr -> Int
getFstExpr :: Expr -> Expr
getSndExpr :: Expr -> Expr
getExprNumValue (ENum i) = i
getFstExpr (EAdd i _) = i
getFstExpr (EMul i _) = i
getSndExpr (EAdd _ i) = i
getSndExpr (EMul _ i) = i