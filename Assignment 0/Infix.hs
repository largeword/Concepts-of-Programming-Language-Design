module Infix where

import Data.Char

data TokenInfix = TNum Int | TPlus | TMulti | TLeftParenthese | TRightParenthese
  deriving (Show)

-- check operators from string
judgePlusMul :: String -> TokenInfix
judgePlusMul c
  | c == "+" = TPlus
  | c == "*" = TMulti
  | c == "(" = TLeftParenthese
  | c == ")" = TRightParenthese
  | otherwise = error "unrecognised operation"

lexerInfix :: String -> [TokenInfix]
lexerInfix input
  | fst t /= "" = TNum (read (fst t)) : lexerInfix (snd t)  -- first part is non-null means it is number!
  | fst t == "" && snd t /= "" && (take 1 (snd t)) == " " = lexerInfix (drop 1 (snd t))  -- remove SPACE from string
  | fst t == "" && snd t /= "" && (take 1 (snd t)) /= " " = judgePlusMul (take 1 (snd t)) : lexerInfix (drop 1 (snd t))  -- extract operator
  | fst t == "" && snd t == "" = []  -- end of string
  where
    t = span isDigit input  -- split string into the first longest number and others, SPACE will be recognise as ""

-- using shunting yard algorithm to convert infix to postfix
shuntingYardAlgo :: [TokenInfix] -> [TokenInfix] -> [TokenInfix] -> [TokenInfix]
shuntingYardAlgo infix_input output_queue operator_stack
  | null infix_input && length operator_stack >= 2 = shuntingYardAlgo infix_input (output_queue ++ [last operator_stack]) (init operator_stack)
  | null infix_input && length operator_stack == 1 = output_queue ++ [last operator_stack]
  | null infix_input && null operator_stack = output_queue
  
  | isTNum (infix_input !! 0) = shuntingYardAlgo (drop 1 infix_input) (output_queue ++ [infix_input !! 0]) operator_stack
  
  | (isTPlus (infix_input !! 0) || isTMulti (infix_input !! 0)) && null operator_stack = shuntingYardAlgo (drop 1 infix_input) output_queue (operator_stack ++ [infix_input !! 0])
  | isTPlus (infix_input !! 0) && (isTPlus (last operator_stack) || isTMulti (last operator_stack)) = shuntingYardAlgo infix_input (output_queue ++ [last operator_stack]) (init operator_stack)
  | isTMulti (infix_input !! 0) && isTMulti (last operator_stack) = shuntingYardAlgo infix_input (output_queue ++ [last operator_stack]) (init operator_stack)
  | isTPlus (infix_input !! 0) || isTMulti (infix_input !! 0)  = shuntingYardAlgo (drop 1 infix_input) output_queue (operator_stack ++ [infix_input !! 0])
  
  | isTLP (infix_input !! 0) = shuntingYardAlgo (drop 1 infix_input) output_queue (operator_stack ++ [infix_input !! 0])
  
  | isTRP (infix_input !! 0) && not (isTLP (last operator_stack)) = shuntingYardAlgo infix_input (output_queue ++ [last operator_stack]) (init operator_stack)
  | isTRP (infix_input !! 0) && isTLP (last operator_stack) = shuntingYardAlgo (drop 1 infix_input) output_queue (init operator_stack)

data Expr = ENum Int | EAdd Expr Expr | EMul Expr Expr
  deriving (Show)

parserInfix :: [TokenInfix] -> Expr
parserInfix t_input = parser' [] (shuntingYardAlgo t_input [] [])

parser' :: [Expr] -> [TokenInfix] -> Expr
parser' e_input t_input
  | null t_input = last e_input  -- if all elements are parsed into expr, output the last expr in list
  | isTPlus (t_input !! 0) && length e_input == 1 = parser' ((init e_input) ++ [EAdd (last e_input) (ENum 0)]) (drop 1 t_input)  -- if less than two numbers are passed to a operator, use zero to make it paired
  | isTPlus (t_input !! 0) && length e_input == 0 = parser' (e_input ++ [EAdd (ENum 0) (ENum 0)]) (drop 1 t_input)
  | isTPlus (t_input !! 0) = parser' (init(init e_input) ++ [EAdd (last e_input) (last (init e_input))]) (drop 1 t_input)
  | isTMulti (t_input !! 0) && length e_input == 1 = parser' ((init e_input) ++ [EMul (last e_input) (ENum 0)]) (drop 1 t_input)
  | isTMulti (t_input !! 0) && length e_input == 0 = parser' (e_input ++ [EMul (ENum 0) (ENum 0)]) (drop 1 t_input)
  | isTMulti (t_input !! 0) = parser' (init(init e_input) ++ [EMul (last e_input) (last (init e_input))]) (drop 1 t_input)
  | isTNum (t_input !! 0) = parser' (e_input ++ [ENum (getValue (t_input !! 0))]) (drop 1 t_input)  -- directly add number to the list
  
-- get number from token
getValue :: TokenInfix -> Int
getValue (TNum i) = i

-- check the type of token
isTNum :: TokenInfix -> Bool
isTPlus :: TokenInfix -> Bool
isTMulti :: TokenInfix -> Bool
isTLP :: TokenInfix -> Bool
isTRP :: TokenInfix -> Bool
isTNum (TNum _) = True
isTNum _ = False
isTPlus (TPlus) = True
isTPlus _ = False
isTMulti (TMulti) = True
isTMulti _ = False
isTLP (TLeftParenthese) =True
isTLP _ =False
isTRP (TRightParenthese) =True
isTRP _ =False
 
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

-- combine number with last op
combineOpAdd :: Expr -> TokenInfix -> Expr
combineOpMul :: Expr -> TokenInfix -> Expr
combineOpAdd (EAdd i j) tnum_input = (EAdd (EAdd i j) (ENum (getValue tnum_input)))
combineOpAdd (EMul i j) tnum_input = (EAdd (EMul i j) (ENum (getValue tnum_input)))
combineOpAdd (ENum i) tnum_input = (EAdd (ENum i) (ENum (getValue tnum_input)))
combineOpMul (EAdd i j) tnum_input = (EAdd i (EMul j (ENum (getValue tnum_input))))
combineOpMul (EMul i j) tnum_input = (EMul (EMul i j) (ENum (getValue tnum_input)))
combineOpMul (ENum i) tnum_input = (EMul (ENum i) (ENum (getValue tnum_input)))

eval :: Expr -> Int
eval e_input
  | isENum e_input = getExprNumValue e_input
  | isEAdd e_input = eval (getFstExpr e_input) + eval (getSndExpr e_input)
  | isEMul e_input = eval (getFstExpr e_input) * eval (getSndExpr e_input)