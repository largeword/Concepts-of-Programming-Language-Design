module Lambda where

import Data.Char
import Data.Map.Strict (Map)
import Data.Text(pack, unpack, replace)
import qualified Data.Map.Strict as Map
import Text.Show.Functions ()

data Token = TNum Int | TVar String | TPlus | TMulti | TLambda | TTick
  deriving (Show)

replaceOperators :: String -> String
replaceOperators ('a':'d':'d':xs) = '+' : replaceOperators xs
replaceOperators ('m':'u':'l':xs) = '*' : replaceOperators xs
replaceOperators (x:xs) = x : replaceOperators xs
replaceOperators "" = ""

judgeOperators :: String -> Token
judgeOperators c
  | c == "+" = TPlus
  | c == "*" = TMulti
  | c == "#" = TLambda
  | c == "`" = TTick
  | otherwise = error "unrecognised operation"

lexer :: String -> [Token]
lexer [] = []
lexer input_raw
  | fst t /= "" = TNum (read (fst t)) : lexer (snd t)  -- first part is non-null means it is number!
  | fst t == "" && snd t /= "" && (take 1 (snd t)) == " " = lexer (drop 1 (snd t))  -- remove SPACE from string
  | fst t == "" && snd t /= "" && (take 1 (snd t)) /= " " &&  fst (span isAlpha (snd t)) /= "" = TVar (fst (span isAlpha (snd t))) : lexer (snd (span isAlpha (snd t)))
  | fst t == "" && snd t /= "" && (take 1 (snd t)) /= " " = judgeOperators (take 1 (snd t)) : lexer (drop 1 (snd t))  -- extract operator
  | fst t == "" && snd t == "" = []  -- end of string
  where
    t = span isDigit input  -- split string into the first longest number and others, SPACE will be recognise as ""
    input = replaceOperators input_raw

data Expr = ENum Int | EVar String | EAdd | EMul | EApp Expr Expr | ELambda Expr Expr
  deriving (Show)


reversal :: [a] -> [a]
reversal [] = []
reversal (x : xs) = (reversal xs) ++ [x]

parser :: [Token] -> Expr
parser t_input_raw = parser' (reversal t_input_raw) [] 

-- need to reverse [token]
parser' :: [Token] -> [Expr] -> Expr
-- parser' token_input expr_stack last_token
parser' (TVar v : xs) expr_s = parser' xs (EVar v : expr_s)
parser' (TNum n : xs) expr_s = parser' xs (ENum n : expr_s)
parser' (TPlus : xs) expr_s = parser' xs (EVar "add": expr_s)
parser' (TMulti : xs) expr_s = parser' xs (EVar "mul": expr_s)
parser' (TTick : xs) expr_s = parser' xs ((EApp (expr_s !! 0) ((drop 1 expr_s) !! 0)) : (drop 2 expr_s))
parser' (TLambda : xs) expr_s = parser' xs ((ELambda (expr_s !! 0) ((drop 1 expr_s) !! 0)) : (drop 2 expr_s))
parser' [] expr_s = expr_s !! 0


data Value = VInt Int | VFun (Value -> Value)
  deriving (Show)

eval :: Expr -> Value
eval e_input = eval' Map.empty e_input

searchValue :: Map String Value -> String -> Value
searchValue dic var =
    case Map.lookup var dic of
        Just (val) -> val
        Nothing -> error "Variable not defined"

eval' :: Map String Value -> Expr -> Value
eval' _ (ENum n) = VInt n
eval' dic (EVar "add") = VFun (\(VInt n1) -> VFun (\(VInt n2) -> VInt (n1+n2)))
eval' dic (EVar "mul") = VFun (\(VInt n1) -> VFun (\(VInt n2) -> VInt (n1*n2)))
eval' dic (ELambda (EVar v) e) = VFun (\value' -> (eval' (Map.insert v value' dic) e))
eval' dic (EVar v) = searchValue dic v
eval' dic (EApp e1 e2) = (getExpr(eval' dic e1) (eval' dic e2))

getExpr :: Value -> Value -> Value
getExpr (VFun e) = e

interpret :: String -> Value
interpret s_input = eval (parser (lexer s_input))