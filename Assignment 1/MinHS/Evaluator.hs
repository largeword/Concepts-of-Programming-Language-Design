module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Debug.Trace
import Text.Show.Functions ()

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           -- Add other variants as needed
           | Closure VEnv Exp  -- pair a function with its environment, forming a closure
           | LetPartPrim Id Type [Id] Exp
           | LetDfun VEnv Bind
           | BVar String  -- store binding relationship between each variable
           | InfList Value  -- task6 lazy list
           | LetPartList Exp  -- store partial list creation expr
           deriving (Show, Read, Eq)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)


-- custom types
type Stack = [ExpFrame]
data ExpFrame = AppArg2 Exp  -- store apply function with its arg1 when arg2 is awaiting to be returned
              | AppArg1 Value
              | SEnv VEnv  -- to push current environment into stack
              | ConsExpr Exp  -- store cons when it is of expr
              | ConsValue Value  -- store cons when it is of value
              | IfCon Exp Exp  -- store if function when condition is awaiting for evaluation
              | PrimArg1 Op Exp  -- store things when arg1 of prim is awaiting evaluation
              | PrimArg2 Op Value
              | PrimNeg
              | ListOp Op
              | BindOp Id Type [Id] Exp  -- store binding and expr when binding body is awaiting for evaluation
              | LetMulti [Bind] Exp
              | InfListArg1 String Exp  -- store list name and expr while body is awaiting to be evaluated
              | LetPartListOp Exp
              deriving (Read,Show)


-- add the definition
data MachineState = MsEval Stack VEnv Exp 
                  | MsRtn Stack VEnv Value
                  deriving (Show, Read)



-- do not change this definition
evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE e
evaluate _ = error "Input program did not have exactly one binding"

-- do not change this definition
evalE :: Exp -> Value
evalE expr = loop (msInitialState expr)
  where 
    loop ms = -- (trace ("debug message " ++ (show ms))) $ -- uncomment this line and pretty print the machine state/parts of it to
                                            -- observe the machine states
             if (msInFinalState newMsState)
                then msGetValue newMsState
                else loop newMsState
              where
                 newMsState = msStep ms



-- implement me!
msInitialState :: Exp -> MachineState
msInitialState expr = MsEval [] E.empty expr



-- implement me!
-- checks whether machine is in final state
msInFinalState :: MachineState -> Bool
msInFinalState (MsEval _ _ _) = False
msInFinalState (MsRtn inputStack inputEnv inputExpr) = case inputStack of
  [] -> case inputExpr of
    I _ -> True
    B _ -> True
    Nil -> True
    Cons _ _ -> True
    _ -> error "Custom: msInFinalState error: Stack is empty while the return is not a value!"
  _ -> False



-- implement me!
-- returns the final value if machine is in final state. If the machine is not in final state, throw an error
msGetValue :: MachineState -> Value
msGetValue (MsEval _ _ _) = error "Custom: msGetValue error: Cannot get value while the E-machine is not at return mode!"
msGetValue (MsRtn _ _ inputValue) = case inputValue of
  I n -> I n
  B tf -> B tf
  Nil -> Nil
  Cons i v -> Cons i v
  _ -> error "Custom: msGetValue error: Input value is illegal!"
  
  

-- implement me!
msStep :: MachineState -> MachineState
msStep inputMs = case inputMs of
  
  
  -- Define basic evaluation and state transfer
  MsEval inputStack inputEnv (Num n) -> MsRtn inputStack inputEnv (I n)
  MsEval inputStack inputEnv (Con condition) -> case condition of
    "True" -> MsRtn inputStack inputEnv (B True)
    "False" -> MsRtn inputStack inputEnv (B False)
    "Nil" -> MsRtn inputStack inputEnv Nil
  
  MsEval inputStack inputEnv (App (App (Con "Cons") x) xs) -> MsEval ((ConsExpr xs):inputStack) inputEnv x  -- temporarily store unevaluated tail of list expr, head is awaiting for evaluation  
  MsRtn ((ConsExpr xs):stack) inputEnv (I n) -> MsEval ((ConsValue (I n)):stack) inputEnv xs  -- pop unevaluated tail of list expr, and push evaluated head of list
  MsRtn ((ConsValue (I n)):stack) inputEnv Nil -> MsRtn stack inputEnv (Cons n Nil)  -- last item of list is at the top of stack
  MsRtn ((ConsValue (I n1)):stack) inputEnv (Cons n2 v) -> MsRtn stack inputEnv (Cons n1 (Cons n2 v))  -- head of the list is at the bottom of stack
  
  MsRtn ((ConsValue Nil):stack) inputEnv (I n) -> MsRtn stack inputEnv (Cons n Nil)
  MsEval inputStack inputEnv (App (Con "Cons") (Num n)) -> MsRtn inputStack inputEnv (Cons n Nil)
  MsEval inputStack inputEnv (App (Con "Cons") (App e1 e2)) -> MsEval ((ConsValue Nil):inputStack) inputEnv (App e1 e2)
  
 
  
  -- Variables (look for var binding)
  MsEval inputStack inputEnv (Var x) -> case (E.lookup inputEnv x) of
    Just (val) -> MsRtn inputStack inputEnv val
    Nothing -> error "Custom: searchValue error: Variable not defined!"
  MsRtn inputStack inputEnv (BVar x) -> case (E.lookup inputEnv x) of
    Just (val) -> MsRtn inputStack inputEnv val
    Nothing -> error "Custom: searchValue error: Variable not defined!"
  
  

  -- Multiple bindings in let (Put this block here to let LetMulti to be handled first, otherwise (App e1 e2) will be processed by others)
  MsEval inputStack inputEnv (Let ((Bind var1 var1Type var1List var1Expr):(Bind var2 var2Type var2List var2Expr):restBind) expr) -> MsEval ((LetMulti ((Bind var2 var2Type var2List var2Expr):restBind) expr):inputStack) inputEnv (Let [Bind var1 var1Type var1List var1Expr] expr)
  MsEval ((LetMulti restBind expr1):inputStack) inputEnv (App e1 e2) -> case (expr1 == (App e1 e2)) of
    True -> case restBind of
      [Bind var varType varList varExpr] -> MsEval inputStack inputEnv (Let [Bind var varType varList varExpr] expr1)
      (Bind var varType varList varExpr):restBind2 -> MsEval ((LetMulti restBind2 expr1):inputStack) inputEnv (Let [Bind var varType varList varExpr] expr1)
  


  -- Primitive operations
  MsEval inputStack inputEnv (App (App (Prim op) expr1) expr2) -> MsEval ((PrimArg1 op expr2):inputStack) inputEnv expr1
  MsRtn ((PrimArg1 op expr2):stack) inputEnv expr1RtnValue -> MsEval ((PrimArg2 op expr1RtnValue):stack) inputEnv expr2
  MsRtn ((PrimArg2 op (I n1)):stack) inputEnv (I n2) -> case op of
    Add -> MsRtn stack inputEnv (I (n1 + n2))
    Sub -> MsRtn stack inputEnv (I (n1 - n2))
    Mul -> MsRtn stack inputEnv (I (n1 * n2))
    Quot -> if n2 == 0
            then error "Custom: msStep error: Division by zero!"
            else MsRtn stack inputEnv (I (quot n1 n2))
    Rem -> if n2 == 0
           then error "Custom: msStep error: Mod by zero!"
           else MsRtn stack inputEnv (I (mod n1 n2))
    
    Gt -> MsRtn stack inputEnv (B (n1 > n2))
    Ge -> MsRtn stack inputEnv (B (n1 >= n2))
    Lt -> MsRtn stack inputEnv (B (n1 < n2))
    Le -> MsRtn stack inputEnv (B (n1 <= n2))
    Eq -> MsRtn stack inputEnv (B (n1 == n2))
    Ne -> MsRtn stack inputEnv (B (n1 /= n2))
  
  MsEval inputStack inputEnv (App (Prim Neg) expr) -> MsEval (PrimNeg:inputStack) inputEnv expr
  MsRtn (PrimNeg:inputStack) inputEnv (I n) -> MsRtn inputStack inputEnv (I ((-1) * n))
  
  MsEval inputStack inputEnv (App (Prim Head) inputExpr) -> case inputExpr of
    Con "Nil" -> error "Custom: msStep error: Get head of a empty list!"
    _ -> MsEval ((ListOp Head):inputStack) inputEnv inputExpr
  MsRtn ((ListOp Head):stack) inputEnv (Cons n _) -> MsRtn stack inputEnv (I n)
  MsRtn ((ListOp Head):stack) inputEnv (InfList Nil) -> error "Custom: msStep error: Get head of a empty inf list!"
  MsRtn ((ListOp Head):stack) inputEnv (InfList (Cons n _)) -> MsRtn stack inputEnv (I n)
  
  MsEval inputStack inputEnv (App (Prim Tail) inputExpr) -> case inputExpr of
    Con "Nil" -> error "Custom: msStep error: Get tail of a empty list!"
    _ -> MsEval ((ListOp Tail):inputStack) inputEnv inputExpr  
  MsRtn ((ListOp Tail):stack) inputEnv (Cons _ tailList) -> MsRtn stack inputEnv tailList
  MsRtn ((ListOp Tail):stack) inputEnv (InfList Nil) -> error "Custom: msStep error: Get tail of a empty inf list!"
  MsRtn ((ListOp Tail):stack) inputEnv (InfList infList) -> MsRtn stack inputEnv (InfList infList)
  
  
  MsEval inputStack inputEnv (App (Prim Null) inputExpr) -> MsEval ((ListOp Null):inputStack) inputEnv inputExpr  
  MsRtn ((ListOp Null):stack) inputEnv (InfList infList) -> if infList == Nil
                                                            then MsRtn stack inputEnv (B True)
                                                            else MsRtn stack inputEnv (B False)
  MsRtn ((ListOp Null):stack) inputEnv rtnList -> if rtnList == Nil
                                                  then MsRtn stack inputEnv (B True)
                                                  else MsRtn stack inputEnv (B False)

  -- if-then-else
  MsEval inputStack inputEnv (If condition expr1 expr2) -> MsEval ((IfCon expr1 expr2):inputStack) inputEnv condition
  MsRtn ((IfCon expr1 expr2):stack) inputEnv (B condition) -> case condition of
    True -> MsEval stack inputEnv expr1
    False -> MsEval stack inputEnv expr2

  
  
  -- Lazy list & partial primops
  MsEval inputStack inputEnv (Let [Bind rfId1 rfType1 rfList1 (Recfun (Bind rfId2 rfType2 [] rfExpr2))] e) -> case rfExpr2 of
    Prim _ -> MsEval inputStack (E.add inputEnv (rfId2, (LetPartPrim rfId2 rfType2 [] rfExpr2))) e  -- Neg, Head, Tail, Null
    
    App (Prim _) _ -> MsEval inputStack (E.add inputEnv (rfId2, (LetPartPrim rfId2 rfType2 [] rfExpr2))) e  -- Add, Sub, Mul, ... any op which needs two args
    App (Con "Cons") _ -> MsEval inputStack (E.add inputEnv (rfId2, (LetPartPrim rfId2 rfType2 [] rfExpr2))) e  -- function: create list
    (Con "Cons") -> MsEval inputStack (E.add inputEnv (rfId2, (LetPartList rfExpr2))) e
    
    (Con "Nil") -> MsEval inputStack (E.add inputEnv (rfId1, (InfList Nil))) e
    
    (App (App (Con "Cons") (Num n)) (Var loopBack)) -> case (loopBack == rfId2) of
      True -> MsEval inputStack (E.add inputEnv (rfId1, (InfList (Cons n Nil)))) e
    (App content (Var loopBack)) -> case (loopBack == rfId2) of
      True -> MsEval ((InfListArg1 rfId2 e):inputStack) inputEnv content
    
  MsRtn ((InfListArg1 rfId2 e):inputStack) inputEnv (Cons n v) -> MsEval inputStack (E.add inputEnv (rfId2, (InfList (Cons n v)))) e
  
  MsRtn ((AppArg1 (I rtn1)):(AppArg1 Nil):inputStack) inputEnv (LetPartList rfExpr2) -> MsEval inputStack inputEnv (App (App rfExpr2 (Num rtn1)) (Con "Nil"))
  MsRtn ((AppArg1 (I rtn1)):(AppArg1 (Cons n v)):inputStack) inputEnv (LetPartList rfExpr2) -> MsEval ((AppArg1 (Cons n v)):inputStack) inputEnv (App rfExpr2 (Num rtn1))
  
  MsRtn ((AppArg1 (Cons n1 v1)):inputStack) inputEnv (Cons n2 Nil) -> MsRtn inputStack inputEnv (Cons n2 (Cons n1 v1))
  
  
  -- let bindings declare functions
  MsEval stack inputEnv (Let [Bind rfId1 rfType1 rfList1 rfExpr] (Let [Bind rfId2 rfType2 [var2] rfExpr2] e)) -> if (rfId1 == rfId2)
    then case rfExpr2 of
      (App (App (Prim oOp) var1) (Var rfId1)) -> MsEval stack (E.add inputEnv (rfId2, LetDfun inputEnv (Bind rfId2 rfType2 [var2] (App (App (Prim oOp) var1) rfExpr)))) e      
    else MsEval stack (E.addAll inputEnv [(rfId1, LetDfun inputEnv (Bind rfId1 rfType1 rfList1 rfExpr)),(rfId2, LetDfun inputEnv (Bind rfId2 rfType2 [var2] rfExpr2))]) e



  -- let & Partial Primops

  MsEval inputStack inputEnv (Let [Bind var varType [] varExpr] expr) -> case varExpr of
    Num n -> MsEval inputStack (E.add inputEnv (var, (I n))) expr
    Con "True" -> MsEval inputStack (E.add inputEnv (var, (B True))) expr
    Con "False" -> MsEval inputStack (E.add inputEnv (var, (B False))) expr
    Var var2 -> MsEval inputStack (E.add inputEnv (var, (BVar var2))) expr
    
    Prim _ -> MsEval inputStack (E.add inputEnv (var, (LetPartPrim var varType [] varExpr))) expr  -- Neg, Head, Tail, Null
    
    App (Prim _) _ -> MsEval inputStack (E.add inputEnv (var, (LetPartPrim var varType [] varExpr))) expr  -- Add, Sub, Mul, ... any op which needs two args
    App (Con "Cons") _ -> MsEval inputStack (E.add inputEnv (var, (LetPartPrim var varType [] varExpr))) expr  -- function: create list
    
    _ -> case expr of
      _ -> MsEval ((BindOp var varType [] expr):inputStack) inputEnv varExpr
  
  MsRtn ((BindOp var varType [] expr):inputStack) inputEnv varRtnValue -> MsEval inputStack (E.add inputEnv (var, varRtnValue)) expr
  
  MsRtn ((AppArg1 (I n1)):(AppArg1 (I n2)):inputStack) inputEnv (LetPartPrim var varType [] (Prim pOp)) -> case pOp of
    Neg -> MsEval ((AppArg1 (I n2)):inputStack) inputEnv (App (Prim pOp) (Num n1))
    Add -> MsEval inputStack inputEnv (App (App (Prim pOp) (Num n1)) (Num n2))
    Sub -> MsEval inputStack inputEnv (App (App (Prim pOp) (Num n1)) (Num n2))
    Mul -> MsEval inputStack inputEnv (App (App (Prim pOp) (Num n1)) (Num n2))
    Quot -> MsEval inputStack inputEnv (App (App (Prim pOp) (Num n1)) (Num n2))
    Rem -> MsEval inputStack inputEnv (App (App (Prim pOp) (Num n1)) (Num n2))
    Gt -> MsEval inputStack inputEnv (App (App (Prim pOp) (Num n1)) (Num n2))
    Ge -> MsEval inputStack inputEnv (App (App (Prim pOp) (Num n1)) (Num n2))
    Lt -> MsEval inputStack inputEnv (App (App (Prim pOp) (Num n1)) (Num n2))
    Le -> MsEval inputStack inputEnv (App (App (Prim pOp) (Num n1)) (Num n2))
    Eq -> MsEval inputStack inputEnv (App (App (Prim pOp) (Num n1)) (Num n2))
    Ne -> MsEval inputStack inputEnv (App (App (Prim pOp) (Num n1)) (Num n2))
  MsRtn ((AppArg1 (I n)):inputStack) inputEnv (LetPartPrim var varType [] (Prim Neg)) -> MsEval inputStack inputEnv (App (Prim Neg) (Num n))
  
  MsRtn ((AppArg1 rtnList):inputStack) inputEnv (LetPartPrim var varType [] (Prim pOp)) -> case pOp of
    Head -> MsRtn ((ListOp Head):inputStack) inputEnv rtnList
    Tail -> MsRtn ((ListOp Tail):inputStack) inputEnv rtnList
    Null -> MsRtn ((ListOp Null):inputStack) inputEnv rtnList
  MsRtn ((AppArg1 rtnList):inputStack) inputEnv (LetPartPrim var varType [] (App (Con "Cons") n)) -> if rtnList == Nil
      then MsEval inputStack inputEnv (App (App (Con "Cons") n) (Con "Nil"))
      else MsEval inputStack inputEnv (App (App (Con "Cons") n) (Con "Nil"))
  
  MsRtn ((AppArg1 (I n)):inputStack) inputEnv (LetPartPrim var varType [] varExpr) -> MsEval inputStack inputEnv (App varExpr (Num n))
  


  -- n-ary functions & let partial application
  MsEval inputStack inputEnv (Recfun (Bind rfId1 rfType1 rfList1 rfExpr1)) -> if (length rfList1) > 1
      then MsRtn inputStack inputEnv (Closure (inputEnv) (Recfun (Bind rfId1 rfType1 [head rfList1] (Recfun (Bind (rfId1 ++ (head rfList1)) rfType1 (tail rfList1) rfExpr1))))) 
      else MsRtn inputStack inputEnv (Closure (inputEnv) (Recfun (Bind (rfId1) rfType1 rfList1 rfExpr1))) 
  
  

  -- let bindings declare functions
  MsEval inputStack inputEnv (Let [Bind rfId1 rfType1 rfList1 rfExpr] e) -> case rfExpr of 
    (Recfun (Bind rfId2 rfType2 [var2] rfExpr2)) -> if rfId1 == rfId2
      then MsEval inputStack (E.add inputEnv (rfId2, LetDfun inputEnv (Bind rfId2 rfType2 [var2] rfExpr2))) e
      else MsEval inputStack (E.addAll inputEnv [(rfId1, BVar rfId2),(rfId2, LetDfun inputEnv (Bind rfId2 rfType2 [var2] rfExpr2))]) e
    _ -> MsEval inputStack (E.add inputEnv (rfId1, LetDfun inputEnv (Bind rfId1 rfType1 rfList1 rfExpr))) e
  
  MsRtn (AppArg1 (I n):xs) inputEnv (LetDfun rfEnv (Bind rfId rfType [var] rfExpr)) -> MsEval xs (E.add inputEnv (var, (I n))) rfExpr
  MsRtn (AppArg1 (I n1):AppArg1 (I n2):xs) inputEnv (LetDfun rfEnv (Bind rfId rfType (var1:var2:vars) rfExpr)) -> MsRtn (AppArg1 (I n2):xs) (E.add inputEnv (var1, (I n1))) (LetDfun rfEnv (Bind rfId rfType (var2:vars) rfExpr))


  
  -- Function application
  MsEval inputStack inputEnv (App inputExpr inputValue) -> MsEval ((AppArg2 inputExpr):inputStack) inputEnv inputValue
  MsRtn ((AppArg2 inputExpr):inputStack) inputEnv inputValue -> MsEval ((AppArg1 inputValue):inputStack) inputEnv inputExpr
  MsRtn ((AppArg1 inputValue):inputStack) inputEnv (Closure rfEnv (Recfun (Bind rfId rfType xList rfExpr))) -> MsEval ((SEnv inputEnv):inputStack) (E.addAll rfEnv [((head xList), inputValue), (rfId, Closure rfEnv (Recfun (Bind rfId rfType xList rfExpr)))]) rfExpr  -- push old env to stack, and select the top variables of binding list
  MsRtn ((SEnv env):stack) inputEnv inputExpr -> MsRtn stack env inputExpr



-- convert list in expr to list in value format
convertExprListToValueList:: Exp -> Value
convertExprListToValueList (App (App (Con "Cons") (Num n)) (Con "Nil")) = Cons n Nil
convertExprListToValueList (App (App (Con "Cons") (Num n)) tailList) = (Cons n (convertExprListToValueList tailList))  

-- convert recfun with multiple bindings into recfuns with one binding of task 3: n-ary functions
buildRecfuns :: Exp -> Exp
buildRecfuns (Recfun (Bind rfId rfType [] rfExpr)) = (Recfun (Bind rfId rfType [] rfExpr))
buildRecfuns (Recfun (Bind rfId rfType [oneElt] rfExpr)) = (Recfun (Bind rfId rfType [oneElt] rfExpr))
buildRecfuns (Recfun (Bind rfId rfType (x:xs) rfExpr)) = (Recfun (Bind (rfId++x) rfType [x] (buildRecfuns (Recfun (Bind rfId rfType xs rfExpr)))))
