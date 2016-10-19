module Interpreter
  (
    -- * Types
    Prog,

    -- * Functions
    evalRaw,
    evalAdt,
  ) where
import qualified Data.Map.Strict as Map
import Data.List.Utils
-------------------------------------------------------------------------------
--------------------------------- The Expr ADT  -------------------------------
-------------------------------------------------------------------------------
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Equal Expr Expr
          | Smaller Expr Expr
          | Symbol String
          | Value Int deriving (Show, Read)

-- [Optional] TODO Implement a parser for the Expr ADT.
--

-------------------------------------------------------------------------------
---------------------------------- The Prog ADT -------------------------------
-------------------------------------------------------------------------------
data Prog = Eq String Expr
          | Seq Prog Prog
          | If Expr Prog Prog
          | While Expr Prog
          | Return Expr deriving (Show, Read)

-- [Optional] TODO Implement a parser for the Prog ADT.
--

-- [Optional] TODO The *parse* function.  It receives a String - the program in
-- a "raw" format and it could return *Just* a program as an instance of the
-- *Prog* data type if no parsing errors are encountered, or Nothing if parsing
-- failed.
--
-- This is composed with evalAdt to yield the evalRaw function.
parse :: String -> Maybe Prog
--parse = undefined
parse = \_ -> Nothing


-------------------------------------------------------------------------------
-------------------------------- The Interpreter ------------------------------
-------------------------------------------------------------------------------

-- TODO The *evalAdt* function.  It receives the program as an instance of the
-- *Prog* data type and returns an instance of *Either String Int*; that is,
-- the result of interpreting the program.
--
-- The result of a correct program is always an Int.  This is a simplification
-- we make in order to ease the implementation.  However, we wrap this Int with
-- a *Either String* type constructor to handle errors.  The *Either* type
-- constructor is defined as:
--
-- data Either a b = Left a | Right b
--
-- and it is generally used for error handling.  That means that a value of
-- *Left a* - Left String in our case - wraps an error while a value of *Right
-- b* - Right Int in our case - wraps a correct result (notice that Right is a
-- synonym for "correct" in English).
-- 
-- For further information on Either, see the references in the statement of
-- the assignment.
--

-- defining the dictionary type, for keeping variables
type Dictionary a = [(String, Int)] 
-- getting and adding functions for dictionary
valueOf :: String -> (Dictionary a) -> (Either String Int) 
valueOf name [] = Left "ERROR"
valueOf name ((s,val):dict) = if (s == name)
              then Right (val)
              else valueOf name dict

addDictionary :: Dictionary a -> String -> Int -> Dictionary a -- trebuie schimbat
addDictionary [] str x = (str,x):[]
addDictionary ((str1, x1):context) str x = if(str == str1)
                                          then ((str1,x):context)
                                          else ((str1,x1):(addDictionary context str x))

-- auxiliary function to get an Int from an Either type
righttoInt :: Either String Int -> Int
righttoInt (Right a) = a

-- eval function for expressions using Pattern Matching
eval :: Expr -> Dictionary a -> Either String Int

-- value
eval (Value a) context = Right a
-- symbol
eval (Symbol a) context = valueOf a context
-- comparation
eval (Smaller a b) context = case (eval a context , eval b context) of
    (Right a, Right b) -> if (a < b) then (Right 1) else (Right 0)
    _ -> Left "ERROR"
-- equality
eval (Equal a b) context = case (eval a context , eval b context) of
    (Right a, Right b) -> if (b == a) then (Right 1) else (Right 0)
    _ -> Left "ERROR"
-- addition
eval (Add a b) context = case (eval a context, eval b context) of
    (Right a, Right b) -> Right(a+b)
    _ -> Left "ERROR"
-- substraction
eval (Sub a b) context = case (eval a context, eval b context) of
    (Right a, Right b) -> Right (a-b)
    _ -> Left "ERROR"
-- multiplication
eval (Mult a b) context = case (eval a context, eval b context) of
    (Right a, Right b) -> Right (a*b)
    _ -> Left "ERROR"

-- program evaluation
evalP :: Prog -> Dictionary a -> (Dictionary a , Either String Int)

--
evalP (Eq s ex) context = if ((eval ex context) == Left "ERROR") then (context, Left "Uninitialized variable")  
                                                            else (addDictionary context s (righttoInt (eval ex context)),Right 0)
evalP (Seq prog1 prog2) context =  case (evalP prog1 context) of
                                (context',Left _) -> (context',Left "Uninitialized variable")  
                                (context',Right val) -> evalP prog2 context'                 
                                                                         

evalP (If expr progr1 progr2) context =
    if ((Right 1) == (eval expr context))
      then evalP progr1 context
      else if ((Left "ERROR") == (eval expr context))
            then (context, Left "Uninitialized variable")
      else evalP progr2 context

-- while
-- posibil incorect
evalP (While expr progr) context = 
    if ((Right 1) == eval expr context)
      then case (evalP progr context) of
        (context' , Right val) -> (evalP (While expr progr) context')
        (context' , _) -> (context', Left "Uninitialized variable")
      else if ((Right 0) == (eval expr context))
        then (context, Right 0)
        else (context , Left "Uninitialized variable")  

-- return

evalP (Return expr) context =
    case (eval expr context) of
      Right val -> (context, Right val)
      _ -> (context, Left "Uninitialized variable")
evaluation :: (Dictionary a, Either String Int) -> (Either String Int)
evaluation (dict,Right 0) = Left "Missing return"
evaluation (dict,a) = a
evalAdt :: Prog -> Either String Int
evalAdt p = evaluation (evalP p [])
--evalAdt = \_ -> Right 0

-- The *evalRaw* function is already implemented, but it relies on the *parse*
-- function which you have to implement.
--
-- Of couse, you can change this definition.  Only its name and type are
-- important.
evalRaw :: String -> Either String Int
evalRaw rawProg = case parse rawProg of
                    Just prog -> evalAdt prog
                    Nothing   -> Left "Syntax error"
