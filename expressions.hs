-- expressions
data Expr = T | F | O | Succ Expr | Pred Expr | IsZero Expr | If Expr Expr Expr
            deriving Show

-- types
data Type = Boolean | Nat | NoType
            deriving (Show, Eq)

isNumeric :: Expr -> Bool
isNumeric O = True
isNumeric (Succ nv) = isNumeric nv
isNumeric _ = False

isValue :: Expr -> Bool
isValue T = True
isValue F = True
isValue nv
  | isNumeric nv = True
  | otherwise = False

-- small-step evaluator
eval1 :: Expr -> Expr

-- computation rules
eval1 (Pred O) = O
eval1 (Pred (Succ nv1))
  | isNumeric nv1 = nv1
  | otherwise = error "Invalid Expression"

eval1 (IsZero O) = T
eval1 (IsZero (Succ nv1))
  | isNumeric nv1 = F
  | otherwise = error "Invalid Expression"

eval1 (If T e2 _) = e2
eval1 (If F _ e3) = e3

-- congruence rules
eval1 (Succ e1) = Succ (eval1 e1)
eval1 (Pred e1) = Pred (eval1 e1)
eval1 (IsZero e1) = IsZero (eval1 e1)
eval1 (If e1 e2 e3) = If (eval1 e1) e2 e3
eval1 _ = error "Invalid Expression"

-- full evaluator
eval :: Expr -> Expr
eval e
  | isValue e = e
  | otherwise = eval (eval1 e)


-- type checker
typeof :: Expr -> Type
typeof T = Boolean
typeof F = Boolean
typeof O = Nat

typeof (Succ nv1)
  | (typeof nv1 == Nat) = Nat
  | otherwise = NoType


typeof (Pred nv1)
  | (typeof nv1 == Nat) = Nat
  | otherwise = NoType

typeof (IsZero nv1)
  | (typeof nv1 == Nat) = Boolean
  | otherwise = NoType

typeof (If e1 e2 e3)
  | (typeof e1 == Boolean) && (typeof e2 == typeof e3) = typeof e2
  | otherwise = NoType

