-- Extend the prettyShow function to remove unnecessary parentheses.

{-- From examples/examples/ch13/num.hs modified according to the assignment --}
import Data.List

data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq, Show)

data SymbolicManip a =
          Number a           -- Simple number, such as 5
        | Symbol String      -- A symbol, such as x
        | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
        | UnaryArith String (SymbolicManip a)
          deriving (Eq)

instance Num a => Num (SymbolicManip a) where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b
    a * b = BinaryArith Mul a b
    negate a = BinaryArith Mul (Number (-1)) a
    abs a = UnaryArith "abs" a
    signum _ = error "signum is unimplemented"
    fromInteger i = Number (fromInteger i)


instance (Fractional a) => Fractional (SymbolicManip a) where
    a / b = BinaryArith Div a b
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)

instance (Floating a) => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp a = UnaryArith "exp" a
    log a = UnaryArith "log" a
    sqrt a = UnaryArith "sqrt" a
    a ** b = BinaryArith Pow a b
    sin a = UnaryArith "sin" a
    cos a = UnaryArith "cos" a
    tan a = UnaryArith "tan" a
    asin a = UnaryArith "asin" a
    acos a = UnaryArith "acos" a
    atan a = UnaryArith "atan" a
    sinh a = UnaryArith "sinh" a
    cosh a = UnaryArith "cosh" a
    tanh a = UnaryArith "tanh" a
    asinh a = UnaryArith "asinh" a
    acosh a = UnaryArith "acosh" a
    atanh a = UnaryArith "atanh" a


prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) =
    let pa = simpleParen a op
        pb = simpleParen b op
        pop = op2str op
        in pa ++ pop ++ pb
prettyShow (UnaryArith opstr a) =
    opstr ++ "(" ++ show a ++ ")"


op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"


simpleParen :: (Show a, Num a) => SymbolicManip a -> Op -> String
simpleParen (Number x) _ = prettyShow (Number x)
simpleParen (Symbol x) _ = prettyShow (Symbol x)
-- If a binary arithmetic expression is a neighbor of a higher priority
-- operator, it needs to be protected from it by parentheses. Otherwise, no need
-- to use parentheses as it will be evaluated with expected priority.
simpleParen x@(BinaryArith op _ _) parentOp = if opHasHigherPriority parentOp op
                                              then "(" ++ prettyShow x ++ ")"
                                              else prettyShow x
simpleParen x@(UnaryArith _ _) _ = prettyShow x


opHasHigherPriority :: Op -> Op -> Bool
opHasHigherPriority a b = (opPrio a) > (opPrio b)
  where
    opPrio Plus = 0
    opPrio Minus = 0
    opPrio Mul = 1
    opPrio Div = 1
    opPrio Pow = 2


instance (Show a, Num a) => Show (SymbolicManip a) where
    show a = prettyShow a
{-- End of code from examples --}


-- ghci> :l 13_a_1.hs
-- [1 of 1] Compiling Main             ( 13_a_1.hs, interpreted )
-- Ok, one module loaded.

-- ghci> prettyShow ((Number 1) / (Number 2) * (Number 3))
-- "1.0/2.0*3.0"

-- ghci> prettyShow ( ( (Number 1) + (Number 2) + (Number 3) ) * (Number 4) )
-- "(1+2+3)*4"
-- ghci> prettyShow ( (Number 1) + (Number 2) + (Number 3) * (Number 4) )
-- "1+2+3*4"
-- ghci> prettyShow ( (Number 1) + (Number 2) + ( (Number 3) * (Number 4) ) )
-- "1+2+3*4"

-- ghci> prettyShow ( (Number 0) * ( (Number 1) + (Number 2) + (Number 3) ) )
-- "0*(1+2+3)"
-- ghci> prettyShow ( (Number 0) * (Number 1) + (Number 2) + (Number 3) )
-- "0*1+2+3"
-- ghci> prettyShow ( ( (Number 0) * (Number 1) ) + (Number 2) + (Number 3) )
-- "0*1+2+3"
