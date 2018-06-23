-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE KindSignatures #-}
data Area = Length | Force | Time | Momentum | Velocity | Mass
    deriving (Show, Eq)
data Dimension = Dimension Area String
    deriving (Show)
data Value = Value Double Dimension
    deriving (Eq)

-- data, newtype, type
metre = Dimension Length "m"
newtone = Dimension Force "N"

class Conversionable a where
    convertToSI :: a -> a
    -- convertToAnother :: a -> a

instance Conversionable Value where
    convertToSI (Value num (Dimension ar dim))
        | dim == "cm" = Value (num / 100.0) metre
        | dim == "mm" = Value (num / 1000.0) metre

instance Eq Dimension where
    Dimension x _ == Dimension y _ = x == y

instance Show Value where
    show (Value num (Dimension _ dim)) = show num ++ " " ++ dim

-- instance Read Value where
--     readsPrec _ input = do
--         let (num, dim) = words input
--             read num :: Double
--             in
--         Value num dim

-- + - * / exp (^) sin cos tan log
a = Value 5.0 metre
b = Value 6.0 metre

-- infixl :+:
add :: Value -> Value -> Value
add (Value num1 dim1) (Value num2 dim2)
    | dim1 == dim2 = Value (num1 + num2) dim1
    | otherwise    = error "Dimensions doesn't match"

main :: IO ()
main = print $ add a b
