-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE KindSignatures #-}
data Area = Length | Force | Time | Momentum | Velocity | Mass
    deriving (Show, Eq)
data Dimension = Dimension Area String
    deriving (Show)
data Value = Value Double Dimension
    deriving (Show, Eq)

-- type Meter = (Area Length) "m"

instance Eq Dimension where
    Dimension x _ == Dimension y _ = x == y

instance Read Value where
    readsPrec num dim =
        read num :: Double
        read dim
        Value num dim

-- + - * / exp (^) sin cos tan log
a = Value 5.0 (Dimension Length "m")
b = Value 6.0 (Dimension Length "m")

-- infixl :+: 
add :: Value -> Value -> Value
add (Value num1 dim1) (Value num2 dim2)
    | dim1 == dim2 = Value (num1 + num2) dim1
    | otherwise    = error "Dimensions doesn't match"

main :: IO ()
main = do
    read a :: Int -- :: Value
    -- b <- read -- :: Value
    print $ a + 1
