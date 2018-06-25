import Data.Hashable

data Block = Block {
                    hash      :: String,
                    prevHash  :: String,
                    timestamp :: String,
                    nonce     :: Int,
                    data_     :: String
                    }

data BlockChain = BlockChain {
                    blocks :: [Block],
                    diffic :: Int
                    }

addToB :: String -> [String] -> [String]
addToB s [] = [s]
addToB s xs = xs ++ [show (hash $ last xs) ++ "\n" ++ s]

-- serialize, deserialize

main = print $ addToB "ss" $ addToB "foo" []
