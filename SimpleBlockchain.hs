import Data.Hashable
import Crypto.Hash.SHA1

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

lastBlock :: BlockChain -> Block
lastBlock b = last $ blocks b

-- mineBlock :: BlockChain -> String -> BlockChain
-- mineBlock b s = do
--     let prevHash = lastBlock b

addToB :: String -> [String] -> [String]
addToB s [] = [s]
addToB s xs = xs ++ [show (hash $ last xs) ++ "\n" ++ s]

-- serialize, deserialize

main = print $ addToB "ss" $ addToB "foo" []
