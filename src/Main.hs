{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Data.Bits (popCount)
import Data.DateTime
import Data.Maybe (mapMaybe)
import Data.Hash
import Data.Word (Word64 (..))
import System.Random

type Nonce = Integer
type Difficulty = Int
type Ref = Word64
type Transaction = String

data Block = Block
    { parent       :: Ref
    , nonce        :: Nonce
    , transactions :: [Transaction]
    } deriving (Show)
type BlockChain = [Block]

fingerprint :: Block -> Ref
fingerprint Block { .. } =
    asWord64 $ foldl1 combine
        [ hash parent
        , hash nonce
        , hash transactions
        ]

difficulty :: Block -> Difficulty
-- TODO(sandy): use countLeadingZeros instead
difficulty = (64 -) . popCount . fingerprint

mineMaybe :: Difficulty -> Ref -> [Transaction] -> Nonce -> Maybe Block
mineMaybe d p ts n = do
    let b = Block p n ts
    guard $ difficulty b >= d
    return b

mine :: Difficulty -> Ref -> [Transaction] -> IO Block
mine d p ts = do
    -- TODO(sandy): make this more reasonable
    initialNonce <- getStdRandom (randomR (0, 100))
    return . head . flip mapMaybe [0..] $ mineMaybe d p ts . (initialNonce +)
