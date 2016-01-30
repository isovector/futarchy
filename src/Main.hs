{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<$>))
import Control.Monad
import Data.Bits (popCount)
import Data.DateTime
import Data.IORef
import Data.Maybe (mapMaybe)
import Data.Map (Map (..))
import qualified Data.Map as M
import Data.Hash
import Data.Word (Word64 (..))
import System.Random
import System.IO.Unsafe (unsafePerformIO)

type Nonce = Integer
type Difficulty = Int
type Ref = Word64

data Choice = A | B | C | D deriving Show
type Vote = Block (Maybe Choice)

instance Hashable Choice where
    hash A = hash (1 :: Int)
    hash B = hash (2 :: Int)
    hash C = hash (3 :: Int)
    hash D = hash (4 :: Int)

data Block a = Block
    { parent :: Ref
    , depth  :: Integer
    , nonce  :: Nonce
    , vote   :: a
    } deriving Show
type BlockChain a = [Block a]
rootBlock = Block 0 0 0 Nothing

knownBlocks :: IORef (Map Ref Vote)
knownBlocks = unsafePerformIO
            . newIORef
            $ M.singleton (fingerprint (rootBlock :: Vote))
                          rootBlock

workingParent :: IORef Vote
workingParent = unsafePerformIO . newIORef $ rootBlock

learn :: Vote -> IO ()
learn b = do
    modifyIORef knownBlocks $ \m -> M.insert (fingerprint b) b m
    working <- readIORef workingParent
    when (depth b > depth working) $ writeIORef workingParent b

fingerprint :: Hashable a => Block a -> Ref
fingerprint Block { .. } =
    asWord64 $ foldl1 combine
        [ hash parent
        , hash depth
        , hash nonce
        , hash vote
        ]

difficulty :: Hashable a => Block a -> Difficulty
-- TODO(sandy): use countLeadingZeros instead
difficulty = (64 -) . popCount . fingerprint

mineMaybe :: Hashable a
          => Difficulty
          -> Block a
          -> a
          -> Nonce
          -> Maybe (Block a)
mineMaybe d p v n = do
    let b = Block (fingerprint p) (depth p + 1) n v
    guard $ difficulty b >= d
    return b

firstOf :: Monad m => a -> [m (Maybe a)] -> m a
firstOf a [] = return a
firstOf a (io:ios) = do
    io >>= \case
        Just x  -> return x
        Nothing -> firstOf a ios


mine :: Hashable a
     => Difficulty
     -> IORef (Block a)
     -> a
     -> IO (Block a)
mine d pio v = do
    initialNonce <- getStdRandom $ randomR (0, 100000000)
    firstOf undefined . flip map [0..] $ \n -> do
        p <- readIORef pio
        return . mineMaybe d p v $ initialNonce + n
