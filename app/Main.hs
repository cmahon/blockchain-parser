{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.DeepSeq
import qualified Control.Foldl as L
import qualified Data.Map.Strict as Map
import Data.Bitcoin.Block
import Data.Foldable
import GHC.Generics
import System.Exit
import Text.Printf

import Bitcoin
import Client

-----------------------------------------------------------------------------

main :: IO ()
main = run analysis

-----------------------------------------------------------------------------

analysis :: Options -> IO ()
analysis Options{..} = do

  dataDir <- updateDataDir _optionsDataDir >>= maybe exitFailure return
  let blksDir = blocksDir _optionsNetwork dataDir

  m <- initBlockMap blksDir Nothing extractBlockStats
 
  let hs    = Map.keys m
      bdb   = BlockDB m Nothing
      bdb'  = foldl' updateBlockRec' bdb hs
      chain = mainChain bdb'
      stats = L.fold chainStatsFold chain

  printf "Blocks processed: %d\n" $ length hs
  printf "Blocks in main chain: %d\n" $ length chain
  print stats

-----------------------------------------------------------------------------

data BlockStats = BlockStats
  { _blockstatsNumTxns :: Int
  } deriving (Generic,Show)

instance NFData BlockStats

extractBlockStats :: Block' -> BlockStats
extractBlockStats = BlockStats . length . _blockTxns . _block'Block

data ChainStats = ChainStats
  { chainstatsNumBlocks        :: Int
  , chainstatsNumTxns          :: Int
  , chainstatsMeanTxnsPerBlock :: Double
  , chainstatsMinTxnsPerBlock  :: Maybe Int
  , chainstatsMaxTxnsPerBlock  :: Maybe Int
  } deriving Show

chainStatsFold :: L.Fold (BlockRec BlockStats) ChainStats
chainStatsFold = L.premap (_blockstatsNumTxns . _blockrecData) $ ChainStats
  <$> L.length
  <*> L.sum
  <*> L.premap fromIntegral mean
  <*> L.minimum
  <*> L.maximum

-----------------------------------------------------------------------------

mean :: L.Fold Double Double
mean = (/) <$> L.sum <*> L.genericLength

