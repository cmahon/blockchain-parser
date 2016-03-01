{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Data.Bitcoin.Block
import qualified Data.Map.Strict as Map
import Data.List
import Data.Monoid
import Options.Applicative
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Safe as PS
import Prelude hiding (readFile)
import System.Directory

import Bitcoin
import PipesExtras

-----------------------------------------------------------------------------

data Options = Options
  { _optionsDataDir :: Maybe String
  , _optionsNetwork :: Network
  }

options :: Parser Options
options = Options
  <$> optional (strOption
      ( long "datadir"
        <> metavar "DATADIR"
        <> help "Blockchain data directory" ))
  <*> option auto
      ( long "network"
        <> short 'n'
        <> metavar "NETWORK"
        <> help "[BTCMain|BTCTest]"
        <> value BTCMain)

-----------------------------------------------------------------------------

main :: IO ()
main = execParser opts >>= run
 where
  opts = info (helper <*> options)
              (fullDesc
               <> progDesc "Parse the bitcoin blockchain"
               <> header "blockchain parser")

-----------------------------------------------------------------------------

run :: Options -> IO ()
run Options{..} = do

  dataPath <- case _optionsDataDir of
    Nothing -> dataDir
    Just d  -> doesDirectoryExist d >>= \case 
      True  -> return d
      False -> dataDir

  m <- PS.runSafeT $ P.fold insertBlockDB Map.empty id $
      block'P' (blocksDir BTCMain dataPath)
      >-> P.map ((,,) <$> _block'Hash 
                      <*> (_prevBlock . _blockHeader . _block'Block)
                      <*> const ())
      >-> P.take 100000
      >-> deepseqP

  let hs       = Map.keys m
      bdb      = BlockDB m Nothing
      bdb' = foldl' updateDepth' bdb hs

  print (length hs)
  print $ _blockdbHead bdb'

