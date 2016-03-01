{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Bitcoin where

import Control.DeepSeq
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bitcoin.Block
import Data.Bitcoin.Types
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.HexString
import Data.List
import Data.Maybe
import Lens.Family
import Pipes
import qualified Pipes.Binary as PB
import qualified Pipes.Prelude as P
import qualified Pipes.Safe as PS
import Prelude hiding (readFile)
import System.Directory
import System.Environment
import System.FilePath
import System.Info (os)
import Text.Printf

import PipesExtras
import Util

-----------------------------------------------------------------------------

instance NFData HexString where
  rnf = rnf . toBytes

-----------------------------------------------------------------------------

data Network = 
    BTCMain
  | BTCTest
    deriving (Read,Show)

parseNetwork :: Get Network
parseNetwork = do
  w <- getWord32le
  case w of
    0xd9b4bef9 -> return BTCMain
    0x0709110b -> return BTCTest
    _          -> fail "parse network failure" 

encodeNetwork :: Network -> Word32
encodeNetwork BTCMain = 0xd9b4bef9
encodeNetwork BTCTest = 0x0709110b

instance Binary Network where
  get = parseNetwork
  put = putWord32le . encodeNetwork

-----------------------------------------------------------------------------

blockFileNameTemplate :: String
blockFileNameTemplate = "blk%0.5d.dat"

blockFileName :: Int -> FilePath
blockFileName = printf blockFileNameTemplate

blocksDir :: Network -> FilePath -> FilePath
blocksDir BTCMain dd = dd </> "blocks"
blocksDir BTCTest dd = dd </> "testnet3" </> "blocks"

blockFilePath :: Network -> FilePath -> Int -> FilePath
blockFilePath nw fp i = blocksDir nw fp </> blockFileName i

blockFiles :: Network -> FilePath -> IO [FilePath]
blockFiles nw dd = blockFiles' (blocksDir nw dd)

blockFiles' :: FilePath -> IO [FilePath]
blockFiles' bd = do
  fs <- getDirectoryContents bd
  let bfs = sort . filter (isPrefixOf "blk") $ fs
  return $ map (bd </>) bfs

dataDir :: IO FilePath
dataDir = case os of
  "mingw"   -> windows
  "mingw32" -> windows
  "mingw64" -> windows
  "darwin"  -> osx
  "linux"   -> unix
  _         -> unix
 where
  homeM = lookupEnv "HOME"
  windows = do
    localAppData <- lookupEnv "LOCALAPPDATA"
    dirM <- case localAppData of
        Nothing -> lookupEnv "APPDATA"
        Just l -> return $ Just l
    case dirM of
        Just d -> return $ d </> "Bitcoin"
        Nothing -> return "."
  osx = homeM >>= \case
    Just home -> return $ home </> "Library"
                               </> "Application Support" </> "Bitcoin"
    Nothing -> return "."
  unix = homeM >>= \case
    Just home -> return $ home </> ".bitcoin"
    Nothing -> return "."

-----------------------------------------------------------------------------

data Block' = Block'
  { _block'Network :: Network
  , _block'Size :: Word32
  , _block'Hash :: BlockHash
  , _block'Block :: Block
  } deriving Show

parseBlock' :: Get Block'
parseBlock' = do
  _block'Network <- parseNetwork
  _block'Size <- getWord32le
  _block'Block <- get
  let _block'Hash = headerHash _block'Block
  return Block'{..}

instance Binary Block' where
  get = parseBlock'
  put Block'{..} = do
    put _block'Network
    putWord32le _block'Size
    put _block'Block

rawBlockP :: FilePath -> Producer BS.ByteString (PS.SafeT IO) ()
rawBlockP bdir = liftIO (blockFiles' bdir) >>= mconcat . map readFile

block'P :: FilePath -> Producer Block' (PS.SafeT IO) ()
block'P bp = rawBlockP bp >-> decodeBinary parseBlock'

block'P' :: FilePath -> Producer Block' (PS.SafeT IO) ()
block'P' = void . view PB.decoded . rawBlockP

blockP :: FilePath -> Producer Block (PS.SafeT IO) ()
blockP bp = block'P bp >-> P.map _block'Block

-----------------------------------------------------------------------------

data BlockRec a = BlockRec
  { _blockrecDepth    :: Maybe Int
  , _blockrecPrevHash :: BlockHash
  , _blockrecPrevRec  :: Maybe (BlockRec a)
  , _blockrecData     :: a
  } deriving Show

type BlockMap a = Map.Map BlockHash (BlockRec a)

data BlockDB a = BlockDB 
  { _blockdbMap  :: BlockMap a
  , _blockdbHead :: Maybe (BlockRec a)
  }

newBlockDB :: BlockDB a
newBlockDB = BlockDB
  { _blockdbMap  = Map.empty
  , _blockdbHead = Nothing
  }

insertBlockDB :: BlockMap a -> (BlockHash,BlockHash,a) -> BlockMap a
insertBlockDB bm (bh,ph,d) = 
  let br = BlockRec Nothing ph Nothing d
  in  Map.insert bh br bm


updateDepth :: BlockDB a -> BlockHash -> (BlockDB a,Int)
updateDepth bdb bh =   
  let m          = _blockdbMap bdb
      h          = _blockdbHead bdb
      Just r     = Map.lookup bh m
      ph         = _blockrecPrevHash r
      pr         = Map.lookup ph m
      (d',r',m') = case pr of 
        Nothing ->
          let _d' = 0 
              _r' = r { _blockrecDepth = Just _d' }
          in  (_d',_r',m)
        Just BlockRec{_blockrecDepth = Just d} ->
          let _d' = d + 1
              _r' = r { _blockrecDepth = Just _d' }
          in  (_d',_r',m)
        Just br ->
          let (_bdb',_d) = updateDepth bdb ph
              _m'        = _blockdbMap _bdb'
              _d'        = _d + 1
              _r'        = r { _blockrecDepth = Just _d' }
          in  (_d',_r',_m')
      m''        = Map.insert bh r' m'
      h'         = Just $ maybe r' (maxBy _blockrecDepth r') h
      bdb'       = BlockDB m'' h'
  in  (bdb',d')

updateDepth' :: BlockDB a -> BlockHash -> BlockDB a
updateDepth' bdb = fst . updateDepth bdb

