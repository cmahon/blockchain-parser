module Bitcoin.Bitcoind where

import Control.DeepSeq
import Control.Monad
import Data.Bitcoin.Block
import Data.Bitcoin.Types
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import Data.Sequence (Seq,(<|))
import qualified Data.Sequence as S
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

import Bitcoin.Types
import Types
import Util

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

-----------------------------------------------------------------------------

getDataDir :: IO FilePath
getDataDir = case os of
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

rawBlockP :: FilePath -> Producer BS.ByteString (PS.SafeT IO) ()
rawBlockP bdir = liftIO (blockFiles' bdir) >>= mconcat . map readFile

block'P :: FilePath -> Producer Block' (PS.SafeT IO) ()
block'P bp = rawBlockP bp >-> decodeBinary parseBlock'

block'P' :: FilePath -> Producer Block' (PS.SafeT IO) ()
block'P' = void . view PB.decoded . rawBlockP

blockP :: FilePath -> Producer Block (PS.SafeT IO) ()
blockP bp = block'P bp >-> P.map _block'Block

-----------------------------------------------------------------------------

initBlockMap :: NFData a =>
                FilePath ->
                Maybe Int ->
                (Block' -> a) ->
                IO (BlockMap a)
initBlockMap blksdir numblocks f = 
  PS.runSafeT $ P.fold insertBlockMap Map.empty id $
    block'P' blksdir
    >-> P.map ((,,) <$> _block'Hash 
                    <*> (_prevBlock . _blockHeader . _block'Block)
                    <*> f)
    >-> maybe cat P.take numblocks
    >-> deepseqP

insertBlockMap :: BlockMap a ->
                  (BlockHash,BlockHash,a) ->
                  BlockMap a
insertBlockMap bm (bh,ph,d) = 
  let br = BlockRec Nothing ph Nothing d
  in  Map.insert bh br bm

-----------------------------------------------------------------------------

updateBlockRec :: BlockDB a -> 
                  BlockHash ->
                  (BlockDB a,BlockRec a)
updateBlockRec bdb bh =   
  let m          = _blockdbMap bdb
      Just r     = Map.lookup bh m
  in  maybe (updateDepth bdb bh r) (const (bdb,r)) (_blockrecDepth r)

updateBlockRec' :: BlockDB a -> BlockHash -> BlockDB a
updateBlockRec' bdb = fst . updateBlockRec bdb

-----------------------------------------------------------------------------

updateDepth :: BlockDB a -> 
               BlockHash ->
               BlockRec a ->
               (BlockDB a,BlockRec a)
updateDepth bdb bh r = 
  let m            = _blockdbMap bdb
      ph           = _blockrecPrevHash r      
      mpr          = Map.lookup ph m
      (r',bdb') = case mpr of 
        Nothing ->
          let _r' = r { _blockrecDepth = Just 0
                      , _blockrecPrevRec = Nothing }
          in  (_r',bdb)
        Just pr -> 
          let (_bdb',pr') = maybe (updateBlockRec bdb ph) (const (bdb,pr)) (_blockrecDepth pr)
              Just pd' = _blockrecDepth pr'
              _r'      = r { _blockrecDepth = Just (pd' + 1)
                           , _blockrecPrevRec = Just pr'}
          in  (_r',_bdb')
      m'           = _blockdbMap bdb'
      m''          = Map.insert bh r' m'
      hd'          = _blockdbHead bdb'
      hd''         = Just $ maybe (bh,r') (maxBy (_blockrecDepth . snd)  (bh,r')) hd'
      bdb''        = BlockDB m'' hd''
  in  (bdb'',r')

-----------------------------------------------------------------------------

mainChain :: BlockDB a -> Seq (BlockRec a)
mainChain BlockDB{ _blockdbHead = Nothing }    = S.empty
mainChain BlockDB{ _blockdbHead = Just (_,h) } = go S.empty h
 where
  go c r@BlockRec{ _blockrecPrevRec = Nothing } = r <| c
  go c r@BlockRec{ _blockrecPrevRec = Just p  } = go (r <| c) p

