{-# LANGUAGE RankNTypes #-}

module PipesExtras where

import Control.DeepSeq
import Control.Monad
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL
import Pipes
import qualified Pipes.ByteString as PBS
import qualified Pipes.Safe as PS
import qualified System.IO as IO

-----------------------------------------------------------------------------

deepseqP :: (Monad m,NFData a) => Pipe a a m r
deepseqP = for cat $ \a -> yield $!! a

readFile :: FilePath -> Producer' BS.ByteString (PS.SafeT IO) ()
readFile = readFile' True

readFile' :: Bool -> FilePath -> Producer' BS.ByteString (PS.SafeT IO) ()
readFile' verbose file = PS.bracket
    (do h <- IO.openFile file IO.ReadMode
        when verbose $ putStrLn $ "{" ++ file ++ " open}"
        return h )
    (\h -> do
        IO.hClose h
        when verbose $ putStrLn $ "{" ++ file ++ " closed}" )
    PBS.fromHandle

decodeBinary :: (Monad m) => Get a -> Pipe BS.ByteString a m () -- r 
decodeBinary p = go0 d0
 where

  d0 = runGetIncremental p

  go0 d = do
    b <- await
    go1 d (BL.fromStrict b)
  
  go1 Fail{} _ = return () -- error msg
  
  go1 (Done leftover _ x) input = do
    yield x
    case (BS.null leftover, BL.null input) of
      (False,False) -> go1 d0 (BL.Chunk leftover input)
      (False,True)  -> go1 d0 (BL.fromStrict leftover)
      (True,False)  -> go1 d0 input
      (True,True)   -> go0 d0
  
  go1 d1@(Partial k) input
    | BL.null input = go0 d1
    | otherwise     = go1 (k . takeHeadChunk $ input) (dropHeadChunk input)
  
takeHeadChunk :: BL.ByteString -> Maybe BS.ByteString
takeHeadChunk lbs = case lbs of
  (BL.Chunk bs _) -> Just bs
  _               -> Nothing

dropHeadChunk :: BL.ByteString -> BL.ByteString
dropHeadChunk lbs = case lbs of
  (BL.Chunk _ lbs') -> lbs'
  _                 -> BL.empty