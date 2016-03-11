module Bitcoin.Types where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bitcoin.Block
import Data.Bitcoin.Types

-----------------------------------------------------------------------------

data Network = 
    BTCMain
  | BTCTest
    deriving (Read,Show)

parseNetwork :: Get Network
parseNetwork = getWord32le >>= \case
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

data Block' = Block'
  { _block'Network :: !Network
  , _block'Size :: !Word32
  , _block'Hash :: !BlockHash
  , _block'Block :: !Block
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
