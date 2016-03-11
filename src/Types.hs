module Types where

import Control.DeepSeq
import qualified Data.Map.Strict as Map
import Data.HexString

-----------------------------------------------------------------------------

instance NFData HexString where
  rnf = rnf . toBytes

-----------------------------------------------------------------------------

data BlockRec a = BlockRec
  { _blockrecDepth    :: !(Maybe Int)
  , _blockrecPrevHash :: !HexString
  , _blockrecPrevRec  :: !(Maybe (BlockRec a))
  , _blockrecData     :: !a
  } deriving Show

-----------------------------------------------------------------------------

type BlockMap a = Map.Map HexString (BlockRec a)

-----------------------------------------------------------------------------

data BlockDB a = BlockDB 
  { _blockdbMap  :: !(BlockMap a)
  , _blockdbHead :: !(Maybe (HexString,BlockRec a))
  } deriving Show

newBlockDB :: BlockDB a
newBlockDB = BlockDB
  { _blockdbMap  = Map.empty
  , _blockdbHead = Nothing
  }

