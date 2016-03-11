module Client where

import Data.Monoid
import Options.Applicative
import System.Directory

import Bitcoin.Bitcoind
import Bitcoin.Types

-----------------------------------------------------------------------------

data Options = Options
  { _optionsDataDir :: Maybe FilePath
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

optionsInfo :: ParserInfo Options
optionsInfo = info (helper <*> options)
  (fullDesc
   <> progDesc "Parse the bitcoin blockchain"
   <> header "blockchain parser")

-----------------------------------------------------------------------------

updateDataDir :: Maybe FilePath -> IO (Maybe FilePath)
updateDataDir = \case
  Nothing -> Just <$> getDataDir
  Just d  -> doesDirectoryExist d >>= \case 
    True  -> return $ Just d
    False -> return Nothing

-----------------------------------------------------------------------------

run :: (Options -> IO ()) -> IO ()
run f = execParser optionsInfo >>= f
