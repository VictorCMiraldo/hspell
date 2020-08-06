{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# OPTIONS_GHC -Wno-orphans    #-}
module HSpell.Config where

import           GHC.Generics
import           System.IO
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
-----
import Text.HSpell.Syntax.Dict (SymSpellVerbosity(..))

data HSpellConfig = HSpellConfig
  { dictionaries     :: [FilePath]
  , dictMaxDist      :: Int
  , dictPrefixLength :: Int
  } deriving (Eq , Show , Generic)

deriving instance Generic SymSpellVerbosity
instance ToJSON SymSpellVerbosity
instance FromJSON SymSpellVerbosity

instance ToJSON   HSpellConfig
instance FromJSON HSpellConfig where
    parseJSON = withObject "HSpellConfig" $ \c -> HSpellConfig
        <$> c .:? "dictionaries"      .!= []
        <*> c .:? "dictMaxDist"       .!= 2
        <*> c .:? "dictPrefixLength"  .!= 4

loadConfigFromFile :: FilePath -> IO (Maybe HSpellConfig)
loadConfigFromFile path = decode' <$> BS.readFile path
