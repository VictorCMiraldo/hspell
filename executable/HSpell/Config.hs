{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# OPTIONS_GHC -Wno-orphans    #-}
module HSpell.Config where

import           GHC.Generics
import           Data.Aeson
import qualified Data.Yaml as Y
import qualified Data.ByteString as BS
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

loadConfigFromFile :: FilePath -> IO (Either Y.ParseException HSpellConfig)
loadConfigFromFile path = Y.decodeEither' <$> BS.readFile path
