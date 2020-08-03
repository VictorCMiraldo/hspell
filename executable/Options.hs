module Options where

import qualified Text.HSpell.Syntax.Dict as HS
import Options.Applicative

readmOneOf :: [(String, a)] -> ReadM a
readmOneOf = maybeReader . flip lookup

data HSpellOptions = HSpellOptions
  { optsDicts      :: [FilePath]
  , optsVerb       :: HS.SymSpellVerbosity
  , optsConfig     :: Maybe FilePath
  , optsWorkOnFile :: FilePath
  } deriving (Eq , Show)

optConfig :: Parser (Maybe FilePath)
optConfig = option (fmap Just str)
          $ long "config"
         <> metavar "FILE"
         <> help "Uses this provided configuration file instead of the default one."
         <> value Nothing
        
optDict :: Parser FilePath
optDict = option str
   $ long "dict"
  <> short 'd'
  <> metavar "FILE"
  <> help "Additional dictionary to load"

optDicts :: Parser [FilePath]
optDicts = many optDict

optSymSpellVerb :: Parser HS.SymSpellVerbosity 
optSymSpellVerb = option (readmOneOf [("all"    , HS.All)
                                     ,("closest", HS.Closest)
                                     ,("top"    , HS.Top)])
                (long "syn-suggest"
                <> metavar "all | closest | top ; default: closest"
                <> value HS.Closest
                <> help "Configures how many syntax suggestions to show")

optsParser :: Parser HSpellOptions
optsParser = HSpellOptions
  <$> optDicts
  <*> optSymSpellVerb
  <*> optConfig
  <*> argument str (metavar "FILE" <> help "File to spell check")

optsParserInfo :: ParserInfo HSpellOptions
optsParserInfo = info optsParser $
  fullDesc <> progDesc "Spells checks things"
           <> header "hpsell"

processCLIOptions :: IO HSpellOptions
processCLIOptions = customExecParser p optsParserInfo
 where
   p = prefs showHelpOnEmpty


