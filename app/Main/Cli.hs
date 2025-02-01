module Main.Cli (parseCliOptions) where

import Colog (Severity (Info))
import HydraScraper.Types
import HydraScraper.Utils (emacsOverlayJobsetUrl)
import Main.Types
import Options.Applicative (Parser)
import Options.Applicative qualified as O
import Relude

parseCliOptions :: IO Options
parseCliOptions = O.execParser (O.info (O.helper <*> optionsParser) (O.progDesc "A hydra scraper"))

optionsParser :: Parser Options
optionsParser = Options <$> optionsCommandParser <*> optionsOutputParser <*> optionsLogLevelParser

optionsCommandParser :: Parser Command
optionsCommandParser =
  O.hsubparser
    ( O.command "predefined" (O.info (PredefinedQuery <$> predefinedQueryOptionsParser) (O.progDesc "Use pre-defined queries"))
        <> O.command "manual" (O.info (ManualQuery <$> manualQueryOptionsParser) (O.progDesc "Use manually-defined queries"))
    )

optionsOutputParser :: Parser PrintFormat
optionsOutputParser = optionsOutputHumanReadableParser <|> optionsOutputMachineReadableParser

optionsLogLevelParser :: Parser Severity
optionsLogLevelParser =
  O.option
    O.auto
    ( O.long "log-level"
        <> O.short 'l'
        <> O.value Info
        <> O.showDefault
        <> O.metavar "LOG-LEVEL"
        <> O.help (show [minBound :: Severity ..])
    )

optionsOutputHumanReadableParser :: Parser PrintFormat
optionsOutputHumanReadableParser =
  O.flag'
    HumanReadable
    ( O.long "human-readable"
        <> O.long "hu"
        <> O.help "Output using a human-readable format"
    )

optionsOutputMachineReadableParser :: Parser PrintFormat
optionsOutputMachineReadableParser =
  MachineReadable
    <$> O.strOption
      ( O.long "machine-readable"
          <> O.short 'm'
          <> O.metavar "OUTPUT-FILE"
          <> O.help "Output using a machine-readable format to this file"
      )

predefinedQueryOptionsParser :: Parser PredefinedQueryOptions
predefinedQueryOptionsParser =
  PredefinedQueryOptions
    <$> O.option
      O.auto
      ( O.long "target"
          <> O.short 't'
          <> O.value EmacsOverlay
          <> O.showDefault
          <> O.metavar "TARGET"
          <> O.help (show [minBound :: PredefinedQueryOptionsTarget ..])
      )

manualQueryOptionsParser :: Parser ManualQueryOptions
manualQueryOptionsParser = ManualQueryOptions <$> urlParser <*> jobPrefixFilterParser <*> noPostFilterParser

urlParser :: Parser Url
urlParser =
  O.strOption
    ( O.long "url"
        <> O.short 'u'
        <> O.metavar "URL"
        <> O.value emacsOverlayJobsetUrl
        <> O.showDefaultWith (toString . display)
        <> O.help "Jobset url"
    )

jobPrefixFilterParser :: Parser JobPrefixFilter
jobPrefixFilterParser =
  O.strOption
    ( O.long "job-prefix-filter"
        <> O.short 'f'
        <> O.metavar "JOB-PREFIX-FILTER"
        <> O.help "Job prefix filter"
    )

noPostFilterParser :: Parser Bool
noPostFilterParser =
  O.switch
    ( O.long "no-post-filter"
        <> O.long "npf"
        <> O.help "Do not do any post filtering"
    )
