{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
  ( someFunc
  ) where

import Data.Functor ((<&>))

import Hoogle (Target)
import Data.ByteString (ByteString)

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP (tlsManagerSettings)
import qualified Options.Applicative as O

data Options = Options
  { search :: String
  , count :: Int
  }

options :: O.ParserInfo Options
options = O.info parser $ O.header " \
  \Hoogle CLI is a command line interface to Hoogle.\
  \Hoogle is a Haskell API search engine, which allows you to search the Haskell libraries on Stackage by either function name, or by approximate type signature."
  where
    parser = Options
      <$> O.strArgument
          (O.metavar "SEARCH"
          <> O.help "String to search for")
      <*> O.option O.auto
          (O.long "count"
          <> O.metavar "INT"
          <> O.help "String to search for"
          <> O.value 20)

someFunc :: IO ()
someFunc = do
  Options{..} <- O.execParser options
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  req <- HTTP.parseRequest "https://hoogle.haskell.org"
    <&> HTTP.setQueryString
      [ ("mode", Just "json")
      , ("start", Just "1")
      , ("count", Just $ bs $ show count)
      , ("hoogle", Just $ bs search)
      ]
  res <- HTTP.httpLbs req manager
  let targets = Aeson.decode (HTTP.responseBody res) :: Maybe [Target]
  print targets

bs :: String -> ByteString
bs = Text.encodeUtf8 . Text.pack
