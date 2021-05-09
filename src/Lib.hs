{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
  ( someFunc
  ) where

import Data.Functor ((<&>))

import qualified Hoogle
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

removeHtml :: String -> String
removeHtml = symbols . tags False
  where
    -- remove html tags
    tags _ [] = []
    tags False ('<':xs) = tags True xs
    tags True  ('>':xs) = tags False xs
    tags False (x:xs)   = x:tags False xs
    tags True  (x:xs)   = tags True xs

    -- replace html symbols
    symbols [] = []
    symbols ('&':'g':'t':';':xs) = '>':symbols xs
    symbols ('&':'l':'t':';':xs) = '<':symbols xs
    symbols (x:xs) = x:symbols xs

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
  let Just targets = Aeson.decode (HTTP.responseBody res) :: Maybe [Hoogle.Target]
  putStrLn $ unlines $ map (removeHtml . Hoogle.targetItem) targets

bs :: String -> ByteString
bs = Text.encodeUtf8 . Text.pack
