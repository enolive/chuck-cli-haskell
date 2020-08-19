{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Text
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.Environment (getArgs)

newtype JokeResponse = JokeResponse {value :: Text}
  deriving (Generic, Show, FromJSON)

newtype CategoriesResponse = CategoriesResponse [Text]
  deriving (Generic, Show, FromJSON)

host :: String
host = "https://api.chucknorris.io/jokes"

fetchRandomJoke :: IO Text
fetchRandomJoke =
  value . getResponseBody
    <$> (parseRequest (host <> "/random") >>= httpJSON)

fetchRandomJokeByCategory :: String -> IO Text
fetchRandomJokeByCategory cat =
  value . getResponseBody
    <$> (parseRequestThrow (host <> "/random?category=" <> cat) >>= httpJSON)

fetchCategories :: IO Text
fetchCategories =
  mkString . getResponseBody
    <$> (parseRequest (host <> "/categories") >>= httpJSON)
  where
    mkString :: CategoriesResponse -> Text
    mkString (CategoriesResponse cs) = intercalate ", " cs

doAction :: [String] -> IO Text
doAction [] = usage
doAction ["joke"] = fetchRandomJoke
doAction ("joke" : cat : _) = fetchRandomJokeByCategory cat
doAction ("categories" : _) = fetchCategories
doAction _ = fetchRandomJoke

usage :: IO Text
usage =
  return
    "usage\n \
    \\tchuck joke [<category>]   # displays a random joke. lets you choose the <category> optionally\n\
    \\tchuck categories          # displays the available categories\n"

main :: IO ()
main = getArgs >>= doAction >>= TIO.putStrLn
