{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Text (Text, intercalate)
import Network.HTTP.Simple
import System.Environment (getArgs)

type ErrorType = Text

type AppResult = Either ErrorType Text

data ApiResponse = ApiResponse
  { resultType :: ResultType,
    value :: Value
  }
  deriving (Show)

data ResultType = SuccessfulResult | ErrorResult Text deriving (Show)

instance FromJSON ResultType where
  parseJSON = withText "type" $ \case
    "success" -> return SuccessfulResult
    err -> return $ ErrorResult err

-- as type is a reserved keyword, we need to decode the JSON by hand
instance FromJSON ApiResponse where
  parseJSON =
    withObject "Joke" $ \v -> ApiResponse <$> v .: "type" <*> v .: "value"

fetchRandomJoke :: IO AppResult
fetchRandomJoke =
  httpJSON "https://api.icndb.com/jokes/random"
    <&> getResponseBody
    <&> extractJoke

extractJoke :: ApiResponse -> AppResult
extractJoke ApiResponse {resultType = SuccessfulResult, value = v} = Right $ v ^. key "joke" . _String
extractJoke ApiResponse {resultType = ErrorResult err, value = v} = extractError err v

extractError :: Text -> Value -> AppResult
extractError err v = Left $ err <> ": " <> v ^. _String

fetchRandomJokeByCategory :: String -> IO AppResult
fetchRandomJokeByCategory cat =
  parseRequest ("https://api.icndb.com/jokes/random?limitTo=" <> cat)
    >>= httpJSON
    <&> getResponseBody
    <&> extractJoke

fetchCategories :: IO AppResult
fetchCategories =
  httpJSON "https://api.icndb.com/categories"
    <&> getResponseBody
    <&> extractCategories

extractCategories :: ApiResponse -> AppResult
extractCategories ApiResponse {resultType = SuccessfulResult, value = v} = Right . intercalate ", " $ v ^.. _Array . each . _String
extractCategories ApiResponse {resultType = ErrorResult err, value = v} = extractError err v

main :: IO ()
main = do
  args <- getArgs
  result <- doAction args
  print result

doAction :: [String] -> IO AppResult
doAction [] = fetchRandomJoke
doAction ("categories" : _) = fetchCategories
doAction (cat : _) = fetchRandomJokeByCategory cat
