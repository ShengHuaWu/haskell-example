{-# LANGUAGE OverloadedStrings #-} -- This makes so that our URL string is automaticly parsed into a Request, which simplifies our HTTP request.

module Main where

-- Allow us to make HTTP requests and work with bytestrings.
import Network.HTTP.Simple (parseRequest, httpBS, getResponseBody)
import qualified Data.ByteString.Char8 as BS

-- Extract the rate of USD
import Control.Lens (preview)
import Data.Aeson.Lens (key, values, _String)
import Data.Aeson.Types (Value)
import Data.Text (Text)
import Data.Vector (Vector)

-- We need to use putStrLn from Data.Text.IO because rate is of type Text and putStrLn from Prelude only works with String.
import qualified Data.Text.IO as TIO

fetchJSON :: IO BS.ByteString
fetchJSON = do
  res <- httpBS "https://api.coindesk.com/v1/bpi/currentprice.json"
  return (getResponseBody res)

getRate :: BS.ByteString -> Maybe Text
getRate = preview (key "bpi" . key "USD" . key "rate" . _String)

fetchPlanet :: Int -> IO BS.ByteString
fetchPlanet n = do 
  req <- parseRequest ("https://swapi.co/api/planets/" <> show n) -- Create a request from string
  res <- httpBS req
  return (getResponseBody res)

-- TODO: Parse films to an array of string
getFilms :: BS.ByteString -> Maybe Text
getFilms = preview (key "films" . values . _String)

main :: IO ()
main = do
  -- json <- fetchJSON
  -- case (getRate json) of
  --   Nothing -> TIO.putStrLn "Could not find the Bitcoin rate :("
  --   Just rate -> TIO.putStrLn $ "The current Bitcoin rate is " <> rate <> " $"
  p <- fetchPlanet 1
  case (getFilms p) of
    Nothing -> TIO.putStrLn "Could not find the films of planet 1 :("
    Just films -> TIO.putStrLn films