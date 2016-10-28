{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Utilities where
import Domain
import System.Random

randomString :: Int -> String
randomString gen=take 30 $ randomRs ('a','z') (mkStdGen gen) :: String

tokenGenerator :: Int -> String -> [String] -> String
tokenGenerator gen t tokens=
  if (length listaTokens)>0
    then
      tokenGenerator (gen+1) (randomString gen) tokens
    else t
  where
    listaTokens = filter (\x-> x==t) tokens

matchesId :: Int -> Menu -> Bool
matchesId iD menu = case idMenu menu of
        Nothing -> False
        Just int -> int == iD
