{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics



data Resultado= Resultado{tipo :: Maybe String, mensaje :: Maybe String} deriving (Show,Generic)
instance ToJSON Resultado

-- Datatypes que representan la base de datos

data Menu = Menu { idMenu :: Maybe Int, name :: Maybe String, description :: Maybe String, price :: Maybe Int, restaurant :: Maybe Int } deriving (Show,Generic)
instance ToJSON Menu
instance FromJSON Menu

data Client= Client{username :: Maybe String,
                    nameClient :: Maybe String,
                    lastname :: Maybe String,
                    idClient:: Maybe String,
                    email:: Maybe String,
                    phone:: Maybe String,
                    cellphone:: Maybe String,
                    password:: Maybe String }
                    deriving (Show,Generic)

-- Poder convertir de datatype a JSON
instance ToJSON Client
instance FromJSON Client


-- Para poder convertir de fila de base de datos a datatype y viceversa
instance FromRow Client where
  fromRow = Client <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Client where
  toRow d = [toField (username d),
             toField (nameClient d),
             toField (lastname d),
             toField (idClient d),
             toField (email d),
             toField (phone d),
             toField (cellphone d),
             toField (password d)]



instance FromRow Menu where
  fromRow = Menu <$> field <*> field <*> field <*> field <*> field
  
  
instance ToRow Menu where
  toRow d = [toField (idMenu d), toField (name d), toField (description d), toField (price d), toField (restaurant d)]
