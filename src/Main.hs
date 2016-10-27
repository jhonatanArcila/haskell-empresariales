{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, ViewPatterns #-}

module Main where


import Db 
import Domain
--import Data.Monoid ((<>))
--import Data.Aeson (FromJSON, ToJSON)

import Web.Scotty
import qualified Database.PostgreSQL.Simple as D

import Control.Monad

import Database.PostgreSQL.Simple.URL
import Control.Monad.IO.Class
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Types.Status
import System.Environment
import Control.Exception 
import Database.PostgreSQL.Simple.Errors
import qualified Network.Wai.Middleware.Cors as C

                    

success :: String
success = "Success"

error' :: String
error' = "Error"



main = do
  
  putStrLn "Starting Server..."
  conn <- D.connectPostgreSQL  "postgres://xlyrtaxdwdozqh:ml6B7YXEWvGLdpcw5ty-BDrcne@ec2-50-19-240-113.compute-1.amazonaws.com:5432/dcku066iig1lq1"
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  scotty port $ do
    middleware C.simpleCors
    
    get "/" $ do
      text ("Bienvenido a un servicio REST construido con Haskell, ingrese a /menus para ver la lista de menus")

    
    get "/menus" $ do
      variable <- liftIO (getAllMenus conn)
      json variable

    
    get "/menus/:id" $ do
      asd <- param "id" :: ActionM Integer
      menu <- liftIO $ getMenuById conn asd
      case menu of
        ([]) ->  json (Resultado {tipo= Just error', mensaje= Just "Menu no encontrado"})
        x -> json menu
      

    post "/menus" $ do
      menu <- (jsonData :: ActionM Menu)
      response <- liftIO $ try $ insertMenu conn menu
      case response of
        Right _ -> json (Resultado {tipo= Just success, mensaje= Just "Menu agregado"}) >> status created201
        Left e -> json (Resultado {tipo= Just error', mensaje= Just (show $ D.sqlErrorMsg e)})



    post "/cliente" $ do
      client <- (jsonData :: ActionM Client)
      response <- liftIO $ try $ insertClient conn client
      case response of
        Right _ -> json (Resultado {tipo= Just success, mensaje= Just "Cliente agregado"}) >> status created201
        Left (constraintViolation -> Just (UniqueViolation _)) -> json (Resultado {tipo= Just error', mensaje= Just "Ya existe un cliente con el mismo username"})>> status badRequest400
        Left e -> json (Resultado {tipo= Just error', mensaje= Just (show $ D.sqlErrorMsg e)})


    get "/cliente" $ do
      variable <- liftIO (getAllClientes conn)
      json variable


    post "/iniciarSesion" $ do
      client <- (jsonData :: ActionM Client)
      pass <- liftIO $ getClient conn client
      case pass of 
          ([]) -> json (Resultado {tipo= Just error', mensaje= Just "Usuario no encontrado"})
          x ->  if (password client) == (password (head pass)) then json (Resultado {tipo= Just success, mensaje= Just "Contraseña correcta"}) else json (Resultado {tipo= Just error', mensaje= Just "Contraseña incorrecta"})
     
    


