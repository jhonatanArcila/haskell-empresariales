{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db where

import Domain


import qualified Database.PostgreSQL.Simple as D


getAllMenus :: D.Connection -> IO [Menu]
getAllMenus c = do
  list <- (D.query_ c "select * from menu" :: IO [Menu])
  return list


getMenuById :: D.Connection -> Integer -> IO [Menu]
getMenuById conn int = do
    menu <- (D.query conn "select * from menu where id = ?" (D.Only int) :: IO [Menu])
    return menu

insertMenu conn menu = do
    result <- D.execute conn "insert into menu (name,description,price,restaurant) values (?,?,?,?)" ((name menu), (description menu), (price menu),(restaurant menu))
    return result


insertClient conn client = do
    result <- D.execute conn "insert into client (username,name,lastname,id,email,password,phone,cellphone) values (?,?,?,?,?,?,?,?)" ((username client),(nameClient client),(lastname client),(idClient client),(email client),(password client),(phone client),(cellphone client))	
    return result


getAllClientes :: D.Connection -> IO [Client]
getAllClientes c = do
  list <- (D.query_ c "select * from client" :: IO [Client])
  return list

getClient :: D.Connection -> Client -> IO [Client]
getClient conn client = do
    result <- (D.query conn "select * from client where username = ? " (D.Only (username client)) :: IO [Client])
    return result

matchesId :: Int -> Menu -> Bool
matchesId id menu = case idMenu menu of
        Nothing -> False
        Just int -> int == id