{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home where

import Import
import Text.Lucius
import Text.Julius
--import Network.HTTP.Types.Status
--import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidgetHead $(luciusFile "templates/home.lucius") 
        $(whamletFile "templates/home.hamlet")
                
getPage3R :: Handler Html
getPage3R = do
    defaultLayout $ do
        $(whamletFile "templates/page3.hamlet")
        toWidgetHead $(luciusFile "templates/home.lucius")
        
        
