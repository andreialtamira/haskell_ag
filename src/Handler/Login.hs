{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Handler.Auxiliar

formLogin :: Form Usuario
formLogin = renderDivs $ Usuario  
    <$> areq textField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing
    
getAutR :: Handler Html
getAutR = do
    (widget,_) <- generateFormPost formLogin 
    msg <- getMessage
    defaultLayout (formWidget widget msg AutR "Entrar")

postAutR :: Handler Html 
postAutR = do
    ((result,_),_) <- runFormPost formLogin 
    case result of 
        FormSuccess (Usuario email senha) -> do
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                Nothing -> do
                    setMessage [shamlet|
                        Usuario não cadastrado!!!
                    |]
                    redirect AutR
                Just (Entity _ usuario) -> do
                    if senha == usuarioSenha usuario then do
                        setSession "_ID" (usuarioEmail usuario)
                        redirect HomeR
                    else do 
                        setMessage [shamlet|
                            Usuario/Senha nao conferem!!!
                        |]
                    redirect AutR
        _ -> redirect HomeR

postSairR :: Handlere Html
postSairR = do
    deleteSession "_ID"
    redirect HomeR