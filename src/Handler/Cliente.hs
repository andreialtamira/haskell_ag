{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Cliente where

import Import
import Handler.Auxiliar

formCliente :: Maybe Cliente -> Form Cliente
formCliente mc = renderDivs $ Cliente
    <$> areq textField "Nome: " (fmap clienteNome mc)
    <*> areq textField "CPF: " (fmap clienteCpf mc)
    <*> areq intField "Idade: " (fmap clienteIdade mc)
    <*> areq textField "Endereço: " (fmap clienteEndereco mc)

getClienteR :: Handler Html
getClienteR = do
    (widget,_) <- generateFormPost (formCliente Nothing)
    msg <- getMessage
    defaultLayout $ (formWidget widget msg ClienteR "Cadastrar")

postClienteR :: Handler Html
postClienteR = do
    ((result,_),_) <- runFormPost (formCliente Nothing)
    case result of
        FormSuccess cliente -> do
            runDB $ insert cliente
            setMessage [shamlet|
                <div>
                    CLIENTE INSERIDO COM SUCESSO!!!
            |]
            redirect ClienteR
        _ -> redirect HomeR

getPerfilR :: ClienteId -> Handler Html
getPerfilR cid = do
    cliente <- runDB $ get404 cid
    defaultLayout [whamlet|
        <h1> 
            Perfil de: #{clienteNome cliente}
        <h2>
            CPF: #{clienteCpf cliente}
        <h2>
            Idade: #{clienteIdade cliente}
        <h2>
            Endereco: #{clienteEndereco cliente}
    |]

getListaCliR :: Handler Html
getListaCliR = do
    clientes <- runDB $ selectList [] [Asc ClienteNome]
    defaultLayout $(whamletFile "templates/clientes.hamlet")

postApagarCliR :: ClienteId -> Handler Html
postApagarCliR cid = do
    runDB $ delete cid
    redirect ListaCliR

getEditarCliR :: ClienteId -> Handler Html
getEditarCliR cid = do
        cliente <- runDB  $ get404 cid
        (widget,_) <- generateFormPost (formCliente (Just cliente))
        msg <- getMessage
        defaultLayout $ (formWidget widget msg (EditarCliR cid) "Editar") 

postEditarCliR :: ClienteId -> Handler Html 
postEditarCliR cid = do
    _ <- runDB $ get404 cid
    ((result,_),_) <- runFormPost (formCliente Nothing)
    case result of  
        FormSuccess novoCliente -> do
            runDB $ replace cid novoCliente
            redirect ListaCliR
        _ -> redirect HomeR

