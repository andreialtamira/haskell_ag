{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Cliente where

import Import

formCliente :: Form Cliente
formCliente = renderDivs $ Cliente
    <$> areq textField "Nome: " Nothing
    <*> areq textField "CPF: " Nothing
    <*> areq intField "Idade: " Nothing
    <*> areq textField "Endereço: " Nothing

getClienteR :: Handler Html
getClienteR = do
    (widget,_) <- generateFormPost formCliente
    msg <- getMessage
    defaultLayout $ do
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}
            <form method=post action=@{ClienteR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postClienteR :: Handler Html
postClienteR = do
    ((result,_),_) <- runFormPost formCliente
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
