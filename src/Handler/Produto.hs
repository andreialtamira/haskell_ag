{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Produto where

import Import

formProduto :: Form Produto
formProduto = renderDivs $ Produto
    <$> areq textField "nome: " Nothing
    <*> areq textField "tipo: " Nothing
    <*> areq doubleField "valor: " Nothing
    <*> areq intField "quantidade: " Nothing

getProdutoR :: Handler Html
getProdutoR = do
    (widget,_) <- generateFormPost formProduto
    msg <- getMessage
    defaultLayout $ do
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}
            <form method=post action=@{ProdutoR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postProdutoR :: Handler Html 
postProdutoR = do
    ((result,_),_) <- runFormPost formProduto
    case result of
        FormSuccess produto -> do
            runDB $ insert produto
            setMessage [shamlet|
                <div>
                    INSERIDO COM SUCESSO!!!
            |]
            redirect ProdutoR
        _ -> redirect HomeR

getTipoR :: ProdutoId -> Handler Html
getTipoR pid = do
    produto <- runDB $ get404 pid
    defaultLayout [whamlet|
        <h1> 
            Nome: #{produtoNome produto}
        <h2>
            Descricao: #{produtoDescricao produto}
        <h2>
            Valor: #{produtoValor produto}
    |]

getListaProR :: Handler Html
getListaProR = do 
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    defaultLayout $(whamletFile "templates/produtos.hamlet")

postApagarProR :: ProdutoId -> Handler Html 
postApagarProR pid = do
    runDB $ delete pid
    redirect ListaProR 