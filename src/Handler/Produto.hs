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
    <*> areq textField "descrição: " Nothing
    <*> areq textField "tipo: " Nothing
    <*> areq doubleField "valor: " Nothing
    <*> areq intField "quantidade: " Nothing