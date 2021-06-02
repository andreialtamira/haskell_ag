{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Venda where

import Import

formVenda :: Form Venda
formVenda = renderDivs $ Venda
    <$> areq textField "Cliente: " Nothing 
    <*> areq textField "Produto: " Nothing
    <*> areq dayField "Data: " Nothing 
    <*> areq doubleField "Valor: " Nothing
    <*> areq intField "Quantidade: " Nothing
    <*> areq boolField "Pago: " Nothing
    <*> areq dayField "Data Pagamento: " Nothing
    <*> areq boolField "Entrega realizada: " Nothing
    <*> areq dayField "Data entrega: " Nothing

getVendaR :: Handler Html
getVendaR = undefined 

postVendaR :: Handler Html
postVendaR = undefined