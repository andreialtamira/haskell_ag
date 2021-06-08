{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Venda where

import Import
import Database.Persist.Postgresql

formVenda :: ClienteId -> Form Venda
formVenda cid = renderDivs $ Venda
    <$> pure cid 
    <*> areq (selectField prodCB) "Produto: " Nothing
    <*> lift (liftIO (map utctDay getCurrentTime))
    <*> areq intField "Quantidade: " Nothing
--    <*> areq doubleField "Valor: " Nothing
--    <*> areq boolField "Pago: " Nothing
--    <*> areq dayField "Data Pagamento: " Nothing
--    <*> areq boolField "Entrega realizada: " Nothing
--    <*> areq dayField "Data entrega: " Nothing

prodCB :: Handler (OptionList (Key Produto))
prodCB = do 
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    optionsPairs $ 
        map (\r -> (produtoNome $ entityVal r, entityKey r)) row

getVendaR :: ClienteId -> Handler Html
getVendaR cid = do
    (widget,_) <- generateFormPost (formVenda cid)
    msg <- getMessage 
    defaultLayout $
        [whamlet|
            $maybe mensa <- msg 
                <div>
                    ^{mensa}
            <h1>
                COMPRAS
            <form method=post action=@{VendaR cid}>
                ^{widget}
                <input type="submit" value="Comprar">
        |] 

postVendaR :: ClienteId -> Handler Html
postVendaR cid = do
    ((result,_),_) <- runFormPost (formVenda cid)
    case result of 
        FormSuccess venda -> do
            runDB $ insert venda
            setMessage [shamlet|
                <div>
                    COMPRA EFETUADA COM SUCESSO!!!
            |]
            redirect (CarrinhoR cid)
        _ -> redirect HomeR

mult :: Double -> Double -> Double
mult = (*)

getCarrinhoR :: ClienteId -> Handler Html
getCarrinhoR cid = do 
    let sql = "SELECT ??,??,?? FROM produto \
        \ INNER JOIN venda ON venda.produtoid = produto.id \
        \ INNER JOIN cliente ON venda.clienteid = cliente.id \
        \ WHERE cliente.id = ?"
    cliente <- runDB $ get 404 cid
    tudo <- runDB $ rawSql sql [toPersistValue cid] :: Handler [(Entity Produto, Entity Venda, Entity Cliente)]
    defaultLayout $ do
        [whamlet|
            <h1>
                CARRINHO DO #{clienteNome cliente}
            <ul>
                $forall (Entity _ produto, Entity _ venda, Entity _ _) <- tudo
                <li>
                    #{produtoNome produto}: #{mult (produtoPreco produto) (fromIntegral (vendaQuantitade venda))} no dia #{show $ vendaDia venda}
        |]