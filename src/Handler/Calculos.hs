{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Calculos where

import Import

-- http://localhost:8080/soma/n1/4/n2/7
-- mostra 11
getSomaR :: Int -> Int -> Handler Html
getSomaR n3 n21 = do
    defaultLayout $ do
        res <- return (n3+n21)
        [whamlet|
            <h1>
                A some é: #{res}
        |]