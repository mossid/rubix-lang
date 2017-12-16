module Rubix.Examples.Token where

import Rubix.Prelude

init :: IO ()
init = do
    sender <- context <$> getSender
    mint sender (Coin "" 100000)
    deploy transfer

balance = ("b",)

transfer :: Handler (Address, Coin) ()
transfer (to, value) = do
    b <- get (balance to)
    when 
