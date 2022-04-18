{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores  #-}

module Spec.P2E ( tests ) where

import Test.Tasty
import qualified Plutus.Trace.Emulator as Trace
import Plutus.Contract (Contract)
import Plutus.Contract.Test
import Ledger.Value as Value
import Control.Monad (void)
import Data.Text (Text)
import P2E

t1 :: Trace.ContractInstanceTag
t1 = Trace.walletInstanceTag w1

tests :: TestTree
tests = testGroup "p2e"
    [ checkPredicate "Mint 10_000_000_000 $AXV"
        ( walletFundsChange w1 (
            Value.singleton curSymbol "APEXAVERSE" 10_000_000_000
          )
        )
        $ do
          h1 <- Trace.activateContractWallet w1 endpoints
          Trace.callEndpoint @"mint" h1 ()
    ]
