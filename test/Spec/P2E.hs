{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores  #-}

module Spec.P2E ( tests ) where

import Test.Tasty
import qualified Plutus.Trace.Emulator as Trace
import Plutus.Contract.Test
import qualified Ledger as Ledger
import Ledger.Value as Value
import Control.Monad (void)
import P2E (mint, policy, policyCode, curSymbol)

t1 :: Trace.ContractInstanceTag
t1 = Trace.walletInstanceTag w1

traceMint :: Trace.EmulatorTrace ()
traceMint = void $ do
    _ <- Trace.activateContractWallet w1 (void mint)
    _ <- Trace.nextSlot
    void Trace.nextSlot

tests :: TestTree
tests = testGroup "P2E"
    [ checkPredicate "Mint contract done without errors"
        (assertDone mint t1 (const True) "mint contract not done")
        traceMint
    , checkPredicate "Mint 10_000_000_000 $AXV"
        (walletFundsChange w1 $ Value.singleton curSymbol "APEXAVERSE" 10_000_000_000
        )
        traceMint
    , goldenPir "test/Spec/policy.pir" policyCode
    , checkPredicate "Script size is reasonable"
        (assertDone mint t1 ((30000 >=) . Ledger.scriptSize . Ledger.unMintingPolicyScript . const policy) "script too large")
        traceMint
    ]
