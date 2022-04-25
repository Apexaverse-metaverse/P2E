{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores  #-}

module Spec.P2E ( tests )  where

import Test.Tasty
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import PlutusTx.Prelude hiding (trace)
import qualified Plutus.Trace.Emulator as Trace
import Plutus.Contract as Contract
import Plutus.Contract.Test
import Plutus.Contract.Trace()
import Ledger (PaymentPubKeyHash, Script, unMintingPolicyScript, scriptSize, getCardanoTxId)
import Ledger.Constraints as Constraints
import Ledger.Value (singleton)
import P2E (mint, burn, policy, curSymbol, tokenSupply, tokenName, mkPolicyParams)

assertScriptSize :: Contract () EmptySchema Text () -> Trace.ContractInstanceTag -> Script -> TracePredicate
assertScriptSize c t code = (assertDone c t assert msg) where
    msg = "script too large"
    assert = const (scriptSize code < 30000)

trace :: Contract () EmptySchema Text ()  -> Trace.EmulatorTrace ()
trace c = void $ do
    _ <- Trace.activateContractWallet w1 c
    _ <- Trace.nextSlot
    void Trace.nextSlot

-- simulates a mint with a policy written for another addr
mintInvalidPK :: PaymentPubKeyHash -> Contract () EmptySchema Text ()
mintInvalidPK givenPk = do
    myPk <- Contract.ownPaymentPubKeyHash
    let pp      = mkPolicyParams myPk
        gpp     = mkPolicyParams givenPk
        val     = singleton (curSymbol pp) tokenName tokenSupply
        lookups = Constraints.mintingPolicy $ policy gpp
        tx'     = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx'
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

-- simulates a mint with a given amount of tokens
mintVal :: Integer -> Contract () EmptySchema Text ()
mintVal v = do
    pk <- Contract.ownPaymentPubKeyHash
    let pp      = mkPolicyParams pk
        val     = singleton (curSymbol pp) tokenName v
        lookups = Constraints.mintingPolicy $ policy pp         
        tx'     = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx'
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

-- simulates a mint of supply then a burn of amount tokens
burnVal :: Integer -> Contract () EmptySchema Text ()
burnVal v = mint *> burn v

tests :: TestTree
tests = let t1 = Trace.walletInstanceTag w1
            pkh1 = mockWalletPaymentPubKeyHash w1
            pkh2 = mockWalletPaymentPubKeyHash w2 
            pp1 = mkPolicyParams pkh1
        in testGroup "P2E"
            [ checkPredicate "Mint contract done without errors"
                (assertDone mint t1 (const True) "mint contract not done")
                (trace mint)
            , checkPredicate "Mint 10_000_000_000 AXV"
                (walletFundsChange w1 $ singleton (curSymbol pp1) "AXV" 10_000_000_000)
                (trace mint)
            , checkPredicate "Not mint when unallowed wallet tries to mint AXV"
                (assertNotDone (mintInvalidPK pkh2) t1 "contract done")
                (trace mint)
            , checkPredicate "Not mint less than 10_000_000_000 AXV"
                (assertFailedTransaction $ \_ _ _ -> True)
                (trace $ mintVal 9_999_999_999)
            , checkPredicate "Not mint more than 10_000_000_000 AXV"
                (assertFailedTransaction $ \_ _ _ -> True)
                (trace $ mintVal 10_000_000_001)
            , checkPredicate "Burn a given amount of AXV"
                (walletFundsChange w1 $ singleton (curSymbol pp1) "AXV" 9_999_999_999)
                (trace $ burnVal 1)
            -- It still not possible to call goldenPir with WrappedMintingPolicyType
            -- , goldenPir "test/Spec/policy.pir" ( policyCode pp1 )
            , checkPredicate "Script size is reasonable"
                 (assertScriptSize mint t1 $ unMintingPolicyScript $ policy pp1)
                (trace mint)
            ]
