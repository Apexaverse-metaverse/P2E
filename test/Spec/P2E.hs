{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores  #-}

module Spec.P2E ( tests ) where

import Test.Tasty
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import qualified Plutus.Trace.Emulator as Trace
import Plutus.Contract as Contract
import Plutus.Contract.Test
import Plutus.Contract.Trace()
import Ledger (PaymentPubKeyHash, Script, unMintingPolicyScript, scriptSize, getCardanoTxId)
import Ledger.Constraints as Constraints
import Ledger.Value (singleton)
import P2E (mint, policy, curSymbol, tokenSupply, tokenName)

t1 :: Trace.ContractInstanceTag
t1 = Trace.walletInstanceTag w1

pkh1 :: PaymentPubKeyHash
pkh1 = mockWalletPaymentPubKeyHash w1

pkh2 :: PaymentPubKeyHash
pkh2 = mockWalletPaymentPubKeyHash w2 

assertScriptSize :: Monoid w => Contract w s e a -> Trace.ContractInstanceTag -> Script -> TracePredicate
assertScriptSize c t code = (assertDone c t assert msg) where
    msg = "script too large"
    assert = const (scriptSize code < 30000)

traceMint :: Trace.EmulatorTrace ()
traceMint = void $ do
    _ <- Trace.activateContractWallet w1 (void mint)
    _ <- Trace.nextSlot
    void Trace.nextSlot


-- simulates a mint with a policy written for another addr
mintInvalidPK :: PaymentPubKeyHash -> Contract () EmptySchema Text ()
mintInvalidPK givenPk = do
    myPk <- Contract.ownPaymentPubKeyHash
    let val     = singleton (curSymbol myPk) tokenName tokenSupply
        lookups = Constraints.mintingPolicy $ policy givenPk         
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

tests :: TestTree
tests = testGroup "P2E"
    [ checkPredicate "Mint contract done without errors"
        (assertDone mint t1 (const True) "mint contract not done")
        traceMint
    , checkPredicate "Mint 10_000_000_000 $AXV"
        (walletFundsChange w1 $ singleton ( curSymbol pkh1 ) "APEXVERSE" 10_000_000_000)
        traceMint
    , checkPredicate "Not mint when unallowed wallet tries to mint $AXV"
        -- errorMintingWrongWallet
        (assertNotDone (mintInvalidPK pkh2) t1 "contract done")
        traceMint
    -- It still not possible to call goldenPir with WrappedMintingPolicyType
    -- , goldenPir "test/Spec/policy.pir" ( policyCode pkh1 )
    , checkPredicate "Script size is reasonable"
         (assertScriptSize mint t1 $ unMintingPolicyScript $ policy pkh1)
        traceMint
    ]
