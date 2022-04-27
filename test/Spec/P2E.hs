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
import Data.Map (toList)
import PlutusTx.Prelude hiding (trace, toList)
import qualified Plutus.Trace.Emulator as Trace
import Plutus.Contract as Contract
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Trace()
import Plutus.Contract.Wallet (getUnspentOutput)
import Ledger
    ( PaymentPubKeyHash
    -- , Script
    , Value
    -- , scriptSize
    , TokenName
    , pubKeyHashAddress 
    )
import Ledger.Constraints as Constraints
import Ledger.Value (singleton, flattenValue)
import Wallet.Emulator.Wallet (mockWalletAddress)
import P2E
    ( mint
    , policy
    , curSymbol
    , tokenSupply
    , tokenName
    , mkPolicyParams
    , submitTxConstraintsWait 
    , mintLookups
    , mintTx
    )

-- assertScriptSize :: Contract () EmptySchema Text () -> Trace.ContractInstanceTag -> Script -> TracePredicate
-- assertScriptSize c t code = (assertDone c t assert msg) where
--     msg = "script too large"
--     assert = const (scriptSize code < 30000)

trace :: Contract () EmptySchema Text ()  -> Trace.EmulatorTrace ()
trace c = void $ do
    _ <- Trace.activateContractWallet w1 c
    _ <- Trace.nextSlot
    void Trace.nextSlot

-- simulates a mint with a policy written for another addr
mintInvalidPK :: PaymentPubKeyHash -> Contract () EmptySchema Text ()
mintInvalidPK givenPk = do
     myPk           <- Contract.ownPaymentPubKeyHash
     oref           <- getUnspentOutput
     gutxos         <- utxosAt (pubKeyHashAddress givenPk Nothing)
     utxos          <- utxosAt (pubKeyHashAddress myPk Nothing)
     let pp         =  mkPolicyParams oref
         gpp        =  mkPolicyParams $ fst $ head $ toList gutxos
         val        =  singleton (curSymbol pp) tokenName tokenSupply
         -- we're trying to cheat the validator, so we'll force
         -- incorrect lookup + tx values
         lookups    =  mintLookups (policy gpp) utxos
         tx'        =  mintTx val oref
     submitTxConstraintsWait @Void lookups tx'

-- simulates a mint with a given amount of tokens
mintVal :: Integer -> Contract () EmptySchema Text ()
mintVal v = do
    oref        <- getUnspentOutput 
    let pp      =  mkPolicyParams oref
        val     =  singleton (curSymbol pp) tokenName v
        lookups =  Constraints.mintingPolicy $ policy pp         
        tx'     =  Constraints.mustMintValue val
    submitTxConstraintsWait @Void lookups tx'

-- simulates minting twice using same wallet
mintTwice :: Contract () EmptySchema Text ()
mintTwice = mint >> mint

hasTokenAmount :: TokenName -> Integer -> Value -> Bool
hasTokenAmount name amount val = all testEntry $ flattenValue val where
    testEntry (_, tName, tAmount) | tName == name = tAmount == amount
    testEntry _                                   = True

tests :: TestTree
tests = let t1    = Trace.walletInstanceTag w1
            addr1 = mockWalletAddress w1
            pkh2  = mockWalletPaymentPubKeyHash w2 
        in testGroup "P2E"
            [ checkPredicate "Mint contract done without errors"
                (assertDone mint t1 (const True) "mint contract not done")
                (trace mint)
            , checkPredicate "Mint 10_000_000_000_000_000 AXV"
                (valueAtAddress addr1 $ hasTokenAmount "AXV" 10_000_000_000_000_000)
                (trace mint)
            , checkPredicate "Not mint when unallowed wallet tries to mint AXV"
                (assertNotDone (mintInvalidPK pkh2) t1 "contract done")
                (trace mint)
            , checkPredicate "Not mint twice the same token"
                (valueAtAddress addr1 $ not . hasTokenAmount "AXV" 20_000_000_000_000_000)
                (trace mintTwice)
            , checkPredicate "Not mint less than 10_000_000_000_000_000 AXV"
                (assertFailedTransaction $ \_ _ _ -> True)
                (trace $ mintVal 9_999_999_999_999_999)
            , checkPredicate "Not mint more than 10_000_000_000_000_000 AXV"
                (assertFailedTransaction $ \_ _ _ -> True)
                (trace $ mintVal 10_000_000_000_000_000_001)
            -- It still not possible to call goldenPir with WrappedMintingPolicyType
            -- , goldenPir "test/Spec/policy.pir" ( policyCode pp1 )
            -- FIXME: add missing pp1
            -- , checkPredicate "Script size is reasonable"
            --      (assertScriptSize mint t1 $ unMintingPolicyScript $ policy pp1)
            --     (trace mint)
            ]
