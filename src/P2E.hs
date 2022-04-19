{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module P2E 
    ( mint
    , burn
    , policy
    , policyCode
    , curSymbol
    , tokenSupply
    , simulate
    , tokenName
    , mkPolicyParams
    ) where

import           Control.Monad          hiding (fmap)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     (mintingPolicy, mustMintValue)
import qualified Ledger.Typed.Scripts   as Scripts
import qualified Ledger.Value           as Value
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

type TokenSupply = Integer

tokenName :: TokenName
tokenName = "APEXAVERSE"

tokenSupply :: TokenSupply
tokenSupply = 10_000_000_000

data PolicyParams = PolicyParams
    { tn :: TokenName
    , ts :: TokenSupply
    , pk :: PaymentPubKeyHash
    } deriving Show

PlutusTx.makeLift ''PolicyParams

{-# INLINABLE mkPolicy #-}
mkPolicy :: PolicyParams -> () -> ScriptContext -> Bool
mkPolicy pp () ctx = traceIfFalse "Invalid PubKey" isPKValid &&
                     traceIfFalse "Invalid $AXV amount" isAmountValid where
                        info           = scriptContextTxInfo ctx
                        isPKValid      = txSignedBy info $ unPaymentPubKeyHash (pk pp)
                        isAmountValid  = case Value.flattenValue (txInfoMint info) of
                           [(_, tn', v)] | tn' == (tn pp) && v > 0 -> v == (ts pp) -- mint supply
                           [(_, tn', v)] | tn' == (tn pp) && v < 0 -> True         -- burn
                           _                                       -> False        -- other

mkPolicyParams :: PaymentPubKeyHash -> PolicyParams
mkPolicyParams p = PolicyParams { tn = tokenName, ts = tokenSupply, pk = p }

policyCode :: PolicyParams -> PlutusTx.CompiledCode Scripts.WrappedMintingPolicyType
policyCode pp = $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pp

policy :: PolicyParams -> Scripts.MintingPolicy
policy = mkMintingPolicyScript . policyCode

curSymbol :: PolicyParams -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

mint :: Contract () EmptySchema Text ()
mint = do
    pkh <- Contract.ownPaymentPubKeyHash
    let pp      = mkPolicyParams pkh
        val     = Value.singleton (curSymbol pp) tokenName tokenSupply
        lookups = mintingPolicy $ policy pp
        tx      = mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

burn :: Integer -> Contract () EmptySchema  Text ()
burn amount = do
    pkh <- Contract.ownPaymentPubKeyHash
    let pp      = mkPolicyParams pkh
        val     = Value.singleton (curSymbol pp) tokenName $ negate $ abs amount
        lookups = mintingPolicy $ policy pp
        tx      = mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "burned %s" (show val)

simulate :: IO ()
simulate = runEmulatorTraceIO $ do
    _ <- activateContractWallet (knownWallet 1) mint
    void $ Emulator.waitNSlots 1
