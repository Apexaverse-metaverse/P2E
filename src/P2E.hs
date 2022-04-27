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
    , policy
    , policyCode
    , curSymbol
    , tokenSupply
    , simulate
    , tokenName
    , mkPolicyParams
    , headUtxo
    , submitTxConstraintsWait 
    ) where

import           Control.Monad          hiding (fmap)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Data.Map               (keys)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     (mintingPolicy, mustMintValue)
import           Ledger.Constraints.OffChain (ScriptLookups)
import           Ledger.Constraints.TxConstraints (TxConstraints)
import qualified Ledger.Typed.Scripts   as Scripts
import qualified Ledger.Value           as Value
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

type TokenSupply = Integer

tokenName :: TokenName
tokenName = "AXV"

tokenSupply :: TokenSupply
tokenSupply = 10_000_000_000_000_000

data PolicyParams = PolicyParams
    { tn :: TokenName
    , ts :: TokenSupply
    , tr :: TxOutRef
    } deriving Show

PlutusTx.makeLift ''PolicyParams

{-# INLINABLE mkPolicy #-}
mkPolicy :: PolicyParams -> () -> ScriptContext -> Bool
mkPolicy pp () ctx = traceIfFalse "Minting UTxO is not being consumed" spendsMintingUTxO &&
                     traceIfFalse "Minting an invalid AXV amount" mintsExpectedAmount where
                        info                = scriptContextTxInfo ctx
                        -- because utxos refs are unique, when at least one output
                        -- matches one input we assume that the spent event is also unique
                        spendsMintingUTxO   = any (\i -> txInInfoOutRef i == tr pp) $ txInfoInputs info
                        mintsExpectedAmount = case Value.flattenValue (txInfoMint info) of
                           [(_, tn', v)] | tn' == (tn pp) && v > 0 -> v == (ts pp) -- mint supply
                           _                                       -> False        -- other

mkPolicyParams :: TxOutRef -> PolicyParams
mkPolicyParams oref = PolicyParams { tn = tokenName, ts = tokenSupply, tr = oref }

policyCode :: PolicyParams -> PlutusTx.CompiledCode Scripts.WrappedMintingPolicyType
policyCode pp = $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pp

policy :: PolicyParams -> Scripts.MintingPolicy
policy = mkMintingPolicyScript . policyCode

curSymbol :: PolicyParams -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

anyTokenAmount :: TokenName -> Integer -> Value -> Bool
anyTokenAmount name amount val = any testEntry $ Value.flattenValue val where
    testEntry (_, tName, tAmount) | tName == name = tAmount >= amount
    testEntry _                                   = False

-- returns the first found utxo in the payment address
headUtxo :: PaymentPubKeyHash -> MaybeT (Contract () EmptySchema Text) TxOutRef
headUtxo pkh = MaybeT $ do
    utxos <- Contract.utxosAt $ pubKeyHashAddress pkh Nothing
    case keys utxos of
      []       -> do
                  Contract.logError @String "no utxo found"
                  return Nothing
      oref : _ -> return (Just oref)

submitTxConstraintsWait :: ( PlutusTx.FromData (Scripts.DatumType a), PlutusTx.ToData (Scripts.RedeemerType a), PlutusTx.ToData (Scripts.DatumType a), AsContractError e ) => ScriptLookups a -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a) -> Contract w s e ()
submitTxConstraintsWait l t = submitTxConstraintsWith l t >>= awaitTxConfirmed . getCardanoTxId

mint :: Contract () EmptySchema Text ()
mint = void $ runMaybeT $ do
               -- MaybeT monad
   pkh         <- lift $ Contract.ownPaymentPubKeyHash
   lift $ Contract.logInfo @String (printf "looking tx for %s" $ show pkh)
   oref        <- headUtxo pkh
   lift $ Contract.logInfo @String (printf "found tx %s" $ show oref)
   let pp      = mkPolicyParams oref
       val     = Value.singleton (curSymbol pp) tokenName tokenSupply
       lookups = mintingPolicy $ policy pp
       tx      = mustMintValue val
   lift $      -- Contract monad
               submitTxConstraintsWait @Void lookups tx
           >>  Contract.logInfo @String ( printf "forged %s" $ show val )

simulate :: IO ()
simulate = runEmulatorTraceIO $ do
    void $ activateContractWallet (knownWallet 1) mint
    void $ Emulator.waitNSlots 1
    void $ activateContractWallet (knownWallet 1) mint
    void $ Emulator.waitNSlots 1
