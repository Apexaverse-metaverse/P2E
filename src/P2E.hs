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


module P2E where

import           Control.Monad          hiding (fmap)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import qualified Ledger.Value           as Value
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

tokenName :: TokenName
tokenName = "APEXAVERSE"

tokenSupply :: Integer
tokenSupply = 10_000_000_000

{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

-- Second arg is a placeholder for endpoint params
type P2ESchema = Endpoint "mint" ()

type P2EParams = ()

mint :: P2EParams -> Contract w MilesSchema Text ()
mint _ = do
    let val     = Value.singleton curSymbol tokenName tokenSupply
        lookups = Constraints.mintingPolicy policy
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () P2ESchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

main :: IO ()
main = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    callEndpoint @"mint" h1 (() :: P2EParams)
    void $ Emulator.waitNSlots 1
