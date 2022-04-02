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

module P2E (mint, policy, policyCode, curSymbol, tokenSupply, simulate, writePolicyFile, unsafeTokenNameHex, tokenName) where

import           Control.Monad          hiding (fmap)
import           Control.Exception      (throwIO)
import           Data.Text              (Text)
import           Data.Void              (Void)
import qualified Data.ByteString.Char8  as BS8
import qualified Data.ByteString.Short  as SBS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Maybe             (fromJust)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..))
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     (mintingPolicy, mustMintValue)
import qualified Ledger.Typed.Scripts   as Scripts
import qualified Ledger.Value           as Value
import           Prelude                (IO, Show (..), String,  userError, FilePath)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           Cardano.Api            (PlutusScriptV1, FileError, writeFileTextEnvelope, serialiseToRawBytesHex, deserialiseFromRawBytes, AsType (..))
import           Cardano.Api.Shelley    (PlutusScript (..))
import           Codec.Serialise        (serialise)

tokenName :: TokenName
tokenName = "APEXAVERSE"

unsafeTokenNameHex :: String
unsafeTokenNameHex = let getByteString (BuiltinByteString bs) = bs in BS8.unpack
                        $ serialiseToRawBytesHex
                        $ fromJust
                        $ deserialiseFromRawBytes AsAssetName
                        $ getByteString
                        $ Value.unTokenName tokenName

tokenSupply :: Integer
tokenSupply = 10_000_000_000

{-# INLINABLE mkPolicy #-}
mkPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) $ unPaymentPubKeyHash pkh

policyCode :: PaymentPubKeyHash -> PlutusTx.CompiledCode Scripts.WrappedMintingPolicyType
policyCode pkh = $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy = mkMintingPolicyScript . policyCode

curSymbol :: PaymentPubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

mint :: Contract () EmptySchema Text ()
mint = do
    pkh <- Contract.ownPaymentPubKeyHash
    let val     = Value.singleton (curSymbol pkh) tokenName tokenSupply
        lookups = mintingPolicy $ policy pkh
        tx      = mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

writeMintingPolicy :: FilePath -> Scripts.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . getMintingPolicy

writePolicyFile :: PaymentPubKeyHash -> IO ()
writePolicyFile pkh = do
    let file  = "policy.plutus"
    e <- writeMintingPolicy file $ policy pkh
    case e of
      Left err -> throwIO $ userError $ show err
      Right () -> return ()

simulate :: IO ()
simulate = runEmulatorTraceIO $ do
    _ <- activateContractWallet (knownWallet 1) mint
    void $ Emulator.waitNSlots 1
