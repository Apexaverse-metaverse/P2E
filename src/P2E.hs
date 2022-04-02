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
    , writePolicyFile
    , unsafeTokenNameHex
    , tokenName
    , mkPolicyParams
    ) where

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

type TokenSupply = Integer

tokenName :: TokenName
tokenName = "APEXAVERSE"

tokenSupply :: TokenSupply
tokenSupply = 10_000_000_000

data PolicyParams = PolicyParams
    { tn :: TokenName
    , ts :: TokenSupply
    , pk :: PaymentPubKeyHash
    }

PlutusTx.makeLift ''PolicyParams

{-# INLINABLE mkPolicy #-}
mkPolicy :: PolicyParams -> () -> ScriptContext -> Bool
mkPolicy pp () ctx = isPKValid && isAmountValid where
    info           = scriptContextTxInfo ctx
    isPKValid      = txSignedBy info $ unPaymentPubKeyHash (pk pp)
    isAmountValid  = case Value.flattenValue (txInfoMint info) of
        [(_, tn', amt)] ->    tn' == (tn pp)
                           && amt == (ts pp)
        _               -> False

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

unsafeTokenNameHex :: String
unsafeTokenNameHex = let getByteString (BuiltinByteString bs) = bs in BS8.unpack
                        $ serialiseToRawBytesHex
                        $ fromJust
                        $ deserialiseFromRawBytes AsAssetName
                        $ getByteString
                        $ Value.unTokenName tokenName

writeMintingPolicy :: FilePath -> Scripts.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . getMintingPolicy

writePolicyFile :: PaymentPubKeyHash -> IO ()
writePolicyFile pkh = do
    let file  = "policy.plutus"
        pp      = mkPolicyParams pkh
    e <- writeMintingPolicy file $ policy pp
    case e of
      Left err -> throwIO $ userError $ show err
      Right () -> return ()

simulate :: IO ()
simulate = runEmulatorTraceIO $ do
    _ <- activateContractWallet (knownWallet 1) mint
    void $ Emulator.waitNSlots 1
