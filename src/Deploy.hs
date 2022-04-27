{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}

module Deploy 
    ( tryReadAddress
    , unsafeTokenNameHex
    , writePolicyFile 
    ) where

import           Debug.Trace                 
import           Control.Exception           (throwIO)
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Short       as SBS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Text                   as Text
import           Data.Maybe                  (fromJust)          
import           Data.String                 (fromString)
import           Ledger.Typed.Scripts        as Plutus.Scripts
import           Cardano.Api                 as API
import           Cardano.Api.Shelley         (Address (..), PlutusScript (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Api        as Plutus
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..))
import           Codec.Serialise             (serialise)
import           P2E                       (tokenName, mkPolicyParams, policy)

credentialLedgerToPlutus :: Ledger.Credential a StandardCrypto -> Plutus.Credential
credentialLedgerToPlutus (ScriptHashObj (ScriptHash h)) = Plutus.ScriptCredential $ Plutus.ValidatorHash $ toBuiltin $ hashToBytes h
credentialLedgerToPlutus (KeyHashObj (KeyHash h))       = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ toBuiltin $ hashToBytes h

tryReadAddress :: String -> Maybe Plutus.Address
tryReadAddress x = case deserialiseAddress AsAddressAny $ Text.pack x of
    Nothing                                      -> Nothing
    Just (AddressByron _)                        -> Nothing
    Just (AddressShelley (ShelleyAddress _ p _)) -> Just Plutus.Address
        { Plutus.addressCredential        = credentialLedgerToPlutus p
          -- We're not using staking address
        , Plutus.addressStakingCredential = Nothing
        }

unsafeTokenNameHex :: String
unsafeTokenNameHex = let getByteString (BuiltinByteString bs) = bs in BS8.unpack
                        $ serialiseToRawBytesHex
                        $ fromJust
                        $ deserialiseFromRawBytes AsAssetName
                        $ getByteString
                        $ Plutus.unTokenName tokenName

writeMintingPolicy :: FilePath -> Plutus.Scripts.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . getMintingPolicy

writePolicyFile :: String -> String -> Integer -> IO ()
writePolicyFile f tId tIdx =     ( writeMintingPolicy f
                                 $ policy
                                 $ Debug.Trace.traceShowId
                                 $ mkPolicyParams (fromString $ tId, tIdx))
                             >>= either (throwIO . userError . show) pure
