module Cardano.Sdk.Address where

import           Data.Either.Extra
import           Data.Text

import qualified Cardano.Api          as C
import qualified Ledger               as P
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Tx.CardanoAPI as Interop

renderValidatorToShellyAddress :: C.NetworkId -> Scripts.Validator -> Text
renderValidatorToShellyAddress network validatorInstance =
    (C.serialiseAddress . C.makeShelleyAddress network paymentCredential) C.NoStakeAddress
  where
    validatorScript   = Scripts.unValidatorScript validatorInstance
    hashScript        = C.hashScript $ Scripts.toCardanoApiScript validatorScript
    paymentCredential = C.PaymentCredentialByScript hashScript

readShellyAddress :: Text -> Maybe P.Address
readShellyAddress text = do
  saddr <- C.deserialiseAddress (C.AsAddress C.AsShelleyAddr) text
  eitherToMaybe $ Interop.fromCardanoAddress (C.shelleyAddressInEra saddr :: C.AddressInEra C.AlonzoEra)

renderShellyAddress :: C.NetworkId -> P.Address -> Maybe Text
renderShellyAddress network addr = do
  addr' <- eitherToMaybe $ Interop.toCardanoAddress network addr
  return $ C.serialiseAddress addr'

readAnyAddress :: Text -> Maybe C.AddressAny
readAnyAddress addr = C.toAddressAny <$> C.deserialiseAddress C.AsShelleyAddress addr

convToAddressAny :: C.NetworkId -> P.Address -> Maybe C.AddressAny
convToAddressAny network addr = do
  addr' <- renderShellyAddress network addr
  readAnyAddress addr'

