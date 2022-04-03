{-# LANGUAGE RecordWildCards #-}

module Cardano.Sdk.Transaction
  ( buildBalancedTx
  , signTx
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley             (ProtocolParameters (..))
import qualified Cardano.Api.Shelley             as C
import qualified Cardano.Sdk.Network             as Sdk
import           Cardano.Sdk.Transaction.BuildTx
import qualified Cardano.Sdk.Transaction.Data    as Sdk
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Text                       as T
import qualified Ledger                          as L
import qualified Ledger.Ada                      as Ada
import qualified Ledger.Tx.CardanoAPI            as Conv
import qualified Plutus.V1.Ledger.Api            as Api
import           RIO

signTx
  :: TxBody AlonzoEra
  -> [ShelleyWitnessSigningKey]
  -> Tx AlonzoEra
signTx body keys =
  makeSignedTransaction wits body
    where wits = keys <&> makeShelleyKeyWitness body

buildBalancedTx
  :: Sdk.NetworkParameters
  -> Sdk.ChangeAddress
  -> S.Set Sdk.TxInCollateral
  -> Sdk.TxBuilder
  -> Either Sdk.TransactionError (BalancedTxBody AlonzoEra)
buildBalancedTx Sdk.NetworkParameters{..} Sdk.ChangeAddress{..} collateral txb@Sdk.TxBuilder{..}= do
    txBody     <- buildTxBodyContent pparams network collateral txb
    utxo       <- buildInputsUTxO network txBuilderInputs
    let eraInMode    = AlonzoEraInCardanoMode
        witOverrides = Nothing
    changeAddr' <- adaptError $ Conv.toCardanoAddress network changeAddress
    adaptError' $ makeTransactionBodyAutoBalance eraInMode sysstart eraHistory pparams pools utxo txBody changeAddr' witOverrides
  where
    adaptError' (Left err) = Left . Sdk.TxBalancingError $ T.pack . show $ err
    adaptError' (Right tx) = pure tx
