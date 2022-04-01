{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Sdk.Transaction
  ( buildBalancedTx
  , signTx
  , TransactionError (..)
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley          (ProtocolParameters (..))
import qualified Cardano.Api.Shelley          as C
import qualified Cardano.Sdk.Network          as Sdk
import qualified Cardano.Sdk.Transaction.Data as Sdk
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import qualified Ledger                       as L
import qualified Ledger.Ada                   as Ada
import qualified Ledger.Tx.CardanoAPI         as Conv
import qualified Plutus.V1.Ledger.Api         as Api
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
  -> Either TransactionError (BalancedTxBody AlonzoEra)
buildBalancedTx Sdk.NetworkParameters{..} changeAddr collateral txb@Sdk.TxBuilder{..}= do
    txBody     <- buildTxBodyContent pparams network collateral txb
    utxo       <- buildInputsUTxO network txBuilderInputs
    let eraInMode    = AlonzoEraInCardanoMode
        witOverrides = Nothing
    changeAddr' <- adaptError $ Conv.toCardanoAddress network $ Sdk.unChangeAddress changeAddr
    adaptError' $ makeTransactionBodyAutoBalance eraInMode sysstart eraHistory pparams pools utxo txBody changeAddr' witOverrides
  where
    adaptError' (Left err) = Left . TxBalancingError $ T.pack . show $ err
    adaptError' (Right tx) = pure tx

buildTxBodyContent
  :: ProtocolParameters
  -> NetworkId
  -> S.Set Sdk.TxInCollateral
  -> Sdk.TxBuilder
  -> Either TransactionError (TxBodyContent BuildTx AlonzoEra)
buildTxBodyContent pparams network collateral Sdk.TxBuilder{..} = do
  txIns           <- buildTxIns txBuilderInputs
  txInsCollateral <- buildTxCollateral $ S.elems collateral
  txOuts          <- buildTxOuts network txBuilderOutputs
  txFee           <- adaptError $ Conv.toCardanoFee dummyFee
  txValidityRange <- adaptError $ Conv.toCardanoValidityRange txBuilderValidRange
  wits <- adaptError $ traverse Conv.toCardanoPaymentKeyHash txBuilderSigners
  pure $ TxBodyContent
    { txIns             = txIns
    , txInsCollateral   = txInsCollateral
    , txOuts            = txOuts
    , txFee             = txFee
    , txValidityRange   = txValidityRange
    , txMintValue       = TxMintNone
    , txProtocolParams  = BuildTxWith $ Just pparams
    , txExtraKeyWits    = TxExtraKeyWitnesses ExtraKeyWitnessesInAlonzoEra wits
    , txScriptValidity  = TxScriptValidityNone
    , txMetadata        = TxMetadataNone
    , txAuxScripts      = TxAuxScriptsNone
    , txWithdrawals     = TxWithdrawalsNone
    , txCertificates    = TxCertificatesNone
    , txUpdateProposal  = TxUpdateProposalNone
    }

buildTxOuts
  :: NetworkId
  -> [Sdk.TxOutputCandidate]
  -> Either TransactionError [TxOut CtxTx AlonzoEra]
buildTxOuts network =
    mapM (adaptError . tr)
  where
    tr out@Sdk.TxOutputCandidate{..} =
      Conv.toCardanoTxOut network (lookupDatum $ Sdk.txOutputToDatumMap out) txOutputCandidateOut

buildTxCollateral
  :: [Sdk.TxInCollateral]
  -> Either TransactionError (TxInsCollateral AlonzoEra)
buildTxCollateral collateral = do
  let txIns = flip L.TxIn Nothing . Sdk.txInCollateralRef <$> collateral
  adaptError $ Conv.toCardanoTxInsCollateral (S.fromList txIns)

buildTxIns
  :: [Sdk.TxInCandidate]
  -> Either TransactionError [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))]
buildTxIns =
    mapM (adaptError . tr)
  where
    tr Sdk.TxInCandidate {txInCandidateTxOut=Sdk.TxOutput{..}, ..} = do
      wit <- Conv.toCardanoTxInWitness txInCandidateType
      txIn <- Conv.toCardanoTxIn txOutputRef
      pure (txIn, BuildTxWith wit)

buildInputsUTxO
  :: NetworkId
  -> [Sdk.TxInCandidate]
  -> Either TransactionError (UTxO AlonzoEra)
buildInputsUTxO network inputs = do
    mapM (adaptError . tr) inputs <&> UTxO . M.fromList
  where
    tr :: Sdk.TxInCandidate ->  Either Conv.ToCardanoError (TxIn, TxOut CtxUTxO AlonzoEra)
    tr Sdk.TxInCandidate{txInCandidateTxOut=out@Sdk.TxOutput{..}} = do
      txIn  <- Conv.toCardanoTxIn txOutputRef
      let dm = Sdk.txOutputToDatumMap $ Sdk.txOutputToCandidate out
      txOut <- Conv.toCardanoTxOut network (lookupDatum dm) txOutputOut
      pure (txIn, toCtxUTxOTxOut txOut)

lookupDatum :: Map L.DatumHash L.Datum -> Maybe L.DatumHash -> Either Conv.ToCardanoError (TxOutDatum CtxTx AlonzoEra)
lookupDatum datums datumHash =
    case flip M.lookup datums =<< datumHash of
        Just datum -> pure $ TxOutDatum ScriptDataInAlonzoEra (toCardanoScriptData $ L.getDatum datum)
        Nothing    -> Conv.toCardanoTxOutDatumHash datumHash

toCardanoScriptData :: Api.BuiltinData -> ScriptData
toCardanoScriptData = C.fromPlutusData . Api.builtinDataToData

dummyFee :: L.Value
dummyFee = Ada.lovelaceValueOf 0

data TransactionError
  = TxCardanoError Conv.ToCardanoError
  | TxBalancingError T.Text
  | TxOtherError T.Text
  deriving (Show, Exception)

adaptError :: Either Conv.ToCardanoError b -> Either TransactionError b
adaptError = mapLeft TxCardanoError

