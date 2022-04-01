{-# LANGUAGE RecordWildCards #-}

module Cardano.Sdk.Transaction.Data where

import           Cardano.Sdk.Address
import           Cardano.Sdk.Network
import           Cardano.Sdk.UTxO
import qualified Data.Map            as M
import           Data.Maybe
import           Ledger

newtype ChangeAddress = ChangeAddress
  { unChangeAddress :: Address } deriving (Show)

data TxInCandidate = TxInCandidate
  { txInCandidateTxOut :: TxOutput
  , txInCandidateType  :: TxInType
  } deriving Show

newtype TxInCollateral = TxInCollateral
  { txInCollateralRef :: TxOutRef
  } deriving Show

data TxOutput = TxOutput
  { txOutputRef   :: TxOutRef
  , txOutputOut   :: TxOut
  , txOutputDatum :: Maybe Datum
  } deriving Show

data TxOutputCandidate = TxOutputCandidate
  { txOutputCandidateOut   :: TxOut
  , txOutputCandidateDatum :: Maybe Datum
  } deriving Show

data TxBuilder = TxBuilder
  { txBuilderInputs     :: [TxInCandidate]
  , txBuilderOutputs    :: [TxOutputCandidate]
  , txBuilderChangeAddr :: ChangeAddress
  , txBulderCollateral  :: Maybe TxInCollateral
  , txBuilderValidRange :: SlotRange
  , txBuilderSigners    :: [PaymentPubKeyHash]
  } deriving (Show)

txOutputToDatumMap :: TxOutputCandidate -> M.Map DatumHash Datum
txOutputToDatumMap TxOutputCandidate{..} =
  fromMaybe mempty (txOutDatumHash txOutputCandidateOut >>= (\hash -> fmap (M.singleton hash) txOutputCandidateDatum))

collateralFromTxIn :: TxIn -> TxInCollateral
collateralFromTxIn (TxIn ref _) = TxInCollateral ref

txOutputToCandidate :: TxOutput -> TxOutputCandidate
txOutputToCandidate TxOutput{..} = TxOutputCandidate txOutputOut txOutputDatum
