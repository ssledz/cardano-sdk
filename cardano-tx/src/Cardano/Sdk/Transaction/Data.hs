{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Sdk.Transaction.Data where

import           Cardano.Sdk.Address
import           Cardano.Sdk.Network
import           Cardano.Sdk.UTxO
import           Control.Exception
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Ledger
import qualified Ledger.Tx.CardanoAPI as Conv


newtype ChangeAddress = ChangeAddress
  { changeAddress :: Address } deriving (Show)

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

data TxBalancing = TxBalancing
  { txBalancingInputs     :: [TxInCandidate]
  , txBalancingChangeAddr :: ChangeAddress
  , txBalancingCollateral :: S.Set TxInCollateral
  }

data TxBuilder = TxBuilder
  { txBuilderInputs     :: [TxInCandidate]
  , txBuilderOutputs    :: [TxOutputCandidate]
  , txBuilderChangeAddr :: ChangeAddress -- to remove
  , txBuilderValidRange :: SlotRange
  , txBuilderSigners    :: [PaymentPubKeyHash]
  } deriving (Show)

data TransactionError
  = TxCardanoError Conv.ToCardanoError
  | TxBalancingError T.Text
  | TxInsufficentFundsError
  | TxOtherError T.Text
  deriving (Show, Exception)

txInCandidateValue :: TxInCandidate -> Value
txInCandidateValue = txOutValue . txOutputOut . txInCandidateTxOut

txOutputCandidateValue :: TxOutputCandidate -> Value
txOutputCandidateValue = txOutValue . txOutputCandidateOut

txOutputToDatumMap :: TxOutputCandidate -> M.Map DatumHash Datum
txOutputToDatumMap TxOutputCandidate{..} =
  fromMaybe mempty (txOutDatumHash txOutputCandidateOut >>= (\hash -> fmap (M.singleton hash) txOutputCandidateDatum))

collateralFromTxIn :: TxIn -> TxInCollateral
collateralFromTxIn (TxIn ref _) = TxInCollateral ref

txOutputToCandidate :: TxOutput -> TxOutputCandidate
txOutputToCandidate TxOutput{..} = TxOutputCandidate txOutputOut txOutputDatum
