module Cardano.Sdk.Transaction.Data where

import qualified Cardano.Api       as C
import qualified Data.Map          as M

import           Ledger
import           PlutusTx.Foldable (fold)


newtype UtxO = UtxO { unUtxO :: M.Map TxIn TxOut } deriving (Show, Eq)

instance Semigroup UtxO where
  UtxO a <> UtxO b = UtxO $ a <> b

instance Monoid UtxO where
  mempty = UtxO mempty

class ToLedgerTxOutRef a where
  toLedgerTxOutRef :: a -> TxOutRef

class ToLedgerTxId a where
  toLedgerTxId :: a -> TxId

class ToLedgerTxIn a where
  toLedgerTxIn :: a -> TxIn

class ToLedgerTxOut a where
  toLedgerTxOut :: a -> TxOut

class ToLedgerValue a where
  toLedgerValue :: a -> Value

class ToLedgerUtxO a where
  toLedgerUtxO :: a -> UtxO

instance ToLedgerValue a => ToLedgerValue [a] where
  toLedgerValue xs = fold $ toLedgerValue <$> xs

instance ToLedgerUtxO a => ToLedgerUtxO [a] where
  toLedgerUtxO xs = mconcat $ toLedgerUtxO <$> xs


