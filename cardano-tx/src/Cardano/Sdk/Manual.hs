module Cardano.Sdk.Manual where

import qualified Cardano.Api                         as C
import           Cardano.Sdk.Adapter.Koios.UTxO      as K
import           Cardano.Sdk.Adapter.Node.Connection
import           Cardano.Sdk.Adapter.Node.UTxO       as N
import           Cardano.Sdk.Address
import           Cardano.Sdk.UTxO
import           Control.Monad
import qualified Data.Map                            as M
import           RIO

main :: IO ()
main = do
  let networkId = C.Testnet $ C.NetworkMagic 1097911063
  let conn = connInfo networkId "/home/ssledz/git/simple-swap-playground/testnet/node.socket"
  let koiosConfig = KoiosConfig "https://testnet.koios.rest" networkId
  let addr = "addr_test1qp72kluzgdnl8h5cazhctxv773zrq7dzq8y50q2vr9w2v2laj7qf05z8tpyhc0k5kkks3083uthryl3leeufkfz6j0pq03n8ck"
  addr' <- maybe (throwString "address parsing error") return (readShellyAddress addr)
  addrInfo <- K.queryAddressInfo koiosConfig addr'
  let value1 = toLedgerValue addrInfo
  utxos <- N.queryUTxo conn $ maybeToList (readAnyAddress addr)
  let value2 = toLedgerValue utxos
  --print value1
  --print value2
  print $ value1 == value2
  let utxos1 = toLedgerUTxO addrInfo
  let utxos2 = toLedgerUTxO utxos
  print $ utxos1 == utxos2
  let keys1 = M.keys . unUTxO $ utxos1
  let keys2 = M.keys . unUTxO $ utxos2
  print $ keys1 == keys2
  --forM_ keys1 print
  --print "------------"
  --forM_ keys2 print
  --print addrInfo

