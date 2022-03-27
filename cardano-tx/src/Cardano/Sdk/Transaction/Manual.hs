module Cardano.Sdk.Transaction.Manual where

import qualified Cardano.Api                           as C
import           Cardano.Sdk.Address
import           Cardano.Sdk.Transaction.Adapter.Koios
import           Cardano.Sdk.Transaction.Adapter.Node
import           Cardano.Sdk.Transaction.Data
import           RIO

main :: IO ()
main = do
  let networkId = C.Testnet $ C.NetworkMagic 1097911063
  let conn = connInfo networkId "/home/ssledz/git/simple-swap-playground/testnet/node.socket"
  let koiosConfig = KoiosConfig "https://testnet.koios.rest"
  let addr = "addr_test1qp72kluzgdnl8h5cazhctxv773zrq7dzq8y50q2vr9w2v2laj7qf05z8tpyhc0k5kkks3083uthryl3leeufkfz6j0pq03n8ck"
  addrInfo <- queryAddressInfo koiosConfig addr
  print addrInfo
  print $ toLedgerValue addrInfo
  utxos <- queryUtxo conn $ maybeToList (parseAsAnyAddress addr)
  print utxos
