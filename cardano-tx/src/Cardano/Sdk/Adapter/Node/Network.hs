{-# LANGUAGE RecordWildCards #-}

module Cardano.Sdk.Adapter.Node.Network
  ( queryNetworkParams
  , queryProtocolParams
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley             (PoolId, ProtocolParameters)
import           Cardano.Sdk.Adapter.Node.Shared
import           Cardano.Sdk.Network
import           Cardano.Slotting.Time           (SystemStart)
import qualified Data.Set                        as Set
import           RIO


queryProtocolParams :: MonadIO m => NodeConn -> m ProtocolParameters
queryProtocolParams conn = do
    cp <- queryChainPoint conn
    let query = QueryInEra AlonzoEraInCardanoMode $
                  QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    paramsOrErr <- liftIO $ queryNodeLocalState conn (Just cp) query
    case paramsOrErr of
        Right (Right p)  -> return p
        Left err         -> throwString $ show err
        Right (Left err) -> throwString $ show err


liftEither :: (MonadIO m, Show e) => Either e a -> m a
liftEither (Right a)  = return a
liftEither (Left err) = throwString $ show err

querySystemStart :: MonadIO m => NodeConn -> m SystemStart
querySystemStart conn = do
  cp <- queryChainPoint conn
  resOrErr <- liftIO $ queryNodeLocalState conn (Just cp) QuerySystemStart
  liftEither resOrErr

queryPools :: MonadIO m => NodeConn -> m (Set.Set PoolId)
queryPools conn = do
  cp <- queryChainPoint conn
  let query = QueryInEra AlonzoEraInCardanoMode $
                QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryStakePools
  resOrErr <- liftIO $ queryNodeLocalState conn (Just cp) query
  case resOrErr of
    Right (Right pools) -> return pools
    Left err            -> throwString $ show err
    Right (Left err)    -> throwString $ show err

queryEraHistory :: MonadIO m => NodeConn -> m (EraHistory CardanoMode)
queryEraHistory conn = do
  cp <- queryChainPoint conn
  let query = QueryEraHistory CardanoModeIsMultiEra
  resOrErr <- liftIO $ queryNodeLocalState conn (Just cp) query
  liftEither resOrErr

queryNetworkParams :: MonadIO m => NodeConn -> NetworkId -> m NetworkParameters
queryNetworkParams conn network = do
  sysstart <- querySystemStart conn
  pparams  <- queryProtocolParams conn
  pools    <- queryPools conn
  eraHistory <- queryEraHistory conn
  return $ NetworkParameters {..}


