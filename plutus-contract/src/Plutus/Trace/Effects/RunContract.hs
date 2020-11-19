{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Trace.Effects.RunContract(
    RunContract(..)
    , ContractConstraints
    , ContractInstanceTag
    , activateContract
    , activateContractWallet
    , callEndpoint
    , getContractState
    , walletInstanceTag
    , handleRunContract
    ) where

import Control.Monad.Freer (Eff, type (~>), Member, interpret, reinterpret)
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.Freer.Log (LogMsg, mapLog, logError)
import Control.Monad (void)
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.State (State)
import Control.Monad.Freer.Reader (runReader, ask, Reader)
import           Control.Monad.Freer.Coroutine                   (Yield)

import Data.String (IsString(..))
import           Plutus.Trace.Effects.ContractInstanceId         (ContractInstanceIdEff, nextId)
import           Data.Proxy                         (Proxy (..))
import           Language.Plutus.Contract           (Contract, HasBlockchainActions, HasEndpoint)
import qualified Data.Row.Internal                  as V
import           Language.Plutus.Contract.Schema    (Input, Output)
import qualified Data.Aeson                         as JSON
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoint
import Wallet.Emulator.Wallet (Wallet(..))
import           Plutus.Trace.Emulator.Types (EmulatorThreads, EmulatorMessage(EndpointCall), ContractInstanceState(..),ContractInstanceError(JSONDecodingError), UserThreadMsg(UserThreadErr))
import           Plutus.Trace.Emulator.ContractInstance          (contractThread, getThread)
import           Plutus.Trace.Scheduler (SystemCall, fork, Tag, Priority(..), mkSysCall, SysCall(Message), ThreadId, sleep)
import           Wallet.Emulator.MultiAgent (MultiAgentEffect, EmulatorEvent'(..))
import Plutus.Trace.Emulator.Types (ContractHandle(..), ContractInstanceTag, EmulatorMessage(ContractInstanceStateRequest, ContractInstanceStateResponse))

type ContractConstraints s =
    ( V.Forall (Output s) V.Unconstrained1
    , V.Forall (Input s) V.Unconstrained1
    , V.AllUniqueLabels (Input s)
    , V.AllUniqueLabels (Output s)
    , V.Forall (Input s) JSON.FromJSON
    , V.Forall (Input s) JSON.ToJSON
    , V.Forall (Output s) JSON.FromJSON
    , V.Forall (Output s) JSON.ToJSON
    )

-- | The 'ContractInstanceTag' for the contract instance of a wallet. See note 
--   [Wallet contract instances]
walletInstanceTag :: Wallet -> ContractInstanceTag
walletInstanceTag (Wallet i) = fromString $ "Contract instance for wallet " <> show i

-- | Run a Plutus contract (client side)
data RunContract r where
    ActivateContract :: (ContractConstraints s, HasBlockchainActions s, Show e, JSON.FromJSON e, JSON.ToJSON e) => Wallet -> Contract s e a -> ContractInstanceTag -> RunContract (ContractHandle s e)
    CallEndpointP :: forall l ep s e. (ContractConstraints s, HasEndpoint l ep s) => Proxy l -> ContractHandle s e -> ep -> RunContract ()
    GetContractState :: forall s e. (ContractConstraints s, JSON.FromJSON e) => ContractHandle s e -> RunContract (ContractInstanceState s e ())

makeEffect ''RunContract

callEndpoint ::
    forall l ep s e effs.
    (ContractConstraints s, HasEndpoint l ep s, Member RunContract effs) => ContractHandle s e -> ep -> Eff effs ()
callEndpoint hdl v = callEndpointP (Proxy @l) hdl v

-- | Like 'activateContract', but using 'walletInstanceTag' for the tag.
activateContractWallet :: forall s e effs. (HasBlockchainActions s, ContractConstraints s, Show e, JSON.ToJSON e, JSON.FromJSON e, Member RunContract effs) => Wallet -> Contract s e () -> Eff effs (ContractHandle s e)
activateContractWallet w contract = activateContract w contract (walletInstanceTag w)

-- | Handle the 'RunContract' effect by running each contract instance in an
--   emulator thread.
handleRunContract :: forall effs effs2.
    ( Member (State EmulatorThreads) effs2
    , Member (Error ContractInstanceError) effs2
    , Member (Error ContractInstanceError) effs
    , Member MultiAgentEffect effs2
    , Member (LogMsg EmulatorEvent') effs2
    , Member (LogMsg EmulatorEvent') effs
    , Member ContractInstanceIdEff effs
    , Member (State EmulatorThreads) effs
    , Member (Reader ThreadId) effs
    , Member (Yield (SystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs
    )
    => RunContract
    ~> Eff effs
handleRunContract = \case
    ActivateContract w c t -> handleActivate @_ @_ @effs @effs2 w t (void c)
    CallEndpointP p h v -> handleCallEndpoint @_ @_ @_ @_ @effs @effs2 p h v
    GetContractState hdl -> 
        interpret (mapLog UserThreadEvent)
            $ handleGetContractState @_ @_ @(LogMsg UserThreadMsg ': effs) @effs2 hdl

handleGetContractState ::
    forall s e effs effs2.
    ( Member (State EmulatorThreads) effs
    , Member (Yield (SystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs
    , Member (Reader ThreadId) effs
    , Member (Error ContractInstanceError) effs
    , ContractConstraints s
    , JSON.FromJSON e
    , Member (LogMsg UserThreadMsg) effs
    )
    => ContractHandle s e
    -> Eff effs (ContractInstanceState s e ())
handleGetContractState ContractHandle{chInstanceId} = do
    ownId <- ask @ThreadId
    threadId <- getThread chInstanceId
    void $ mkSysCall @effs2 @EmulatorMessage Normal (Message threadId $ ContractInstanceStateRequest ownId)

    let checkResponse = \case
            Just (ContractInstanceStateResponse r) -> do
                case JSON.fromJSON @(ContractInstanceState s e ()) r of
                    JSON.Error e' -> do
                        let msg = JSONDecodingError e'
                        logError $ UserThreadErr msg
                        throwError msg
                    JSON.Success event' -> pure event'
            _ -> sleep @effs2 Sleeping >>= checkResponse
    sleep @effs2 Normal >>= checkResponse

handleActivate :: forall s e effs effs2.
    ( ContractConstraints s
    , Member ContractInstanceIdEff effs
    , Member (State EmulatorThreads) effs2
    , Member MultiAgentEffect effs2
    , Member (Error ContractInstanceError) effs2
    , Member (LogMsg EmulatorEvent') effs2
    , Member (Yield (SystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs
    , HasBlockchainActions s
    , Show e
    , JSON.ToJSON e
    )
    => Wallet
    -> ContractInstanceTag
    -> Contract s e ()
    -> Eff effs (ContractHandle s e)
handleActivate wllt tag con = do
    i <- nextId
    let handle = ContractHandle{chContract=con, chInstanceId = i, chInstanceTag = tag}
    _ <- fork @effs2 @EmulatorMessage runningContractInstanceTag Normal (runReader wllt $ interpret (mapLog InstanceEvent) $ reinterpret (mapLog InstanceEvent) $ contractThread handle)
    pure handle

runningContractInstanceTag :: Tag
runningContractInstanceTag = "contract instance"

handleCallEndpoint :: forall s l e ep effs effs2.
    ( ContractConstraints s
    , HasEndpoint l ep s
    , Member (State EmulatorThreads) effs2
    , Member (Error ContractInstanceError) effs2
    , Member (Yield (SystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs
    )
    => Proxy l
    -> ContractHandle s e
    -> ep
    -> Eff effs ()
handleCallEndpoint _ ContractHandle{chInstanceId} ep = do
    let epJson = JSON.toJSON $ Endpoint.event @l @ep @s ep
        thr = do
            threadId <- getThread chInstanceId
            void $ mkSysCall @effs2 @EmulatorMessage Normal (Message threadId $ EndpointCall epJson)
    void $ fork @effs2 @EmulatorMessage callEndpointTag Normal thr

callEndpointTag :: Tag
callEndpointTag = "call endpoint"
