{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Plutus.Trace.Effects.EmulatorControl(
    EmulatorControl(..)
    , setSigningProcess
    , agentState
    , freezeContractInstance
    , thawContractInstance
    , chainState
    , handleEmulatorControl
    ) where

import Control.Lens (view, at)
import Control.Monad (void)
import Control.Monad.Freer (type (~>), Eff, Member)
import Control.Monad.Freer.State (State, gets)
import Data.Maybe (fromMaybe)
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.Freer.Coroutine (Yield)
import Wallet.Emulator.Wallet (Wallet, SigningProcess, WalletState)
import Wallet.Types (ContractInstanceId)
import Wallet.Emulator.Chain (ChainState)
import Plutus.Trace.Scheduler (SystemCall, mkSysCall, Priority(Normal), SysCall(Message, Thaw))
import Plutus.Trace.Emulator.Types (EmulatorMessage(Freeze), EmulatorThreads)
import           Plutus.Trace.Emulator.ContractInstance          (EmulatorRuntimeError, getThread)
import qualified Wallet.Emulator.Wallet                          as W
import qualified Wallet.Emulator                                 as EM
import           Wallet.Emulator.MultiAgent (walletControlAction, MultiAgentEffect, EmulatorState)

data EmulatorControl r where
    SetSigningProcess :: Wallet -> SigningProcess -> EmulatorControl ()
    AgentState :: Wallet -> EmulatorControl WalletState
    FreezeContractInstance :: ContractInstanceId -> EmulatorControl ()
    ThawContractInstance :: ContractInstanceId -> EmulatorControl ()
    ChainState :: EmulatorControl ChainState

handleEmulatorControl ::
    forall effs effs2.
    ( Member (State EmulatorThreads) effs
    , Member (State EmulatorState) effs
    , Member (Error EmulatorRuntimeError) effs
    , Member MultiAgentEffect effs
    , Member (Yield (SystemCall effs2 EmulatorMessage) (Maybe EmulatorMessage)) effs
    )
    => EmulatorControl
    ~> Eff effs
handleEmulatorControl = \case
    SetSigningProcess wllt sp -> walletControlAction wllt $ W.setSigningProcess sp
    AgentState wllt -> gets @EmulatorState (fromMaybe (W.emptyWalletState wllt) . view (EM.walletStates . at wllt))
    FreezeContractInstance i -> do
        threadId <- getThread i
        void $ mkSysCall @effs2 @EmulatorMessage Normal (Message threadId Freeze)
    ThawContractInstance i -> do
        threadId <- getThread i
        void $ mkSysCall @effs2 @EmulatorMessage Normal (Thaw threadId)
    ChainState -> gets (view EM.chainState)

makeEffect ''EmulatorControl
