{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Spec.PingPong(tests) where

import Control.Monad (void)
import           Data.Maybe                                        (isNothing, isJust)
import           Language.Plutus.Contract
import           Language.Plutus.Contract.Test
import           Language.PlutusTx.Lattice

import           Language.Plutus.Contract.StateMachine             (OnChainState)
import           Language.PlutusTx.Coordination.Contracts.PingPong (Input, PingPongError, PingPongSchema, PingPongState)
import qualified Language.PlutusTx.Coordination.Contracts.PingPong as PingPong
import qualified Plutus.Trace.Emulator    as Trace
import Plutus.Trace.Emulator (EmulatorTrace)

import           Test.Tasty

theContract :: Contract PingPongSchema PingPongError ()
theContract = do
    _ <- PingPong.initialise
    PingPong.runPong
    PingPong.runPing
    PingPong.runPong

twoParties :: Contract PingPongSchema PingPongError (Maybe (OnChainState PingPongState Input))
twoParties =
    -- one party calls "initialise"
    -- the other party calls "stop"
    -- then the first party will learn that the instance has
    -- terminated when 'runWaitForUpdate' returns 'Nothing'.
    let p1 = PingPong.initialise >> PingPong.runWaitForUpdate
        p2 = PingPong.runStop
    in p1 `select` fmap (const Nothing) p2

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

tests :: TestTree
tests = testGroup "pingpong"
    [ checkPredicate "activate endpoints"
        (endpointAvailable @"pong" theContract (Trace.walletInstanceTag w1))
        $ do
            hdl <- Trace.activateContractWallet w1 theContract
            Trace.callEndpoint @"initialise" hdl ()
            Trace.waitNSlots 2
            Trace.callEndpoint @"pong" hdl ()
            Trace.waitNSlots 2
            Trace.callEndpoint @"ping" hdl ()
            void $ Trace.waitNSlots 2
    , checkPredicate "Stop the contract"
        (assertDone twoParties (Trace.walletInstanceTag w1) isJust "W1"
        .&&. assertDone twoParties (Trace.walletInstanceTag w2) isNothing "W2"
        )
        $ do
            hdl1 <- Trace.activateContractWallet w1 (void twoParties)
            hdl2 <- Trace.activateContractWallet w2 (void twoParties)
            Trace.callEndpoint @"initialise" hdl1 ()
            Trace.waitNSlots 2
            Trace.callEndpoint @"stop" hdl2 ()
            void $ Trace.waitNSlots 2

    ]
