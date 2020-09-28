{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- | Interfacing with the wallet (for making payments)
module Plutus.Trace.Effects.EmulatedWalletAPI(
    EmulatedWalletAPI(..)
    , liftWallet
    , payToWallet
    , handleEmulatedWalletAPI
    ) where

import qualified Wallet.Emulator                                 as EM
import Control.Monad.Freer (Eff, type (~>), Member, subsume)
import Control.Monad.Freer.Extras (raiseEnd2)
import Control.Monad.Freer.TH (makeEffect)
import           Wallet.Emulator.Wallet (Wallet)
import Wallet.Effects (WalletEffect, SigningProcessEffect)
import Wallet.Emulator.MultiAgent (MultiAgentEffect, walletAction)
import Ledger.Value (Value)
import Ledger.Tx (txId)
import Ledger.TxId (TxId)
import           Wallet.API                                      (payToPublicKey, defaultSlotRange)

data EmulatedWalletAPI r where
    LiftWallet :: Wallet -> Eff '[WalletEffect, SigningProcessEffect] a -> EmulatedWalletAPI a

makeEffect ''EmulatedWalletAPI

-- | Make a payment from one wallet to another
payToWallet ::
    forall effs.
    Member EmulatedWalletAPI effs
    => Wallet
    -> Wallet
    -> Value
    -> Eff effs TxId
payToWallet source target amount =
    liftWallet source $ fmap txId $ payToPublicKey defaultSlotRange amount (EM.walletPubKey target) 

-- | Handle the 'EmulatedWalletAPI' effect using the emulator's
--   'MultiAgent' effect.
handleEmulatedWalletAPI ::
    Member MultiAgentEffect effs
    => EmulatedWalletAPI
    ~> Eff effs
handleEmulatedWalletAPI = \case
    LiftWallet w action ->
        walletAction w
            $ subsume
            $ subsume
            $ raiseEnd2 action