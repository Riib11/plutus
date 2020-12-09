module Language.PlutusTx.LoadBind
    ( newLoadBind
    , loadBind
    ) where

import NameEnv
import Id
import TcRnTypes
import TcRnMonad
import Data.Maybe
import Outputable
import IfaceSyn
import IfaceEnv
import TcIface
import BinIface
import Maybes
import Data.IORef
import Name
import Module
import HscTypes
import CoreSyn
import GhcPlugins

{-
Initialise a stateful `IO` function for loading core bindings by loading the
relevant `ModIface` from disk. Each interface that is loaded has its bindings
cached within an `IORef (ModuleEnv (Maybe (NameEnv (Bind CoreBndr))))`.

The current module doesn't have an interface file yet, so we recover its binds
from the `ModGuts` instead. However, since we're running this plugin early in
the core pipeline, the current module's binds won't have passed through the
core optimisation phases yet, so if we're inlining `Name`s from this module
we'll have to rely on the optimisations being run on our already inlined ASTs.
-}
newLoadBind :: HscEnv -> ModGuts -> IO (Name -> IO (Maybe (Bind CoreBndr)))
newLoadBind hscEnv guts = do
  let binds = mkNameEnvWith nameOf (mg_binds guts)
  modBindsR <- newIORef (extendModuleEnv emptyModuleEnv (mg_module guts) (Just binds))
  return (loadBind modBindsR hscEnv)

{-
Return the `Bind` for the given `Name`. Cache the results of disk lookups.
-}
loadBind :: IORef (ModuleEnv (Maybe (NameEnv (Bind CoreBndr))))
         -> HscEnv
         -> Name
         -> IO (Maybe (Bind CoreBndr))
loadBind modBindsR env name = do
  eps      <- hscEPS env
  modBinds <- readIORef modBindsR
  case nameModule_maybe name of
   Just modu | Just iface <- lookupIfaceByModule (hsc_HPT env) (eps_PIT eps) modu -> do
      case lookupModuleEnv modBinds modu of
        Just Nothing -> return Nothing -- We've already checked this module, and it doesn't have bindings
                                       -- serialised - probably because it's from an external package,
                                       -- but it could also have not been compiled with the plugin.
        Just (Just binds) -> return $ lookupNameEnv binds name -- We've imported this module - lookup the binding.
        Nothing -> do -- Try and import the module.
             bnds <- initIfaceLoad env $
                     -- FIXME: original NotBoot changed to False
                     initIfaceLcl (mi_semantic_module iface) (text "core") False $
                       loadCoreBindings iface
             case bnds of
               Just bds -> do
                 let binds' = mkNameEnvWith nameOf bds
                 writeIORef modBindsR (extendModuleEnv modBinds modu (Just binds'))
                 return $ lookupNameEnv binds' name
               Nothing -> do
                 writeIORef modBindsR (extendModuleEnv modBinds modu Nothing)
                 return Nothing
   _ -> return Nothing



{-
Retrieve the name of a top-level binding. In the case of recursive bindings,
we assume (based on which types of bindings we have determined become top-level
recursive bindings):
* The binding has at least one case
* All cases have the same `Name`.
-}
nameOf :: Bind Id -> Name
nameOf (NonRec n _)     = idName n
nameOf (Rec ((n, _):_)) = idName n
-- FIXME: pattern match non-exhaustive
nameOf _ = error "nameOf pattern match match non-exhaustive"

{-
Perform interface `typecheck` loading from this binding's extensible interface
field within the deserialised `ModIface` to load the bindings that the field
contains, if the field exists.
-}
loadCoreBindings :: ModIface -> IfL (Maybe [Bind CoreBndr])
loadCoreBindings iface@ModIface{mi_module = modu} = do
  ncu <- mkNameCacheUpdater
  mbinds <- liftIO (readIfaceFieldWith "plutus/core-bindings" (getWithUserData ncu) iface)
  case mbinds of
    Just (loc, ibinds) -> Just . catMaybes <$> mapM (tcIfaceBinding modu loc) ibinds
    Nothing            -> return Nothing



-------------------------------------------------------------------------------
-- Interface loading for top-level bindings
-------------------------------------------------------------------------------

{-
Certain RHSs fail to typecheck due to the error `Iface id out of scope: ...`.
In particular, this workaround is used to exclude GHC's special type reflection
bindings from causing problems in loading.

In theory, we should be removing them during serialisation, but they are structured
as real bindings, so we would have to do a fragile test on the `Name`.
-}
tcIfaceBinding :: Module -> SrcSpan -> IfaceBinding -> IfL (Maybe (Bind Id))
tcIfaceBinding modu loc ibind =
  rightToMaybe <$> tryAllM (tcIfaceBinding' modu loc ibind)

tcIfaceBinding' :: Module -> SrcSpan -> IfaceBinding -> IfL (Bind Id)
tcIfaceBinding' _mod _loc b =
  case b of
    IfaceNonRec letbndr rhs -> uncurry NonRec <$> go letbndr rhs
    IfaceRec    pairs       -> Rec <$> mapM (uncurry go) pairs
  where
    go (IfLetBndr fs ty _info ji) rhs = do
        name <- lookupIfaceTop (mkVarOccFS fs)
        ty'  <- tcIfaceType ty
        rhs' <- tcIfaceExpr rhs
        let ident = mkExportedVanillaId name ty' `asJoinId_maybe` tcJoinInfo ji
        return (ident, rhs')

