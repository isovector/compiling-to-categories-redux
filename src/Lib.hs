{-# LANGUAGE GADTs           #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Lib where

import HsPat
import Plugins
import GhcPlugins
import TcRnTypes
import HsDecls
import HsExpr
import HsExtension
import HsDumpAst
import RnEnv
import Data.Generics.Schemes
import Data.Generics.Aliases

plugin :: Plugin
plugin = defaultPlugin
  { renamedResultAction = ourPluginImpl
  }

ourPluginImpl
    :: [CommandLineOption]
    -> TcGblEnv
    -> HsGroup GhcRn
    -> TcM (TcGblEnv, HsGroup GhcRn)
ourPluginImpl _ gbl_env grp = do
  toCCCName <- lookupGlobalOccRn $ Qual (mkModuleName "CCC") (mkVarOcc "toCCC")
  id' <- lookupGlobalOccRn $ Qual (mkModuleName "Control.Category") (mkVarOcc "id")
  let ?toCCC = toCCCName
      ?id = id'

  let grp' = everywhere (mkT rewrite) grp
  pure (gbl_env, grp')

--   pprPanic "force it" $ ppr grp'
--   -- pprPanic "uh oh" $ ppr $ flip listify grp $ \case
--   --   ToCCC lambda -> True
--   --   _            -> False


rewrite :: (?toCCC :: Name, ?id :: Name) => HsExpr GhcRn -> HsExpr GhcRn
rewrite (ToCCC (Lambda var (HsVar _ (L _ name)))) | var == name = HsVar noExt $ noLoc ?id
rewrite a = a


pattern Lambda :: Name -> HsExpr GhcRn -> HsExpr GhcRn
pattern Lambda var body <- HsLam _ (MG _ (L _ [L _ (Match _ _ [L _ (VarPat _ (L _ var))] (GRHSs _ [L _ (GRHS _ [] (L _ body))] _))]) _)


pattern ToCCC :: (?toCCC :: Name) => HsExpr GhcRn -> HsExpr GhcRn
pattern ToCCC lambda
  <- HsApp _ (L _ (HsVar _ (L _ ((== ?toCCC) -> True)))) (L _ (HsPar _ (L _ lambda)))
  where
    ToCCC lambda =
      HsApp noExt
        (noLoc (HsVar noExt (noLoc ?toCCC)))
        (noLoc (HsPar noExt (noLoc lambda)))

-- toCCC (someLambda)

