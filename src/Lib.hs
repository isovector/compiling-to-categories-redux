{-# LANGUAGE GADTs           #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Lib where

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
  toCCCName <- lookupGlobalOccRn $ Unqual (mkVarOcc "toCCC")
  -- pprPanic "hi" $ ppr toCCCName
  let ?toCCC = toCCCName
  -- pprPanic "uh oh" $ showAstData BlankSrcSpan $ hs_valds grp
  pprPanic "uh oh" $ ppr $ listify (\case
    ToCCC lambda -> True
    _ -> False) grp
    -- -- BlankSrcSpan grp

pattern ToCCC :: (?toCCC :: Name) => HsExpr GhcRn -> HsExpr GhcRn
pattern ToCCC lambda
  <- HsApp _ (L _ (HsVar _ (L _ ((== ?toCCC) -> True)))) (L _ (HsPar _ (L _ lambda)))
  where
    ToCCC lambda =
      HsApp noExt
        (noLoc (HsVar noExt (noLoc ?toCCC)))
        (noLoc (HsPar noExt (noLoc lambda)))

-- toCCC (someLambda)

