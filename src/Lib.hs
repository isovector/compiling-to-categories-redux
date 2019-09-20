{-# LANGUAGE GADTs            #-}
{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module Lib where

import           Bag
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.List
import qualified Data.Map as M
import           GhcPlugins
import           HsBinds
import           HsDecls
import           HsDumpAst
import           HsExpr
import           HsExtension
import           HsPat
import           Plugins
import           RnEnv
import           TcEvidence
import           TcRnMonad
import           TcRnTypes

plugin :: Plugin
plugin = defaultPlugin
  { renamedResultAction = ourPluginImpl
  }


data Context = Context
  { ctxToCCC      :: Name
  , ctxId         :: Name
  , ctxCompose    :: Name
  , ctxApply      :: Name
  , ctxFork       :: Name
  , ctxCurry      :: Name
  , ctxFst        :: Name
  , ctxSnd        :: Name
  , ctxConst      :: Name
  , ctxConstFun   :: Name
  , ctxKnownFuncs :: M.Map Name Name
  , ctxDontSplit  :: [Name]
  }

buildContext :: TcM Context
buildContext = do
  let find nm = lookupGlobalOccRn $ Qual (mkModuleName "CCC") (mkVarOcc nm)
  let findPrel nm = lookupGlobalOccRn $ Qual (mkModuleName "Prelude") (mkVarOcc nm)

  ctxToCCC    <- find "toCCC"
  ctxId       <- find "id"
  ctxCompose  <- find "."
  ctxApply    <- find "apply"
  ctxFork     <- find "&&&"
  ctxCurry    <- find "curry"
  ctxFst      <- find "fst"
  ctxSnd      <- find "snd"
  ctxConst    <- find "const"
  ctxConstFun <- find "constFun"

  prelPlus <- findPrel "+"
  plus     <- find "addC"

  let ctxKnownFuncs = M.singleton prelPlus plus
      ctxDontSplit = [ctxFst, ctxSnd]

  pure $ Context {..}


ourPluginImpl
    :: [CommandLineOption]
    -> TcGblEnv
    -> HsGroup GhcRn
    -> TcM (TcGblEnv, HsGroup GhcRn)
ourPluginImpl _ gbl_env grp = do
  ctx <- buildContext
  let ?ctx = ctx
  let grp' = everywhere' (mkT rewrite) grp

  pprTraceM "result" $ ppr $ hs_valds grp'


  case flip listify grp' (\case { ToCCC _ -> True; _ -> False; }) of
    [] -> pure ()
    x -> pprPanic "You tried to compile a bad ccc" $ ppr x

  pure (gbl_env, grp')


rewrite :: (?ctx :: Context) => HsExpr GhcRn -> HsExpr GhcRn
rewrite (ToCCC (Lambda var (HsVar _ (L _ name))))
  | var == name = HsVar noExt $ noLoc $ ctxId ?ctx
rewrite (ToCCC (Lambda x (HsApp _ (L _ z@(HsVar _ (L _ u))) (L _ v))))
  | elem u (ctxDontSplit ?ctx)
  = z
rewrite (ToCCC (Lambda x (HsApp _ (L _ u) (L _ v)))) =
  applyFun (ctxCompose ?ctx)
    [ HsVar noExt $ noLoc $ ctxApply ?ctx
    , applyFun (ctxFork ?ctx)
        [ ToCCC $ Lambda x u
        , ToCCC $ Lambda x v
        ]
    ]
rewrite (ToCCC (Lambda x (Lambda y u))) =
  let z = HsVar noExt $ noLoc x in
  applyFun (ctxCurry ?ctx)
    [ ToCCC $ Lambda x $ subst x (applyFun (ctxFst ?ctx) [z])
            $ subst y (applyFun (ctxSnd ?ctx) [z])
            $ u
    ]
rewrite (ToCCC (Lambda x v@(HsVar _ (L _ c)))) =
  case M.lookup c (ctxKnownFuncs ?ctx) of
    -- TODO(sandy): this curry shouldnt always be here; it should depend on the arity of the function
    Just c' ->
      applyFun (ctxConstFun ?ctx) [applyFun (ctxCurry ?ctx) [HsPar noExt $ noLoc $ HsVar noExt $ noLoc c']]
    _ -> doConst v
rewrite (ToCCC (Lambda x c)) = doConst c
rewrite (ToCCC z) = pprPanic "couldn't reduce" $ ppr z
rewrite (HsPar _ (L _ a)) = a
rewrite a = a

doConst :: (?ctx::Context) => HsExpr GhcRn -> HsExpr GhcRn
doConst c = applyFun (ctxConst ?ctx) [c]


subst :: Name -> HsExpr GhcRn -> HsExpr GhcRn -> HsExpr GhcRn
subst nm rep = everywhere $ mkT $
  \case
    HsVar _ (L _ nm') | nm == nm' -> rep
    a -> a

applyFun
  :: (XVar p ~ NoExt, XApp p ~ NoExt) =>
     IdP p -> [HsExpr p] -> HsExpr p
applyFun fn_nm args = unLoc $
  foldl' ((noLoc .) .HsApp noExt) (noLoc $ HsVar noExt $ noLoc fn_nm) $ fmap noLoc args


--   HsApp noExt
--     (noLoc $ HsVar noExt $ noLoc fn_nm)
--     (noLoc arg)


pattern Lambda :: Name -> HsExpr GhcRn -> HsExpr GhcRn
pattern Lambda var body
  <- HsLam _ (MG _ (L _ [L _ (Match _ _ [L _ (VarPat _ (L _ var))] (GRHSs _ [L _ (GRHS _ [] (L _ body))] _))]) _)
  where
    Lambda var body =
      HsLam noExt
            (MG noExt
                (noLoc [ noLoc (Match noExt
                                      LambdaExpr
                                      [noLoc (VarPat noExt (noLoc var))]
                                      (GRHSs noExt
                                             [ noLoc (GRHS noExt
                                                           []
                                                           (noLoc body))
                                             ]
                                             (noLoc $ EmptyLocalBinds noExt)))
                       ])
                Generated)



pattern ToCCC :: (?ctx :: Context) => HsExpr GhcRn -> HsExpr GhcRn
pattern ToCCC lambda
  <- HsApp _ (L _ (HsVar _ (L _ ((== ctxToCCC ?ctx) -> True)))) (L _ (HsPar _ (L _ lambda)))
  where
    ToCCC lambda =
      HsApp noExt
        (noLoc (HsVar noExt (noLoc $ ctxToCCC ?ctx)))
        (noLoc (HsPar noExt (noLoc lambda)))

-- toCCC (someLambda)

