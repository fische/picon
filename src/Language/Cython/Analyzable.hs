{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Language.Cython.Analyzable (
  Analyzable(..),
  Context(..),
  empty
) where

import qualified Data.Map.Strict as Map
import Data.Data
import Data.Bool

import Control.Monad.State (get, put)
import Control.Monad.Trans.Except

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.SrcLocation (SrcSpan(..))
import Language.Cython.Error
import Language.Cython.Context
import Language.Cython.AST
import Language.Cython.Annotation
import Language.Cython.Type

type State annot = ContextState Context annot

data Context =
  Context {
    inGlobalScope :: Bool,
    options :: Options,
    globalVars :: Map.Map String [TypeAnnotation],
    outerVars :: Map.Map String [TypeAnnotation],
    localVars :: Map.Map String Binding
  }
  deriving (Eq,Ord,Show,Typeable,Data)

empty :: Context
empty = Context {
  inGlobalScope = False,
  globalVars = Map.empty,
  outerVars = Map.empty,
  localVars = Map.empty,
  options = Options{}
}

mergeLocalScope :: Context -> Map.Map String Binding ->
  (Map.Map String [TypeAnnotation], Context)
mergeLocalScope ctx localScope =
  let (innerLocals, innerBindings) =
        Map.partition isLocal localScope -- TODO Resolve references to innerLocals
      (innerBoundGlobals, innerBoundNonLocals) =
        Map.partition isGlobal innerBindings
      mergeBinding old new = (cytype new) ++ old
      newOuters =
        Map.intersectionWith mergeBinding (outerVars ctx) innerBoundNonLocals
      newGlobals =
        Map.unionWith (++) (fmap cytype innerBoundGlobals) (globalVars ctx)
      toResolve = fmap cytype innerLocals
      resetBindings = Map.map (\l -> l{cytype = []}) innerBindings
  in (toResolve, ctx{
    localVars = Map.union innerLocals resetBindings,
    outerVars = newOuters,
    globalVars = newGlobals
  })

copyScope :: State annot Context
copyScope = do
  ctx <- get
  return ctx

openScope :: State annot Context
openScope = do
  ctx <- copyScope
  let mergedCtx = snd $ mergeLocalScope ctx (localVars ctx)
      locals = localVars mergedCtx
      localGlobals = (locals, Map.empty)
      partition = Map.partition isGlobal locals
      (globals, outers) = bool partition localGlobals (inGlobalScope mergedCtx)
      updatedGlobal = Map.union (fmap cytype globals) (globalVars mergedCtx)
      updatedOuter = Map.union (fmap cytype outers) (outerVars mergedCtx)
  put mergedCtx
  return (mergedCtx{
    inGlobalScope = False,
    globalVars = Map.map (const []) updatedGlobal,
    outerVars = Map.map (const []) updatedOuter,
    localVars = Map.empty
  })

openModule :: State annot Context
openModule = do
  ctx <- get
  return (ctx{
    inGlobalScope = True,
    globalVars = Map.empty,
    outerVars = Map.empty,
    localVars = Map.empty
  })

getBindingRef :: String -> Binding -> TypeAnnotation
getBindingRef ident (Local _) = LocalRef ident
getBindingRef ident (NonLocal _) = NonLocalRef ident
getBindingRef ident (Global _) = GlobalRef ident

getVarRef :: String -> State annot TypeAnnotation
getVarRef ident = do
  ctx <- get
  let inLocalScope = Map.lookup ident (localVars ctx)
      inOuterScope = Map.member ident (outerVars ctx)
  return $ maybe
    (bool (GlobalRef ident) (NonLocalRef ident) inOuterScope)
    (getBindingRef ident)
    inLocalScope

addVarType :: String -> TypeAnnotation -> State annot ()
addVarType ident typ = do
  ctx <- get
  let insertBinding _ old = old{ cytype = (typ : (cytype old)) }
      binding = Local [typ]
      oldLocals = localVars ctx
      newLocals = Map.insertWith insertBinding ident binding oldLocals
  put ctx{localVars = newLocals}

bindGlobalVars' :: annot -> Context -> [String] ->
  State annot Context
bindGlobalVars' _ ctx [] = return ctx
bindGlobalVars' loc ctx (ident:tl) =
  let locals = localVars ctx
      found = Map.lookup ident locals
  in maybe
    (bindGlobalVars' loc (ctx{
      localVars = Map.insert ident (Global []) locals
    }) tl)
    (\var -> if isLocal var
      then
        -- TODO Handle when variable is already declared locally
        throwE $ errVarAlreadyDeclared loc ident
      else if isNonLocal var
        then
          throwE $ errVarAlreadyBound loc ident
        else
          bindGlobalVars' loc ctx tl)
    found

bindGlobalVars :: annot -> [String] -> State annot ()
bindGlobalVars loc idents = do
  ctx <- get
  if inGlobalScope ctx
    then
      throwE $ errNotAllowedInGlobalScope loc "Global bindings"
    else do
      newCtx <- bindGlobalVars' loc ctx idents
      put newCtx
      return ()

bindNonLocalVars' :: annot -> Context -> [String] ->
  State annot Context
bindNonLocalVars' _ ctx [] = return ctx
bindNonLocalVars' loc ctx (ident:tl) =
  let locals = localVars ctx
      found = Map.lookup ident locals
      checkAndInsert =
        let outer = Map.member ident (outerVars ctx)
        in bool
          (throwE $ errVarNotFound loc ident)
          (bindNonLocalVars' loc (ctx{
              localVars = Map.insert ident (NonLocal []) locals
          }) tl)
          outer
  in maybe
    checkAndInsert
    (\var -> if isLocal var
      then
        -- TODO Handle when variable is already declared locally
        throwE $ errVarAlreadyDeclared loc ident
      else if isGlobal var
        then
          throwE $ errVarAlreadyBound loc ident
        else
          bindNonLocalVars' loc ctx tl)
    found


bindNonLocalVars :: annot -> [String] -> State annot ()
bindNonLocalVars loc idents = do
  ctx <- get
  if inGlobalScope ctx
    then
      throwE $ errNotAllowedInGlobalScope loc "Nonlocal bindings"
    else do
      newCtx <- bindNonLocalVars' loc ctx idents
      put newCtx
      return ()

inScope :: Map.Map String Binding -> String -> a -> Bool
inScope scope ident _ = Map.member ident scope

mergeCopy :: Context -> State annot (Map.Map String [TypeAnnotation])
mergeCopy innerCtx = do
  currCtx <- get
  let currLocals = localVars currCtx
      (newLocalScope, innerScope) =
        Map.partitionWithKey (inScope currLocals) (localVars innerCtx)
      (resolvedLocals, mergedInner) = mergeLocalScope innerCtx innerScope

  put currCtx{
    localVars = newLocalScope,
    outerVars = (outerVars mergedInner),
    globalVars = (globalVars mergedInner)
  }
  return resolvedLocals

mergeScope :: Context -> State annot (Map.Map String [TypeAnnotation])
mergeScope innerCtx = do
  currCtx <- get
  let innerScope = (localVars innerCtx)
      (resolvedLocals, mergedInner) = mergeLocalScope innerCtx innerScope

      mergeTypes old new = new ++ old
      mergeBindings old new =
        old{cytype = mergeTypes (cytype old) (cytype new)}

  put $ bool
    -- Split updated variables in local and outer scope if not in global scope
    (let currLocals = Map.filter isLocal (localVars currCtx)
         (updatedLocals, updatedOuters) =
           Map.partitionWithKey (inScope currLocals) (outerVars mergedInner)

         newGlobals =
           Map.unionWith mergeTypes (globalVars currCtx) (globalVars mergedInner)
         newOuters =
           Map.unionWith mergeTypes (outerVars currCtx) updatedOuters

    in currCtx{
      localVars = Map.unionWith mergeBindings (localVars currCtx)
        (Local <$> updatedLocals),
      outerVars = newOuters,
      globalVars = newGlobals
    })
    (currCtx{
      localVars = Map.unionWith mergeBindings (localVars currCtx)
        (Local <$> (globalVars mergedInner))
    })
    (inGlobalScope currCtx)
  return resolvedLocals

mergeModule :: Context -> State annot (Map.Map String [TypeAnnotation])
mergeModule innerCtx =
  let globals = localVars innerCtx -- TODO Resolve references to globals
  in return $ fmap cytype globals



class Analyzable t c where
  analyze :: t (Maybe CythonAnnotation, SrcSpan) ->
    State SrcSpan (c (Maybe CythonAnnotation, SrcSpan))

analyzeArray :: (Analyzable t c) => [t (Maybe CythonAnnotation, SrcSpan)] ->
  State SrcSpan [c (Maybe CythonAnnotation, SrcSpan)]
analyzeArray [] = return []
analyzeArray (hd:tl) = do
  rhd <- analyze hd
  rtl <- analyzeArray tl
  return (rhd:rtl)

analyzeSuite :: [AST.Statement (Maybe CythonAnnotation, SrcSpan)] ->
  State SrcSpan (Suite (Maybe CythonAnnotation, SrcSpan))
analyzeSuite s = do
  arr <- analyzeArray s
  return (Suite arr (Nothing, SpanEmpty))

analyzeMaybe :: (Analyzable t c) =>
  Maybe (t (Maybe CythonAnnotation, SrcSpan)) ->
  State SrcSpan (Maybe (c (Maybe CythonAnnotation, SrcSpan)))
analyzeMaybe (Just m) = do
  r <- analyze m
  return (Just r)
analyzeMaybe Nothing = return Nothing

analyzeGuards :: (Analyzable t c) =>
  [(t (Maybe CythonAnnotation, SrcSpan),
    AST.Suite (Maybe CythonAnnotation, SrcSpan))] ->
  State SrcSpan
    [(c (Maybe CythonAnnotation, SrcSpan),
      Suite (Maybe CythonAnnotation, SrcSpan))]
analyzeGuards [] = return []
analyzeGuards ((f,s):tl) = do
  cf <- analyze f
  ctx <- copyScope
  (cs, suiteCtx) <- runState (analyzeSuite s) ctx
  suiteLocals <- mergeCopy suiteCtx
  let suiteAnnot = (Just $ Locals suiteLocals, snd $ AST.annot cs)
  rtl <- analyzeGuards tl
  return ((cf, cs{suite_annot = suiteAnnot}):rtl)

analyzeContext :: (Analyzable t c) =>
  [(t (Maybe CythonAnnotation, SrcSpan),
    Maybe (t (Maybe CythonAnnotation, SrcSpan)))] ->
  State SrcSpan [(c (Maybe CythonAnnotation, SrcSpan),
    Maybe (c (Maybe CythonAnnotation, SrcSpan)))]
analyzeContext [] = return []
analyzeContext ((f,s):tl) = do
  cf <- analyze f
  cs <- analyzeMaybe s
  rtl <- analyzeContext tl
  return ((cf, cs):rtl)

instance Analyzable AST.Ident AST.Ident where
  analyze i = return i

instance Analyzable AST.Op AST.Op where
  analyze o = return o

instance Analyzable AST.AssignOp AST.AssignOp where
  analyze o = return o

instance Analyzable AST.Module Module where
  analyze (AST.Module stmts) = do
    ctx <- openModule
    (rstmts, stmtsCtx) <- runState (analyzeSuite stmts) ctx
    stmtsLocals <- mergeModule stmtsCtx
    let stmtsAnnot = (Just $ Locals stmtsLocals, snd $ AST.annot rstmts)
    return (Module rstmts{suite_annot = stmtsAnnot} (Nothing, SpanEmpty))

instance Analyzable AST.ImportItem AST.ImportItem where
  analyze (AST.ImportItem item as annot) = do
    citem <- analyzeArray item
    cas <- analyzeMaybe as
    return (AST.ImportItem citem cas annot)
--
instance Analyzable AST.FromItem AST.FromItem where
  analyze (AST.FromItem item as annot) = do
    citem <- analyze item
    cas <- analyzeMaybe as
    return (AST.FromItem citem cas annot)

instance Analyzable AST.FromItems AST.FromItems where
  analyze (AST.ImportEverything annot) =
    return (AST.ImportEverything annot)
  analyze (AST.FromItems items annot) = do
    citems <- analyzeArray items
    return (AST.FromItems citems annot)

instance Analyzable AST.ImportRelative AST.ImportRelative where
  analyze (AST.ImportRelative dots (Just dotted) annot) = do
    rmod <- analyzeArray dotted
    return (AST.ImportRelative dots (Just rmod) annot)
  analyze (AST.ImportRelative dots Nothing annot) =
    return (AST.ImportRelative dots Nothing annot)

instance Analyzable AST.Statement Statement where
  analyze (AST.Import items annot) = do
    citems <- analyzeArray items
    return (Statement $ AST.Import citems annot)
  analyze (AST.FromImport m items annot) = do
    cm <- analyze m
    citems <- analyze items
    return (Statement $ AST.FromImport cm citems annot)
  analyze (AST.While cond body e annot) = do
    ccond <- analyze cond
    ctx <- copyScope
    (cbody, bodyCtx) <- runState (analyzeSuite body) ctx
    bodyLocals <- mergeCopy bodyCtx
    (celse, elseCtx) <- runState (analyzeSuite e) ctx
    elseLocals <- mergeCopy elseCtx
    let bodyAnnot = (Just $ Locals bodyLocals, snd $ AST.annot cbody)
        elseAnnot = (Just $ Locals elseLocals, snd $ AST.annot celse)
    return (While ccond cbody{suite_annot = bodyAnnot}
      celse{suite_annot = elseAnnot} annot)
  analyze (AST.For targets gen body e annot) = do
    ctargets <- analyzeArray targets
    cgen <- analyze gen
    ctx <- copyScope
    (cbody, bodyCtx) <- runState (analyzeSuite body) ctx
    bodyLocals <- mergeCopy bodyCtx
    (celse, elseCtx) <- runState (analyzeSuite e) ctx
    elseLocals <- mergeCopy elseCtx
    let bodyAnnot = (Just $ Locals bodyLocals, snd $ AST.annot cbody)
        elseAnnot = (Just $ Locals elseLocals, snd $ AST.annot celse)
    return (For ctargets cgen cbody{suite_annot = bodyAnnot}
      celse{suite_annot = elseAnnot} annot)
  analyze (AST.Fun name args result body annot) = do
    cname <- analyze name
    ctx <- openScope
    (cargs, argsctx) <- runState (analyzeArray args) ctx
    (cbody, bodyCtx) <- runState (analyzeSuite body) argsctx
    bodyLocals <- mergeScope bodyCtx
    let bodyAnnot = (Just $ Locals bodyLocals, snd $ AST.annot cbody)
    return (Fun cname cargs result cbody{suite_annot = bodyAnnot} annot)
  analyze (AST.Class name args body annot) = do
    cname <- analyze name
    ctx <- openScope
    (cargs, argsctx) <- runState (analyzeArray args) ctx
    (cbody, bodyCtx) <- runState (analyzeSuite body) argsctx
    bodyLocals <- mergeScope bodyCtx
    let bodyAnnot = (Just $ Locals bodyLocals, snd $ AST.annot cbody)
    return (Class cname cargs cbody{suite_annot = bodyAnnot} annot)
  analyze (AST.Conditional guards e annot) = do
    cguards <- analyzeGuards guards
    ctx <- copyScope
    (celse, elseCtx) <- runState (analyzeSuite e) ctx
    elseLocals <- mergeCopy elseCtx
    let elseAnnot = (Just $ Locals elseLocals, snd $ AST.annot celse)
    return (Conditional cguards celse{suite_annot = elseAnnot} annot)
  -- TODO Handle when assign_to is an array with multiple elements
  analyze (AST.Assign [to@AST.Var{}] expr annot) = do
    cexpr <- analyze expr
    cto <- analyze to
    let ident = AST.ident_string $ AST.var_ident to
        exprTyp = maybe Unknown getType (fst $ AST.annot cexpr)
    _ <- addVarType ident exprTyp
    return (Statement $ AST.Assign [cto] cexpr annot)
  analyze (AST.Assign tos expr annot) = do
    cexpr <- analyze expr
    ctos <- analyzeArray tos
    return (Statement $ AST.Assign ctos cexpr annot)
  analyze (AST.AugmentedAssign to op expr annot) = do
    cexpr <- analyze expr
    cop <- analyze op
    cto <- analyze to
    return (Statement $ AST.AugmentedAssign cto cop cexpr annot)
  analyze (AST.Decorated decorators def annot) = do
    cdecorators <- analyzeArray decorators
    cydef <- analyze def
    return (Decorated cdecorators cydef annot)
  analyze (AST.Return expr annot) = do
    cexpr <- analyzeMaybe expr
    return (Statement $ AST.Return cexpr annot)
  analyze (AST.Try body excepts e fin annot) = do
    ctx <- copyScope
    (cbody, bodyCtx) <- runState (analyzeSuite body) ctx
    bodyLocals <- mergeCopy bodyCtx
    cexcepts <- analyzeArray excepts
    (celse, elseCtx) <- runState (analyzeSuite e) ctx
    elseLocals <- mergeCopy elseCtx
    (cfin, finCtx) <- runState (analyzeSuite fin) ctx
    finLocals <- mergeCopy finCtx
    let bodyAnnot = (Just $ Locals bodyLocals, snd $ AST.annot cbody)
        elseAnnot = (Just $ Locals elseLocals, snd $ AST.annot celse)
        finAnnot = (Just $ Locals finLocals, snd $ AST.annot cfin)
    return (Try cbody{suite_annot = bodyAnnot} cexcepts
      celse{suite_annot = elseAnnot} cfin{suite_annot = finAnnot} annot)
  analyze (AST.Raise expr annot) = do
    cexpr <- analyze expr
    return (Statement $ AST.Raise cexpr annot)
  analyze (AST.With wctx body annot) = do
    cwctx <- analyzeContext wctx
    ctx <- copyScope
    (cbody, bodyCtx) <- runState (analyzeSuite body) ctx
    bodyLocals <- mergeCopy bodyCtx
    let bodyAnnot = (Just $ Locals bodyLocals, snd $ AST.annot cbody)
    return (With cwctx cbody{suite_annot = bodyAnnot} annot)
  analyze (AST.Pass annot) = return (Statement $ AST.Pass annot)
  analyze (AST.Break annot) = return (Statement $ AST.Break annot)
  analyze (AST.Continue annot) = return (Statement $ AST.Continue annot)
  analyze (AST.Delete exprs annot) = do
    cexprs <- analyzeArray exprs
    return (Statement $ AST.Delete cexprs annot)
  analyze (AST.StmtExpr expr annot) = do
    cexpr <- analyze expr
    return (Statement $ AST.StmtExpr cexpr annot)
  analyze (AST.Global vars annot) = do
    bindGlobalVars (snd annot) (fmap AST.ident_string vars)
    cvars <- analyzeArray vars
    return (Statement $ AST.Global cvars annot)
  analyze (AST.NonLocal vars annot) = do
    bindNonLocalVars (snd annot) (fmap AST.ident_string vars)
    cvars <- analyzeArray vars
    return (Statement $ AST.NonLocal cvars annot)
  analyze (AST.Assert exprs annot) = do
    cexprs <- analyzeArray exprs
    return (Statement $ AST.Assert cexprs annot)
  analyze (AST.Print chevron exprs comma annot) = do
    cexprs <- analyzeArray exprs
    return (Statement $ AST.Print chevron cexprs comma annot)
  analyze (AST.Exec expr (Just (t1, Just t2)) annot) = do
    cexpr <- analyze expr
    rt1 <- analyze t1
    rt2 <- analyze t2
    return (Statement $ AST.Exec cexpr (Just (rt1, Just rt2)) annot)
  analyze (AST.Exec expr (Just (t1, Nothing)) annot) = do
    rt1 <- analyze t1
    cexpr <- analyze expr
    return (Statement $ AST.Exec cexpr (Just (rt1, Nothing)) annot)
  analyze (AST.Exec expr Nothing annot) = do
    cexpr <- analyze expr
    return (Statement $ AST.Exec cexpr Nothing annot)

instance Analyzable AST.RaiseExpr AST.RaiseExpr where
  analyze (AST.RaiseV3 (Just (expr1, Just expr2))) = do
    cexpr1 <- analyze expr1
    cexpr2 <- analyze expr2
    return (AST.RaiseV3 (Just (cexpr1, Just cexpr2)))
  analyze (AST.RaiseV3 (Just (expr1, Nothing))) = do
    cexpr1 <- analyze expr1
    return (AST.RaiseV3 (Just (cexpr1, Nothing)))
  analyze (AST.RaiseV3 Nothing) = return (AST.RaiseV3 Nothing)
  analyze (AST.RaiseV2 (Just (expr1, Just (expr2, Just expr3)))) = do
    cexpr1 <- analyze expr1
    cexpr2 <- analyze expr2
    cexpr3 <- analyze expr3
    return (AST.RaiseV2 (Just (cexpr1, Just (cexpr2, Just cexpr3))))
  analyze (AST.RaiseV2 (Just (expr1, Just (expr2, Nothing)))) = do
    cexpr1 <- analyze expr1
    cexpr2 <- analyze expr2
    return (AST.RaiseV2 (Just (cexpr1, Just (cexpr2, Nothing))))
  analyze (AST.RaiseV2 (Just (expr1, Nothing))) = do
    cexpr1 <- analyze expr1
    return (AST.RaiseV2 (Just (cexpr1, Nothing)))
  analyze (AST.RaiseV2 Nothing) = return (AST.RaiseV2 Nothing)

instance Analyzable AST.Decorator AST.Decorator where
  analyze (AST.Decorator name args annot) = do
    cname <- analyzeArray name
    cargs <- analyzeArray args
    return (AST.Decorator cname cargs annot)

instance Analyzable AST.Parameter AST.Parameter where
  analyze (AST.Param name py_annot dflt annot) = do
    cdflt <- analyzeMaybe dflt
    cname <- analyze name
    return (AST.Param cname py_annot cdflt annot)
  analyze (AST.VarArgsPos name py_annot annot) = do
    cname <- analyze name
    return (AST.VarArgsPos cname py_annot annot)
  analyze (AST.VarArgsKeyword name py_annot annot) = do
    cname <- analyze name
    return (AST.VarArgsKeyword cname py_annot annot)
  analyze (AST.EndPositional annot) = return (AST.EndPositional annot)
  analyze (AST.UnPackTuple unpack dflt annot) = do
    cunpack <- analyze unpack
    cdflt <- analyzeMaybe dflt
    return (AST.UnPackTuple cunpack cdflt annot)

instance Analyzable AST.ParamTuple AST.ParamTuple where
  analyze (AST.ParamTupleName name annot) = do
    cname <- analyze name
    return (AST.ParamTupleName cname annot)
  analyze (AST.ParamTuple tuple annot) = do
    ctuple <- analyzeArray tuple
    return (AST.ParamTuple ctuple annot)

instance Analyzable AST.Argument AST.Argument where
  analyze (AST.ArgExpr expr annot) = do
    cexpr <- analyze expr
    return (AST.ArgExpr cexpr annot)
  analyze (AST.ArgVarArgsPos expr annot) = do
    cexpr <- analyze expr
    return (AST.ArgVarArgsPos cexpr annot)
  analyze (AST.ArgVarArgsKeyword expr annot) = do
    cexpr <- analyze expr
    return (AST.ArgVarArgsKeyword cexpr annot)
  analyze (AST.ArgKeyword keyword expr annot) = do
    ckeyword <- analyze keyword
    cexpr <- analyze expr
    return (AST.ArgKeyword ckeyword cexpr annot)

instance Analyzable AST.Handler Handler where
  analyze (AST.Handler clause suite annot) = do
    cclause <- analyze clause
    ctx <- copyScope
    (csuite, suiteCtx) <- runState (analyzeSuite suite) ctx
    suiteLocals <- mergeCopy suiteCtx
    let suiteAnnot = (Just $ Locals suiteLocals, snd $ AST.annot csuite)
    return (Handler cclause csuite{suite_annot = suiteAnnot} annot)

instance Analyzable AST.ExceptClause AST.ExceptClause where
  analyze (AST.ExceptClause (Just (expr1, Just expr2)) annot) = do
    cexpr1 <- analyze expr1
    cexpr2 <- analyze expr2
    return (AST.ExceptClause (Just (cexpr1, Just cexpr2)) annot)
  analyze (AST.ExceptClause (Just (expr1, Nothing)) annot) = do
    cexpr1 <- analyze expr1
    return (AST.ExceptClause (Just (cexpr1, Nothing)) annot)
  analyze (AST.ExceptClause Nothing annot) =
    return (AST.ExceptClause Nothing annot)

instance Analyzable AST.Comprehension AST.Comprehension where
  analyze (AST.Comprehension expr for annot) = do
    cexpr <- analyze expr
    cfor <- analyze for
    return (AST.Comprehension cexpr cfor annot)

instance Analyzable AST.ComprehensionExpr AST.ComprehensionExpr where
  analyze (AST.ComprehensionExpr expr) = do
    cexpr <- analyze expr
    return (AST.ComprehensionExpr cexpr)
  analyze (AST.ComprehensionDict expr) = do
    cexpr <- analyze expr
    return (AST.ComprehensionDict cexpr)

instance Analyzable AST.CompFor AST.CompFor where
  analyze (AST.CompFor for in_expr iter annot) = do
    cfor <- analyzeArray for
    cin_expr <- analyze in_expr
    citer <- analyzeMaybe iter
    return (AST.CompFor cfor cin_expr citer annot)

instance Analyzable AST.CompIf AST.CompIf where
  analyze (AST.CompIf expr iter annot) = do
    cexpr <- analyze expr
    citer <- analyzeMaybe iter
    return (AST.CompIf cexpr citer annot)

instance Analyzable AST.CompIter AST.CompIter where
  analyze (AST.IterFor iter annot) = do
    citer <- analyze iter
    return (AST.IterFor citer annot)
  analyze (AST.IterIf iter annot) = do
    citer <- analyze iter
    return (AST.IterIf citer annot)

instance Analyzable AST.Expr AST.Expr where
  -- TODO Differentiate refs to local vars and to outer vars
  analyze (AST.Var ident (_, annot)) = do
    cident <- analyze ident
    ref <- getVarRef $ AST.ident_string ident
    return (AST.Var cident (Just . Type $ ref, annot))
  analyze (AST.Int val lit (_, annot)) =
    return (AST.Int val lit (Just . Type . Const . CType $ Signed Int, annot))
  analyze (AST.LongInt val lit (_, annot)) =
    let typedAnnot = (Just . Type . Const . CType $ Signed Long, annot)
    in return (AST.LongInt val lit typedAnnot)
  analyze (AST.Float val lit (_, annot)) =
    let typedAnnot = (Just . Type . Const . CType $ Signed Double, annot)
    in return (AST.Float val lit typedAnnot)
  analyze (AST.Imaginary val lit annot) =
    return (AST.Imaginary val lit annot)
  analyze (AST.Bool val (_, annot)) =
    return (AST.Bool val (Just . Type . Const $ CType BInt, annot))
  analyze (AST.None annot) =
    return (AST.None annot)
  analyze (AST.Ellipsis annot) =
    return (AST.Ellipsis annot)
  analyze (AST.ByteStrings str (_, annot)) =
    return (AST.ByteStrings str (Just . Type $ Const Bytes, annot))
  analyze (AST.Strings str (_, annot)) =
    return (AST.Strings str (Just . Type $ Const String, annot))
  analyze (AST.UnicodeStrings str (_, annot)) =
    return (AST.UnicodeStrings str (Just . Type $ Const Unicode, annot))
  analyze (AST.Call fun args annot) = do
    cfun <- analyze fun
    cargs <- analyzeArray args
    return (AST.Call cfun cargs annot)
  analyze (AST.Subscript e expr annot) = do
    ce <- analyze e
    cexpr <- analyze expr
    return (AST.Subscript ce cexpr annot)
  analyze (AST.SlicedExpr expr slice annot) = do
    cexpr <- analyze expr
    cslice <- analyzeArray slice
    return (AST.SlicedExpr cexpr cslice annot)
  analyze (AST.CondExpr true cond false annot) = do
    ctrue <- analyze true
    ccond <- analyze cond
    cfalse <- analyze false
    return (AST.CondExpr ctrue ccond cfalse annot)
  analyze (AST.BinaryOp op left right annot) = do
    cop <- analyze op
    cleft <- analyze left
    cright <- analyze right
    return (AST.BinaryOp cop cleft cright annot)
  analyze (AST.UnaryOp op expr annot) = do
    cop <- analyze op
    cexpr <- analyze expr
    return (AST.UnaryOp cop cexpr annot)
  analyze (AST.Dot expr ident annot) = do
    cexpr <- analyze expr
    cident <- analyze ident
    return (AST.Dot cexpr cident annot)
  analyze (AST.Lambda args body annot) = do
    cargs <- analyzeArray args
    cbody <- analyze body
    return (AST.Lambda cargs cbody annot)
  analyze (AST.Tuple exprs annot) = do
    cexprs <- analyzeArray exprs
    return (AST.Tuple cexprs annot)
  analyze (AST.Yield arg annot) = do
    carg <- analyzeMaybe arg
    return (AST.Yield carg annot)
  analyze (AST.Generator comp annot) = do
    ccomp <- analyze comp
    return (AST.Generator ccomp annot)
  analyze (AST.ListComp comp annot) = do
    ccomp <- analyze comp
    return (AST.ListComp ccomp annot)
  analyze (AST.List exprs annot) = do
    cexprs <- analyzeArray exprs
    return (AST.List cexprs annot)
  analyze (AST.Dictionary mappings annot) = do
    cmappings <- analyzeArray mappings
    return (AST.Dictionary cmappings annot)
  analyze (AST.DictComp comp annot) = do
    ccomp <- analyze comp
    return (AST.DictComp ccomp annot)
  analyze (AST.Set exprs annot) = do
    cexprs <- analyzeArray exprs
    return (AST.Set cexprs annot)
  analyze (AST.SetComp comp annot) = do
    ccomp <- analyze comp
    return (AST.SetComp ccomp annot)
  analyze (AST.Starred expr annot) = do
    cexpr <- analyze expr
    return (AST.Starred cexpr annot)
  analyze (AST.Paren expr annot) = do
    cexpr <- analyze expr
    return (AST.Paren cexpr annot)
  analyze (AST.StringConversion expr annot) = do
    cexpr <- analyze expr
    return (AST.StringConversion cexpr annot)

instance Analyzable AST.YieldArg AST.YieldArg where
  analyze (AST.YieldFrom expr annot) = do
    cexpr <- analyze expr
    return (AST.YieldFrom cexpr annot)
  analyze (AST.YieldExpr expr) = do
    cexpr <- analyze expr
    return (AST.YieldExpr cexpr)

instance Analyzable AST.DictMappingPair AST.DictMappingPair where
  analyze (AST.DictMappingPair expr1 expr2) = do
    cexpr1 <- analyze expr1
    cexpr2 <- analyze expr2
    return (AST.DictMappingPair cexpr1 cexpr2)

instance Analyzable AST.Slice AST.Slice where
  analyze (AST.SliceProper lower upper (Just stride) annot) = do
    clower <- analyzeMaybe lower
    cupper <- analyzeMaybe upper
    cstride <- analyzeMaybe stride
    return (AST.SliceProper clower cupper (Just cstride) annot)
  analyze (AST.SliceProper lower upper Nothing annot) = do
    clower <- analyzeMaybe lower
    cupper <- analyzeMaybe upper
    return (AST.SliceProper clower cupper Nothing annot)
  analyze (AST.SliceExpr expr annot) = do
    cexpr <- analyze expr
    return (AST.SliceExpr cexpr annot)
  analyze (AST.SliceEllipsis annot) = do
    return (AST.SliceEllipsis annot)
