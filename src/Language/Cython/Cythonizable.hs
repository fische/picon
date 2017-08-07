{-# LANGUAGE DeriveDataTypeable #-}

module Language.Cython.Cythonizable (
  Cythonizable(..),
  Context(..),
  empty
) where

import qualified Data.Map.Strict as Map
import Data.Data
import Data.Bool

import Control.Monad.State (get, put, evalState)
import Control.Monad.Trans.Except

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.SrcLocation (SrcSpan(..))
import Language.Cython.Annotation
import Language.Cython.AST
import Language.Cython.Context
import Language.Cython.Error

type State annot = ContextState Context annot

-- TODO Use CythonType as annotation instead of TypeAnnotation
data Context =
  Context {
    inGlobalScope :: Bool,
    options :: Options,
    globalScope :: Map.Map String [TypeAnnotation],
    outerScope :: Map.Map String [TypeAnnotation],
    localStash :: Map.Map String [TypeAnnotation],
    localScope :: Map.Map String [TypeAnnotation]
  }
  deriving (Eq,Ord,Show,Typeable,Data)

empty :: Context
empty = Context {
  inGlobalScope = False,
  options = Options{},
  globalScope = Map.empty,
  outerScope = Map.empty,
  localStash = Map.empty,
  localScope = Map.empty
}

openBlock :: State annot Context
openBlock = do
  ctx <- get
  return ctx

openFunction :: State annot Context
openFunction = do
  ctx <- openBlock
  return $ bool ctx{
    inGlobalScope = False,
    localStash = Map.empty,
    localScope = Map.empty,
    outerScope =
      Map.unions [(localStash ctx), (localScope ctx), (outerScope ctx)]
  } ctx{
    inGlobalScope = False,
    localStash = Map.empty,
    localScope = Map.empty,
    globalScope =
      Map.unions [(localStash ctx), (localScope ctx), (globalScope ctx)]
  } (inGlobalScope ctx)

getRefTypes :: Context -> Ref -> Maybe [TypeAnnotation]
getRefTypes ctx (LocalRef ident) = Map.lookup ident (localScope ctx)
getRefTypes ctx (NonLocalRef ident) = Map.lookup ident (outerScope ctx)
getRefTypes ctx (GlobalRef ident) = Map.lookup ident (globalScope ctx)

mergeLocalScope :: Context -> Map.Map String [TypeAnnotation] -> Context
mergeLocalScope ctx locals =
  let resolvedLocals =
        evalState (mapWithKey (resolveRefs (getRefTypes ctx)) locals) Map.empty
  in ctx{
    localStash = resolvedLocals
  }

unstashLocal :: annot -> String -> State annot [TypeAnnotation]
unstashLocal loc ident = do
  ctx <- get
  let remove _ _ = Nothing
      (found, newStash) = Map.updateLookupWithKey remove ident (localStash ctx)
  maybe
    (throwE $ errVarNotFound loc ident)
    (\types -> do
      put ctx{
        localScope = Map.insert ident types (localScope ctx),
        localStash = newStash
      }
      return types)
    found



class Cythonizable t where
  cythonize :: t (Maybe CythonAnnotation, SrcSpan) ->
    State SrcSpan (t (Maybe TypeAnnotation, SrcSpan))

cythonizeArray :: (Cythonizable c) => [c (Maybe CythonAnnotation, SrcSpan)] ->
  State SrcSpan [c (Maybe TypeAnnotation, SrcSpan)]
cythonizeArray [] = return []
cythonizeArray (hd:tl) = do
  rhd <- cythonize hd
  rtl <- cythonizeArray tl
  return (rhd:rtl)

cythonizeMaybe :: (Cythonizable c) =>
  Maybe (c (Maybe CythonAnnotation, SrcSpan)) ->
  State SrcSpan (Maybe (c (Maybe TypeAnnotation, SrcSpan)))
cythonizeMaybe (Just c) = do
  rc <- cythonize c
  return (Just rc)
cythonizeMaybe Nothing = return Nothing

cythonizeGuards :: (Cythonizable c) =>
  [(c (Maybe CythonAnnotation, SrcSpan),
    Suite (Maybe CythonAnnotation, SrcSpan))]
  -> State SrcSpan
      [(c (Maybe TypeAnnotation, SrcSpan),
        Suite (Maybe TypeAnnotation, SrcSpan))]
cythonizeGuards [] = return []
cythonizeGuards ((f,s):tl) = do
  cf <- cythonize f
  ctx <- openBlock
  (cs, _) <- runState (cythonize s) ctx
  rtl <- cythonizeGuards tl
  return ((cf, cs):rtl)

cythonizePythonGuards :: (Cythonizable c) =>
  [(c (Maybe CythonAnnotation, SrcSpan),
    AST.Suite (Maybe CythonAnnotation, SrcSpan))] ->
  State SrcSpan
      [(c (Maybe TypeAnnotation, SrcSpan),
        AST.Suite (Maybe TypeAnnotation, SrcSpan))]
cythonizePythonGuards [] = return []
cythonizePythonGuards ((f,s):tl) = do
  cf <- cythonize f
  ctx <- openBlock
  (cs, _) <- runState (cythonizeArray s) ctx
  rtl <- cythonizePythonGuards tl
  return ((cf, cs):rtl)

cythonizeContext :: (Cythonizable c) =>
  [(c (Maybe CythonAnnotation, SrcSpan),
    Maybe (c (Maybe CythonAnnotation, SrcSpan)))] ->
  State SrcSpan [(c (Maybe TypeAnnotation, SrcSpan),
    Maybe (c (Maybe TypeAnnotation, SrcSpan)))]
cythonizeContext [] = return []
cythonizeContext ((f,s):tl) = do
  cf <- cythonize f
  cs <- cythonizeMaybe s
  rtl <- cythonizeContext tl
  return ((cf, cs):rtl)

instance Cythonizable Module where
  cythonize (Module stmts annot) = do
    let ctx = empty{inGlobalScope = True}
    (rstmts, _) <- runState (cythonize stmts) ctx
    return (Module rstmts (Nothing, snd annot))

instance Cythonizable Suite where
  cythonize (Suite stmts (Just (Locals locals), annot)) = do
    ctx <- get
    put $ mergeLocalScope ctx locals
    rstmts <- cythonizeArray stmts
    return (Suite rstmts (Nothing, annot))
  cythonize (Suite stmts (_, annot)) = do
    rstmts <- cythonizeArray stmts
    return (Suite rstmts (Nothing, annot))

instance Cythonizable Statement where
  cythonize (While cond body e annot) = do
    ccond <- cythonize cond
    ctx <- openBlock
    (cbody, _) <- runState (cythonize body) ctx
    (celse, _) <- runState (cythonize e) ctx
    return (While ccond cbody celse (Nothing, snd annot))
  cythonize (For targets gen body e annot) = do
    ctargets <- cythonizeArray targets
    cgen <- cythonize gen
    ctx <- openBlock
    (cbody, _) <- runState (cythonize body) ctx
    (celse, _) <- runState (cythonize e) ctx
    return (For ctargets cgen cbody celse (Nothing, snd annot))
  cythonize (Fun name args result body annot) = do
    cname <- cythonize name
    cresult <- cythonizeMaybe result
    ctx <- openFunction
    (cargs, argsctx) <- runState (cythonizeArray args) ctx
    (cbody, _) <- runState (cythonize body) argsctx
    return (Fun cname cargs cresult cbody (Nothing, snd annot))
  cythonize (Class name args body annot) = do
    cname <- cythonize name
    ctx <- openBlock
    (cargs, argsctx) <- runState (cythonizeArray args) ctx
    (cbody, _) <- runState (cythonize body) argsctx
    return (Class cname cargs cbody (Nothing, snd annot))
  cythonize (Conditional guards e annot) = do
    cguards <- cythonizeGuards guards
    ctx <- openBlock
    (celse, _) <- runState (cythonize e) ctx
    return (Conditional cguards celse (Nothing, snd annot))
  cythonize (Decorated decorators def annot) = do
    cdecorators <- cythonizeArray decorators
    cydef <- cythonize def
    return (Decorated cdecorators cydef (Nothing, snd annot))
  cythonize (Try body excepts e fin annot) = do
    ctx <- openBlock
    (cbody, _) <- runState (cythonize body) ctx
    cexcepts <- cythonizeArray excepts
    (celse, _) <- runState (cythonize e) ctx
    (cfin, _) <- runState (cythonize fin) ctx
    return (Try cbody cexcepts celse cfin (Nothing, snd annot))
  cythonize (With wctx body annot) = do
    cwctx <- cythonizeContext wctx
    ctx <- openBlock
    (cbody, _) <- runState (cythonize body) ctx
    return (With cwctx cbody (Nothing, snd annot))
  cythonize (CDef name val annot) = do
    cname <- cythonize name
    cval <- cythonizeMaybe val
    return (CDef cname cval (Nothing, snd annot))
  cythonize (Statement (AST.Assign [to@AST.Var{}] expr annot)) = do
    cexpr <- cythonize expr
    cto <- cythonize to
    ctx <- get
    let loc = snd annot
        ident = AST.ident_string $ AST.var_ident to
        cdef = do
          unstashed <- unstashLocal loc ident
          let cannot = (Just $ head unstashed, loc)
          return $ CDef (AST.var_ident cto) (Just cexpr) cannot

        reassign _ =
          let cannot = (Nothing, snd annot)
          in return $ Statement (AST.Assign [cto] cexpr cannot)

        getVarTypes (Just (Type (Ref ref))) =
          let types = getRefTypes ctx ref
          in maybe cdef reassign types
        getVarTypes _ =
          throwE $ errVariableShouldHoldRef loc ident
    getVarTypes . fst $ AST.annot to
  cythonize (Statement stmt) = do
    rstmt <- cythonize stmt
    return (Statement rstmt)

instance Cythonizable Handler where
  cythonize (Handler clause suite annot) = do
    cclause <- cythonize clause
    ctx <- openBlock
    (csuite, _) <- runState (cythonize suite) ctx
    return (Handler cclause csuite (Nothing, snd annot))

instance Cythonizable AST.Ident where
  cythonize i =
    let annot = (Nothing, snd $ AST.annot i)
    in return (i{AST.ident_annot = annot})

instance Cythonizable AST.Op where
  cythonize o =
    let annot = (Nothing, snd $ AST.annot o)
    in return (o{AST.op_annot = annot})

instance Cythonizable AST.AssignOp where
  cythonize o =
    let annot = (Nothing, snd $ AST.annot o)
    in return (o{AST.assignOp_annot = annot})

instance Cythonizable AST.Module where
  cythonize (AST.Module stmts) = do
    let ctx = empty{inGlobalScope = True}
    (rstmts, _) <- runState (cythonizeArray stmts) ctx
    return (AST.Module rstmts)

instance Cythonizable AST.ImportItem where
  cythonize (AST.ImportItem item as annot) = do
    citem <- cythonizeArray item
    cas <- cythonizeMaybe as
    return (AST.ImportItem citem cas (Nothing, snd annot))

instance Cythonizable AST.FromItem where
  cythonize (AST.FromItem item as annot) = do
    citem <- cythonize item
    cas <- cythonizeMaybe as
    return (AST.FromItem citem cas (Nothing, snd annot))

instance Cythonizable AST.FromItems where
  cythonize (AST.ImportEverything annot) =
    return (AST.ImportEverything (Nothing, snd annot))
  cythonize (AST.FromItems items annot) = do
    citems <- cythonizeArray items
    return (AST.FromItems citems (Nothing, snd annot))

instance Cythonizable AST.ImportRelative where
  cythonize (AST.ImportRelative dots (Just dotted) annot) = do
    rmod <- cythonizeArray dotted
    return (AST.ImportRelative dots (Just rmod) (Nothing, snd annot))
  cythonize (AST.ImportRelative dots Nothing annot) =
    return (AST.ImportRelative dots Nothing (Nothing, snd annot))

instance Cythonizable AST.Statement where
  cythonize (AST.Import items annot) = do
    citems <- cythonizeArray items
    return (AST.Import citems (Nothing, snd annot))
  cythonize (AST.FromImport m items annot) = do
    cm <- cythonize m
    citems <- cythonize items
    return (AST.FromImport cm citems (Nothing, snd annot))
  cythonize (AST.While cond body e annot) = do
    ccond <- cythonize cond
    ctx <- openBlock
    (cbody, _) <- runState (cythonizeArray body) ctx
    (celse, _) <- runState (cythonizeArray e) ctx
    return (AST.While ccond cbody celse (Nothing, snd annot))
  cythonize (AST.For targets gen body e annot) = do
    ctargets <- cythonizeArray targets
    cgen <- cythonize gen
    ctx <- openBlock
    (cbody, _) <- runState (cythonizeArray body) ctx
    (celse, _) <- runState (cythonizeArray e) ctx
    return (AST.For ctargets cgen cbody celse (Nothing, snd annot))
  cythonize (AST.Fun name args result body annot) = do
    cname <- cythonize name
    cresult <- cythonizeMaybe result
    ctx <- openFunction
    (cargs, argsctx) <- runState (cythonizeArray args) ctx
    (cbody, _) <- runState (cythonizeArray body) argsctx
    return (AST.Fun cname cargs cresult cbody (Nothing, snd annot))
  cythonize (AST.Class name args body annot) = do
    cname <- cythonize name
    ctx <- get
    (cargs, argsctx) <- runState (cythonizeArray args) ctx
    (cbody, _) <- runState (cythonizeArray body) argsctx
    return (AST.Class cname cargs cbody (Nothing, snd annot))
  cythonize (AST.Conditional guards e annot) = do
    cguards <- cythonizePythonGuards guards
    ctx <- openBlock
    (celse, _) <- runState (cythonizeArray e) ctx
    return (AST.Conditional cguards celse (Nothing, snd annot))
  -- TODO Handle when assign_to is an array with multiple elements
  cythonize (AST.Assign [to@AST.Var{}] expr annot) = do
    cexpr <- cythonize expr
    cto <- cythonize to
    return (AST.Assign [cto] cexpr (Nothing, snd annot))
  cythonize (AST.Assign tos expr annot) = do
    cexpr <- cythonize expr
    ctos <- cythonizeArray tos
    return (AST.Assign ctos cexpr (Nothing, snd annot))
  cythonize (AST.AugmentedAssign to op expr annot) = do
    cexpr <- cythonize expr
    cop <- cythonize op
    cto <- cythonize to
    return (AST.AugmentedAssign cto cop cexpr (Nothing, snd annot))
  cythonize (AST.Decorated decorators def annot) = do
    cdecorators <- cythonizeArray decorators
    cydef <- cythonize def
    return (AST.Decorated cdecorators cydef (Nothing, snd annot))
  cythonize (AST.Return expr annot) = do
    cexpr <- cythonizeMaybe expr
    return (AST.Return cexpr (Nothing, snd annot))
  cythonize (AST.Try body excepts e fin annot) = do
    ctx <- openBlock
    (cbody, _) <- runState (cythonizeArray body) ctx
    cexcepts <- cythonizeArray excepts
    (celse, _) <- runState (cythonizeArray e) ctx
    (cfin, _) <- runState (cythonizeArray fin) ctx
    return (AST.Try cbody cexcepts celse cfin (Nothing, snd annot))
  cythonize (AST.Raise expr annot) = do
    cexpr <- cythonize expr
    return (AST.Raise cexpr (Nothing, snd annot))
  cythonize (AST.With wctx body annot) = do
    cwctx <- cythonizeContext wctx
    ctx <- openBlock
    (cbody, _) <- runState (cythonizeArray body) ctx
    return (AST.With cwctx cbody (Nothing, snd annot))
  cythonize (AST.Pass annot) =
    return (AST.Pass (Nothing, snd annot))
  cythonize (AST.Break annot) =
    return (AST.Break (Nothing, snd annot))
  cythonize (AST.Continue annot) =
    return (AST.Continue (Nothing, snd annot))
  cythonize (AST.Delete exprs annot) = do
    cexprs <- cythonizeArray exprs
    return (AST.Delete cexprs (Nothing, snd annot))
  cythonize (AST.StmtExpr expr annot) = do
    cexpr <- cythonize expr
    return (AST.StmtExpr cexpr (Nothing, snd annot))
  cythonize (AST.Global vars annot) = do
    cvars <- cythonizeArray vars
    return (AST.Global cvars (Nothing, snd annot))
  cythonize (AST.NonLocal vars annot) = do
    cvars <- cythonizeArray vars
    return (AST.NonLocal cvars (Nothing, snd annot))
  cythonize (AST.Assert exprs annot) = do
    cexprs <- cythonizeArray exprs
    return (AST.Assert cexprs (Nothing, snd annot))
  cythonize (AST.Print chevron exprs comma annot) = do
    cexprs <- cythonizeArray exprs
    return (AST.Print chevron cexprs comma (Nothing, snd annot))
  cythonize (AST.Exec expr (Just (t1, Just t2)) annot) = do
    cexpr <- cythonize expr
    rt1 <- cythonize t1
    rt2 <- cythonize t2
    return (AST.Exec cexpr (Just (rt1, Just rt2)) (Nothing, snd annot))
  cythonize (AST.Exec expr (Just (t1, Nothing)) annot) = do
    rt1 <- cythonize t1
    cexpr <- cythonize expr
    return (AST.Exec cexpr (Just (rt1, Nothing)) (Nothing, snd annot))
  cythonize (AST.Exec expr Nothing annot) = do
    cexpr <- cythonize expr
    return (AST.Exec cexpr Nothing (Nothing, snd annot))

instance Cythonizable AST.RaiseExpr where
  cythonize (AST.RaiseV3 (Just (expr1, Just expr2))) = do
    cexpr1 <- cythonize expr1
    cexpr2 <- cythonize expr2
    return (AST.RaiseV3 (Just (cexpr1, Just cexpr2)))
  cythonize (AST.RaiseV3 (Just (expr1, Nothing))) = do
    cexpr1 <- cythonize expr1
    return (AST.RaiseV3 (Just (cexpr1, Nothing)))
  cythonize (AST.RaiseV3 Nothing) = return (AST.RaiseV3 Nothing)
  cythonize (AST.RaiseV2 (Just (expr1, Just (expr2, Just expr3)))) = do
    cexpr1 <- cythonize expr1
    cexpr2 <- cythonize expr2
    cexpr3 <- cythonize expr3
    return (AST.RaiseV2 (Just (cexpr1, Just (cexpr2, Just cexpr3))))
  cythonize (AST.RaiseV2 (Just (expr1, Just (expr2, Nothing)))) = do
    cexpr1 <- cythonize expr1
    cexpr2 <- cythonize expr2
    return (AST.RaiseV2 (Just (cexpr1, Just (cexpr2, Nothing))))
  cythonize (AST.RaiseV2 (Just (expr1, Nothing))) = do
    cexpr1 <- cythonize expr1
    return (AST.RaiseV2 (Just (cexpr1, Nothing)))
  cythonize (AST.RaiseV2 Nothing) = return (AST.RaiseV2 Nothing)

instance Cythonizable AST.Decorator where
  cythonize (AST.Decorator name args annot) = do
    cname <- cythonizeArray name
    cargs <- cythonizeArray args
    return (AST.Decorator cname cargs (Nothing, snd annot))

instance Cythonizable AST.Parameter where
  cythonize (AST.Param name py_annot dflt annot) = do
    cdflt <- cythonizeMaybe dflt
    cname <- cythonize name
    cpy_annot <- cythonizeMaybe py_annot
    return (AST.Param cname cpy_annot cdflt (Nothing, snd annot))
  cythonize (AST.VarArgsPos name py_annot annot) = do
    cname <- cythonize name
    cpy_annot <- cythonizeMaybe py_annot
    return (AST.VarArgsPos cname cpy_annot (Nothing, snd annot))
  cythonize (AST.VarArgsKeyword name py_annot annot) = do
    cname <- cythonize name
    cpy_annot <- cythonizeMaybe py_annot
    return (AST.VarArgsKeyword cname cpy_annot (Nothing, snd annot))
  cythonize (AST.EndPositional annot) =
    return (AST.EndPositional (Nothing, snd annot))
  cythonize (AST.UnPackTuple unpack dflt annot) = do
    cunpack <- cythonize unpack
    cdflt <- cythonizeMaybe dflt
    return (AST.UnPackTuple cunpack cdflt (Nothing, snd annot))

instance Cythonizable AST.ParamTuple where
  cythonize (AST.ParamTupleName name annot) = do
    cname <- cythonize name
    return (AST.ParamTupleName cname (Nothing, snd annot))
  cythonize (AST.ParamTuple tuple annot) = do
    ctuple <- cythonizeArray tuple
    return (AST.ParamTuple ctuple (Nothing, snd annot))

instance Cythonizable AST.Argument where
  cythonize (AST.ArgExpr expr annot) = do
    cexpr <- cythonize expr
    return (AST.ArgExpr cexpr (Nothing, snd annot))
  cythonize (AST.ArgVarArgsPos expr annot) = do
    cexpr <- cythonize expr
    return (AST.ArgVarArgsPos cexpr (Nothing, snd annot))
  cythonize (AST.ArgVarArgsKeyword expr annot) = do
    cexpr <- cythonize expr
    return (AST.ArgVarArgsKeyword cexpr (Nothing, snd annot))
  cythonize (AST.ArgKeyword keyword expr annot) = do
    ckeyword <- cythonize keyword
    cexpr <- cythonize expr
    return (AST.ArgKeyword ckeyword cexpr (Nothing, snd annot))

instance Cythonizable AST.Handler where
  cythonize (AST.Handler clause suite annot) = do
    cclause <- cythonize clause
    ctx <- openBlock
    (csuite, _) <- runState (cythonizeArray suite) ctx
    return (AST.Handler cclause csuite (Nothing, snd annot))

instance Cythonizable AST.ExceptClause where
  cythonize (AST.ExceptClause (Just (expr1, Just expr2)) annot) = do
    cexpr1 <- cythonize expr1
    cexpr2 <- cythonize expr2
    return (AST.ExceptClause (Just (cexpr1, Just cexpr2))
      (Nothing, snd annot))
  cythonize (AST.ExceptClause (Just (expr1, Nothing)) annot) = do
    cexpr1 <- cythonize expr1
    return (AST.ExceptClause (Just (cexpr1, Nothing))
      (Nothing, snd annot))
  cythonize (AST.ExceptClause Nothing annot) =
    return (AST.ExceptClause Nothing (Nothing, snd annot))

instance Cythonizable AST.Comprehension where
  cythonize (AST.Comprehension expr for annot) = do
    cexpr <- cythonize expr
    cfor <- cythonize for
    return (AST.Comprehension cexpr cfor (Nothing, snd annot))

instance Cythonizable AST.ComprehensionExpr where
  cythonize (AST.ComprehensionExpr expr) = do
    cexpr <- cythonize expr
    return (AST.ComprehensionExpr cexpr)
  cythonize (AST.ComprehensionDict expr) = do
    cexpr <- cythonize expr
    return (AST.ComprehensionDict cexpr)

instance Cythonizable AST.CompFor where
  cythonize (AST.CompFor for in_expr iter annot) = do
    cfor <- cythonizeArray for
    cin_expr <- cythonize in_expr
    citer <- cythonizeMaybe iter
    return (AST.CompFor cfor cin_expr citer (Nothing, snd annot))

instance Cythonizable AST.CompIf where
  cythonize (AST.CompIf expr iter annot) = do
    cexpr <- cythonize expr
    citer <- cythonizeMaybe iter
    return (AST.CompIf cexpr citer (Nothing, snd annot))

instance Cythonizable AST.CompIter where
  cythonize (AST.IterFor iter annot) = do
    citer <- cythonize iter
    return (AST.IterFor citer (Nothing, snd annot))
  cythonize (AST.IterIf iter annot) = do
    citer <- cythonize iter
    return (AST.IterIf citer (Nothing, snd annot))

instance Cythonizable AST.Expr where
  cythonize (AST.Var ident annot) = do
    cident <- cythonize ident
    return (AST.Var cident (Nothing, snd annot))
  cythonize (AST.Int val lit annot) =
    return (AST.Int val lit (Nothing, snd annot))
  cythonize (AST.LongInt val lit annot) =
    return (AST.LongInt val lit (Nothing, snd annot))
  cythonize (AST.Float val lit annot) =
    return (AST.Float val lit (Nothing, snd annot))
  cythonize (AST.Imaginary val lit annot) =
    return (AST.Imaginary val lit (Nothing, snd annot))
  cythonize (AST.Bool val annot) =
    return (AST.Bool val (Nothing, snd annot))
  cythonize (AST.None annot) =
    return (AST.None (Nothing, snd annot))
  cythonize (AST.Ellipsis annot) =
    return (AST.Ellipsis (Nothing, snd annot))
  cythonize (AST.ByteStrings str annot) =
    return (AST.ByteStrings str (Nothing, snd annot))
  cythonize (AST.Strings str annot) =
    return (AST.Strings str (Nothing, snd annot))
  cythonize (AST.UnicodeStrings str annot) =
    return (AST.UnicodeStrings str (Nothing, snd annot))
  cythonize (AST.Call fun args annot) = do
    cfun <- cythonize fun
    cargs <- cythonizeArray args
    return (AST.Call cfun cargs (Nothing, snd annot))
  cythonize (AST.Subscript e expr annot) = do
    ce <- cythonize e
    cexpr <- cythonize expr
    return (AST.Subscript ce cexpr (Nothing, snd annot))
  cythonize (AST.SlicedExpr expr slice annot) = do
    cexpr <- cythonize expr
    cslice <- cythonizeArray slice
    return (AST.SlicedExpr cexpr cslice (Nothing, snd annot))
  cythonize (AST.CondExpr true cond false annot) = do
    ctrue <- cythonize true
    ccond <- cythonize cond
    cfalse <- cythonize false
    return (AST.CondExpr ctrue ccond cfalse (Nothing, snd annot))
  cythonize (AST.BinaryOp op left right annot) = do
    cop <- cythonize op
    cleft <- cythonize left
    cright <- cythonize right
    return (AST.BinaryOp cop cleft cright (Nothing, snd annot))
  cythonize (AST.UnaryOp op expr annot) = do
    cop <- cythonize op
    cexpr <- cythonize expr
    return (AST.UnaryOp cop cexpr (Nothing, snd annot))
  cythonize (AST.Dot expr ident annot) = do
    cexpr <- cythonize expr
    cident <- cythonize ident
    return (AST.Dot cexpr cident (Nothing, snd annot))
  cythonize (AST.Lambda args body annot) = do
    cargs <- cythonizeArray args
    cbody <- cythonize body
    return (AST.Lambda cargs cbody (Nothing, snd annot))
  cythonize (AST.Tuple exprs annot) = do
    cexprs <- cythonizeArray exprs
    return (AST.Tuple cexprs (Nothing, snd annot))
  cythonize (AST.Yield arg annot) = do
    carg <- cythonizeMaybe arg
    return (AST.Yield carg (Nothing, snd annot))
  cythonize (AST.Generator comp annot) = do
    ccomp <- cythonize comp
    return (AST.Generator ccomp (Nothing, snd annot))
  cythonize (AST.ListComp comp annot) = do
    ccomp <- cythonize comp
    return (AST.ListComp ccomp (Nothing, snd annot))
  cythonize (AST.List exprs annot) = do
    cexprs <- cythonizeArray exprs
    return (AST.List cexprs (Nothing, snd annot))
  cythonize (AST.Dictionary mappings annot) = do
    cmappings <- cythonizeArray mappings
    return (AST.Dictionary cmappings (Nothing, snd annot))
  cythonize (AST.DictComp comp annot) = do
    ccomp <- cythonize comp
    return (AST.DictComp ccomp (Nothing, snd annot))
  cythonize (AST.Set exprs annot) = do
    cexprs <- cythonizeArray exprs
    return (AST.Set cexprs (Nothing, snd annot))
  cythonize (AST.SetComp comp annot) = do
    ccomp <- cythonize comp
    return (AST.SetComp ccomp (Nothing, snd annot))
  cythonize (AST.Starred expr annot) = do
    cexpr <- cythonize expr
    return (AST.Starred cexpr (Nothing, snd annot))
  cythonize (AST.Paren expr annot) = do
    cexpr <- cythonize expr
    return (AST.Paren cexpr (Nothing, snd annot))
  cythonize (AST.StringConversion expr annot) = do
    cexpr <- cythonize expr
    return (AST.StringConversion cexpr (Nothing, snd annot))

instance Cythonizable AST.YieldArg where
  cythonize (AST.YieldFrom expr annot) = do
    cexpr <- cythonize expr
    return (AST.YieldFrom cexpr (Nothing, snd annot))
  cythonize (AST.YieldExpr expr) = do
    cexpr <- cythonize expr
    return (AST.YieldExpr cexpr)

instance Cythonizable AST.DictMappingPair where
  cythonize (AST.DictMappingPair expr1 expr2) = do
    cexpr1 <- cythonize expr1
    cexpr2 <- cythonize expr2
    return (AST.DictMappingPair cexpr1 cexpr2)

instance Cythonizable AST.Slice where
  cythonize (AST.SliceProper lower upper (Just stride) annot) = do
    clower <- cythonizeMaybe lower
    cupper <- cythonizeMaybe upper
    cstride <- cythonizeMaybe stride
    return (AST.SliceProper clower cupper (Just cstride)
      (Nothing, snd annot))
  cythonize (AST.SliceProper lower upper Nothing annot) = do
    clower <- cythonizeMaybe lower
    cupper <- cythonizeMaybe upper
    return (AST.SliceProper clower cupper Nothing (Nothing, snd annot))
  cythonize (AST.SliceExpr expr annot) = do
    cexpr <- cythonize expr
    return (AST.SliceExpr cexpr (Nothing, snd annot))
  cythonize (AST.SliceEllipsis annot) = do
    return (AST.SliceEllipsis (Nothing, snd annot))
