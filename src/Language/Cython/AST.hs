{-# LANGUAGE DefaultSignatures #-}

module Language.Cython.AST (
  Cythonizable(..),
  initCythonAST
) where

import qualified Language.Python.Common.AST as AST
import qualified Control.Monad.State as State
import Control.Monad.Trans.Except
import Language.Cython.Annotation
import Language.Cython.Context

runState :: ContextState a -> Context -> ContextState (a, Context)
runState s c = do
  let (newState, newCtx) = State.runState (runExceptT s) c
  either throwE (\r -> return (r, newCtx)) newState

initCythonAST :: (Functor f) => f annot -> f (Annotation, annot)
initCythonAST = fmap (\s -> (Empty, s))

class Cythonizable t where
  cythonize :: t (Annotation, annot)
    -> ContextState (t (Annotation, annot))
  default cythonize :: t (Annotation, annot)
    -> ContextState (t (Annotation, annot))
  cythonize node = return node

cythonizeArray :: (Cythonizable c) => [c (Annotation, s)]
  -> ContextState [c (Annotation, s)]
cythonizeArray [] = return []
cythonizeArray (hd:tl) = do
  rhd <- cythonize hd
  rtl <- cythonizeArray tl
  return (rhd:rtl)

cythonizeMaybe :: (Cythonizable c) => Maybe (c (Annotation, s))
  -> ContextState (Maybe (c (Annotation, s)))
cythonizeMaybe (Just c) = do
  rc <- cythonize c
  return (Just rc)
cythonizeMaybe Nothing = return Nothing

cythonizeGuards :: (Cythonizable c) =>
  [(c (Annotation, s), AST.Suite (Annotation, s))]
  -> ContextState [(c (Annotation, s), AST.Suite (Annotation, s))]
cythonizeGuards [] = return []
cythonizeGuards ((f,s):tl) = do
  ctx <- copyContext
  (cf, tmpctx1) <- runState (cythonize f) ctx
  (cs, rctx) <- runState (cythonizeArray s) tmpctx1
  _ <- mergeCopiedContext rctx
  rtl <- cythonizeGuards tl
  return ((cf, cs):rtl)

cythonizeContext :: (Cythonizable c) =>
  [(c (Annotation, s), Maybe (c (Annotation, s)))]
  -> ContextState [(c (Annotation, s), Maybe (c (Annotation, s)))]
cythonizeContext [] = return []
cythonizeContext ((f,s):tl) = do
  cf <- cythonize f
  cs <- cythonizeMaybe s
  rtl <- cythonizeContext tl
  return ((cf, cs):rtl)

instance Cythonizable AST.Ident where
  cythonize (AST.Ident ident (_, annot)) = do
    typ <- getVarType ident
    return (AST.Ident ident (Type typ, annot))

instance Cythonizable AST.Op

instance Cythonizable AST.AssignOp

instance Cythonizable AST.Module where
  cythonize (AST.Module stmts) = do
    ctx <- copyContext
    let rctx = ctx{inGlobalScope = True}
    (rstmts, _) <- runState (cythonizeArray stmts) rctx
    return (AST.Module(rstmts))

instance Cythonizable AST.ImportItem where
  cythonize (AST.ImportItem item as annot) = do
    citem <- cythonizeArray item
    cas <- cythonizeMaybe as
    return (AST.ImportItem citem cas annot)

instance Cythonizable AST.FromItem where
  cythonize (AST.FromItem item as annot) = do
    citem <- cythonize item
    cas <- cythonizeMaybe as
    return (AST.FromItem citem cas annot)

instance Cythonizable AST.FromItems where
  cythonize (AST.ImportEverything annot) =
    return (AST.ImportEverything annot)
  cythonize (AST.FromItems items annot) = do
    citems <- cythonizeArray items
    return (AST.FromItems citems annot)

instance Cythonizable AST.ImportRelative where
  cythonize (AST.ImportRelative dots (Just dotted) annot) = do
    rmod <- cythonizeArray dotted
    return (AST.ImportRelative dots (Just rmod) annot)
  cythonize (AST.ImportRelative dots Nothing annot) =
    return (AST.ImportRelative dots Nothing annot)

instance Cythonizable AST.Statement where
  cythonize (AST.Import items annot) = do
    citems <- cythonizeArray items
    return (AST.Import citems annot)
  cythonize (AST.FromImport m items annot) = do
    cm <- cythonize m
    citems <- cythonize items
    return (AST.FromImport cm citems annot)
  cythonize (AST.While cond body e annot) = do
    ccond <- cythonize cond
    ctx <- copyContext
    (cbody, tmpctx1) <- runState (cythonizeArray body) ctx
    tmpctx2 <- mergeCopiedContext tmpctx1
    (celse, rctx) <- runState (cythonizeArray e) tmpctx2
    _ <- mergeCopiedContext rctx
    return (AST.While ccond cbody celse annot)
  cythonize (AST.For targets gen body e annot) = do
    ctargets <- cythonizeArray targets
    cgen <- cythonize gen
    ctx <- copyContext
    (cbody, tmpctx1) <- runState (cythonizeArray body) ctx
    tmpctx2 <- mergeCopiedContext tmpctx1
    (celse, rctx) <- runState (cythonizeArray e) tmpctx2
    _ <- mergeCopiedContext rctx
    return (AST.For ctargets cgen cbody celse annot)
  cythonize (AST.Fun name args result body annot) = do
    cname <- cythonize name
    ctx <- openNewContext
    (cargs, argsctx) <- runState (cythonizeArray args) ctx
    (cbody, rctx) <- runState (cythonizeArray body) argsctx
    _ <- mergeInnerContext rctx
    return (AST.Fun cname cargs result cbody annot)
  cythonize (AST.Class name args body annot) = do
    cname <- cythonize name
    ctx <- openNewContext
    (cargs, argsctx) <- runState (cythonizeArray args) ctx
    (cbody, rctx) <- runState (cythonizeArray body) argsctx
    _ <- mergeInnerContext rctx
    return (AST.Class cname cargs cbody annot)
  cythonize (AST.Conditional guards e annot) = do
    cguards <- cythonizeGuards guards
    ctx <- copyContext
    (celse, rctx) <- runState (cythonizeArray e) ctx
    _ <- mergeCopiedContext rctx
    return (AST.Conditional cguards celse annot)
  cythonize (AST.Assign [to@AST.Var{}] expr (_, annot)) = do
    cexpr <- cythonize expr
    let ident = AST.ident_string $ AST.var_ident to
        typ = getAnnotationType . fst $ AST.annot cexpr
    rdef <- assignVar ident typ
    cto <- cythonize to
    return (AST.Assign [cto] cexpr (CDef rdef, annot))
  cythonize (AST.Assign tos expr annot) = do
    cexpr <- cythonize expr
    ctos <- cythonizeArray tos
    return (AST.Assign ctos cexpr annot)
  cythonize (AST.AugmentedAssign to op expr annot) = do
    cexpr <- cythonize expr
    cop <- cythonize op
    cto <- cythonize to
    return (AST.AugmentedAssign cto cop cexpr annot)
  cythonize (AST.Decorated decorators def annot) = do
    cdecorators <- cythonizeArray decorators
    cydef <- cythonize def
    return (AST.Decorated cdecorators cydef annot)
  cythonize (AST.Return expr annot) = do
    cexpr <- cythonizeMaybe expr
    return (AST.Return cexpr annot)
  cythonize (AST.Try body excepts e fin annot) = do
    ctx <- copyContext
    (cbody, tmpctx1) <- runState (cythonizeArray body) ctx
    tmpctx2 <- mergeCopiedContext tmpctx1
    cexcepts <- cythonizeArray excepts
    (celse, tmpctx3) <- runState (cythonizeArray e) tmpctx2
    tmpctx4 <- mergeCopiedContext tmpctx3
    (cfin, rctx) <- runState (cythonizeArray fin) tmpctx4
    _ <- mergeCopiedContext rctx
    return (AST.Try cbody cexcepts celse cfin annot)
  cythonize (AST.Raise expr annot) = do
    cexpr <- cythonize expr
    return (AST.Raise cexpr annot)
  cythonize (AST.With wctx body annot) = do
    cwctx <- cythonizeContext wctx
    cbody <- cythonizeArray body
    return (AST.With cwctx cbody annot)
  cythonize (AST.Pass annot) = return (AST.Pass annot)
  cythonize (AST.Break annot) = return (AST.Break annot)
  cythonize (AST.Continue annot) = return (AST.Continue annot)
  cythonize (AST.Delete exprs annot) = do
    cexprs <- cythonizeArray exprs
    return (AST.Delete cexprs annot)
  cythonize (AST.StmtExpr expr annot) = do
    cexpr <- cythonize expr
    return (AST.StmtExpr cexpr annot)
  cythonize (AST.Global vars annot) = do
    bindGlobalVars (fmap AST.ident_string vars)
    cvars <- cythonizeArray vars
    return (AST.Global cvars annot)
  cythonize (AST.NonLocal vars annot) = do
    bindNonLocalVars (fmap AST.ident_string vars)
    cvars <- cythonizeArray vars
    return (AST.NonLocal cvars annot)
  cythonize (AST.Assert exprs annot) = do
    cexprs <- cythonizeArray exprs
    return (AST.Assert cexprs annot)
  cythonize (AST.Print chevron exprs comma annot) = do
    cexprs <- cythonizeArray exprs
    return (AST.Print chevron cexprs comma annot)
  cythonize (AST.Exec expr (Just (t1, Just t2)) annot) = do
    cexpr <- cythonize expr
    rt1 <- cythonize t1
    rt2 <- cythonize t2
    return (AST.Exec cexpr (Just (rt1, Just rt2)) annot)
  cythonize (AST.Exec expr (Just (t1, Nothing)) annot) = do
    rt1 <- cythonize t1
    cexpr <- cythonize expr
    return (AST.Exec cexpr (Just (rt1, Nothing)) annot)
  cythonize (AST.Exec expr Nothing annot) = do
    cexpr <- cythonize expr
    return (AST.Exec cexpr Nothing annot)

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
    return (AST.Decorator cname cargs annot)

instance Cythonizable AST.Parameter where
  cythonize (AST.Param name py_annot dflt (_, annot)) = do
    cdflt <- cythonizeMaybe dflt
    let ident = AST.ident_string name
        typ = case cdflt of
                Nothing -> Unknown
                Just expr -> getAnnotationType . fst $ AST.annot expr
    insertVar ident typ
    cname <- cythonize name
    return (AST.Param cname py_annot cdflt (Type typ, annot))
  cythonize (AST.VarArgsPos name py_annot annot) = do
    cname <- cythonize name
    return (AST.VarArgsPos cname py_annot annot)
  cythonize (AST.VarArgsKeyword name py_annot annot) = do
    cname <- cythonize name
    return (AST.VarArgsKeyword cname py_annot annot)
  cythonize (AST.EndPositional annot) = return (AST.EndPositional annot)
  cythonize (AST.UnPackTuple unpack dflt annot) = do
    cunpack <- cythonize unpack
    cdflt <- cythonizeMaybe dflt
    return (AST.UnPackTuple cunpack cdflt annot)

instance Cythonizable AST.ParamTuple where
  cythonize (AST.ParamTupleName name annot) = do
    cname <- cythonize name
    return (AST.ParamTupleName cname annot)
  cythonize (AST.ParamTuple tuple annot) = do
    ctuple <- cythonizeArray tuple
    return (AST.ParamTuple ctuple annot)

instance Cythonizable AST.Argument where
  cythonize (AST.ArgExpr expr annot) = do
    cexpr <- cythonize expr
    return (AST.ArgExpr cexpr annot)
  cythonize (AST.ArgVarArgsPos expr annot) = do
    cexpr <- cythonize expr
    return (AST.ArgVarArgsPos cexpr annot)
  cythonize (AST.ArgVarArgsKeyword expr annot) = do
    cexpr <- cythonize expr
    return (AST.ArgVarArgsKeyword cexpr annot)
  cythonize (AST.ArgKeyword keyword expr annot) = do
    ckeyword <- cythonize keyword
    cexpr <- cythonize expr
    return (AST.ArgKeyword ckeyword cexpr annot)

instance Cythonizable AST.Handler where
  cythonize (AST.Handler clause suite annot) = do
    ctx <- copyContext
    (cclause, tmpctx1) <- runState (cythonize clause) ctx
    tmpctx2 <- mergeCopiedContext tmpctx1
    (csuite, rctx) <- runState (cythonizeArray suite) tmpctx2
    _ <- mergeCopiedContext rctx
    return (AST.Handler cclause csuite annot)

instance Cythonizable AST.ExceptClause where
  cythonize (AST.ExceptClause (Just (expr1, Just expr2)) annot) = do
    cexpr1 <- cythonize expr1
    cexpr2 <- cythonize expr2
    return (AST.ExceptClause (Just (cexpr1, Just cexpr2)) annot)
  cythonize (AST.ExceptClause (Just (expr1, Nothing)) annot) = do
    cexpr1 <- cythonize expr1
    return (AST.ExceptClause (Just (cexpr1, Nothing)) annot)
  cythonize (AST.ExceptClause Nothing annot) =
    return (AST.ExceptClause Nothing annot)

instance Cythonizable AST.Comprehension where
  cythonize (AST.Comprehension expr for annot) = do
    cexpr <- cythonize expr
    cfor <- cythonize for
    return (AST.Comprehension cexpr cfor annot)

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
    return (AST.CompFor cfor cin_expr citer annot)

instance Cythonizable AST.CompIf where
  cythonize (AST.CompIf expr iter annot) = do
    cexpr <- cythonize expr
    citer <- cythonizeMaybe iter
    return (AST.CompIf cexpr citer annot)

instance Cythonizable AST.CompIter where
  cythonize (AST.IterFor iter annot) = do
    citer <- cythonize iter
    return (AST.IterFor citer annot)
  cythonize (AST.IterIf iter annot) = do
    citer <- cythonize iter
    return (AST.IterIf citer annot)

instance Cythonizable AST.Expr where
  cythonize (AST.Var ident (_, annot)) = do
    cident <- cythonize ident
    let (typ, _) = AST.annot cident
    return (AST.Var cident (typ, annot))
  cythonize (AST.Int val lit (_, annot)) =
    return (AST.Int val lit (Type . CType $ Signed Int, annot))
  cythonize (AST.LongInt val lit (_, annot)) =
    return (AST.LongInt val lit (Type . CType $ Signed Long, annot))
  cythonize (AST.Float val lit (_, annot)) =
    return (AST.Float val lit (Type . CType $ Signed Double, annot))
  cythonize (AST.Imaginary val lit annot) =
    return (AST.Imaginary val lit annot)
  cythonize (AST.Bool val (_, annot)) =
    return (AST.Bool val (Type $ CType BInt, annot))
  cythonize (AST.None annot) =
    return (AST.None annot)
  cythonize (AST.Ellipsis annot) =
    return (AST.Ellipsis annot)
  cythonize (AST.ByteStrings str (_, annot)) =
    return (AST.ByteStrings str (Type Bytes, annot))
  cythonize (AST.Strings str (_, annot)) =
    return (AST.Strings str (Type String, annot))
  cythonize (AST.UnicodeStrings str (_, annot)) =
    return (AST.UnicodeStrings str (Type Unicode, annot))
  cythonize (AST.Call fun args annot) = do
    cfun <- cythonize fun
    cargs <- cythonizeArray args
    return (AST.Call cfun cargs annot)
  cythonize (AST.Subscript e expr annot) = do
    ce <- cythonize e
    cexpr <- cythonize expr
    return (AST.Subscript ce cexpr annot)
  cythonize (AST.SlicedExpr expr slice annot) = do
    cexpr <- cythonize expr
    cslice <- cythonizeArray slice
    return (AST.SlicedExpr cexpr cslice annot)
  cythonize (AST.CondExpr true cond false annot) = do
    ctrue <- cythonize true
    ccond <- cythonize cond
    cfalse <- cythonize false
    return (AST.CondExpr ctrue ccond cfalse annot)
  cythonize (AST.BinaryOp op left right annot) = do
    cop <- cythonize op
    cleft <- cythonize left
    cright <- cythonize right
    return (AST.BinaryOp cop cleft cright annot)
  cythonize (AST.UnaryOp op expr annot) = do
    cop <- cythonize op
    cexpr <- cythonize expr
    return (AST.UnaryOp cop cexpr annot)
  cythonize (AST.Dot expr ident annot) = do
    cexpr <- cythonize expr
    cident <- cythonize ident
    return (AST.Dot cexpr cident annot)
  cythonize (AST.Lambda args body annot) = do
    cargs <- cythonizeArray args
    cbody <- cythonize body
    return (AST.Lambda cargs cbody annot)
  cythonize (AST.Tuple exprs annot) = do
    cexprs <- cythonizeArray exprs
    return (AST.Tuple cexprs annot)
  cythonize (AST.Yield arg annot) = do
    carg <- cythonizeMaybe arg
    return (AST.Yield carg annot)
  cythonize (AST.Generator comp annot) = do
    ccomp <- cythonize comp
    return (AST.Generator ccomp annot)
  cythonize (AST.ListComp comp annot) = do
    ccomp <- cythonize comp
    return (AST.ListComp ccomp annot)
  cythonize (AST.List exprs annot) = do
    cexprs <- cythonizeArray exprs
    return (AST.List cexprs annot)
  cythonize (AST.Dictionary mappings annot) = do
    cmappings <- cythonizeArray mappings
    return (AST.Dictionary cmappings annot)
  cythonize (AST.DictComp comp annot) = do
    ccomp <- cythonize comp
    return (AST.DictComp ccomp annot)
  cythonize (AST.Set exprs annot) = do
    cexprs <- cythonizeArray exprs
    return (AST.Set cexprs annot)
  cythonize (AST.SetComp comp annot) = do
    ccomp <- cythonize comp
    return (AST.SetComp ccomp annot)
  cythonize (AST.Starred expr annot) = do
    cexpr <- cythonize expr
    return (AST.Starred cexpr annot)
  cythonize (AST.Paren expr annot) = do
    cexpr <- cythonize expr
    return (AST.Paren cexpr annot)
  cythonize (AST.StringConversion expr annot) = do
    cexpr <- cythonize expr
    return (AST.StringConversion cexpr annot)

instance Cythonizable AST.YieldArg where
  cythonize (AST.YieldFrom expr annot) = do
    cexpr <- cythonize expr
    return (AST.YieldFrom cexpr annot)
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
    return (AST.SliceProper clower cupper (Just cstride) annot)
  cythonize (AST.SliceProper lower upper Nothing annot) = do
    clower <- cythonizeMaybe lower
    cupper <- cythonizeMaybe upper
    return (AST.SliceProper clower cupper Nothing annot)
  cythonize (AST.SliceExpr expr annot) = do
    cexpr <- cythonize expr
    return (AST.SliceExpr cexpr annot)
  cythonize (AST.SliceEllipsis annot) = do
    return (AST.SliceEllipsis annot)
