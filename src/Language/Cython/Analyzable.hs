{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Language.Cython.Analyzable (
  Analyzable(..)
) where

import qualified Control.Monad.State as State
import Control.Monad.Trans.Except

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.SrcLocation (SrcSpan(..))
import Language.Cython.AST
import Language.Cython.Annotation
import Language.Cython.Type
import Language.Cython.Context

runState :: ContextState SrcSpan a -> Context ->
  ContextState SrcSpan (a, Context)
runState s c = do
  let (newState, newCtx) = State.runState (runExceptT s) c
  either throwE (\r -> return (r, newCtx)) newState

class Analyzable t c where
  analyze :: t (Type, SrcSpan) -> ContextState SrcSpan (c (Type, SrcSpan))

analyzeArray :: (Analyzable t c) => [t (Type, SrcSpan)] ->
  ContextState SrcSpan [c (Type, SrcSpan)]
analyzeArray [] = return []
analyzeArray (hd:tl) = do
  rhd <- analyze hd
  rtl <- analyzeArray tl
  return (rhd:rtl)

analyzeSuite :: [AST.Statement (Type, SrcSpan)] ->
  ContextState SrcSpan (Suite (Type, SrcSpan))
analyzeSuite s = do
  arr <- analyzeArray s
  return (Suite arr (None, SpanEmpty))

analyzeMaybe :: (Analyzable t c) => Maybe (t (Type, SrcSpan)) ->
  ContextState SrcSpan (Maybe (c (Type, SrcSpan)))
analyzeMaybe (Just m) = do
  r <- analyze m
  return (Just r)
analyzeMaybe Nothing = return Nothing

analyzeGuards :: (Analyzable t c) =>
  [(t (Type, SrcSpan), AST.Suite (Type, SrcSpan))] ->
  ContextState SrcSpan [(c (Type, SrcSpan), Suite (Type, SrcSpan))]
analyzeGuards [] = return []
analyzeGuards ((f,s):tl) = do
  ctx <- copy
  (cf, tmpctx1) <- runState (analyze f) ctx
  (cs, _) <- runState (analyzeSuite s) tmpctx1
  rtl <- analyzeGuards tl
  return ((cf, cs):rtl)

analyzeContext :: (Analyzable t c) =>
  [(t (Type, SrcSpan), Maybe (t (Type, SrcSpan)))] ->
  ContextState SrcSpan [(c (Type, SrcSpan), Maybe (c (Type, SrcSpan)))]
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
    (rstmts, _) <- runState (analyzeSuite stmts) ctx
    return (Module rstmts (None, SpanEmpty))

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
    ctx <- copy
    (cbody, _) <- runState (analyzeSuite body) ctx
    (celse, _) <- runState (analyzeSuite e) ctx
    return (While ccond cbody celse annot)
  analyze (AST.For targets gen body e annot) = do
    ctargets <- analyzeArray targets
    cgen <- analyze gen
    ctx <- copy
    (cbody, _) <- runState (analyzeSuite body) ctx
    (celse, _) <- runState (analyzeSuite e) ctx
    return (For ctargets cgen cbody celse annot)
  analyze (AST.Fun name args result body annot) = do
    cname <- analyze name
    ctx <- openScope
    (cargs, argsctx) <- runState (analyzeArray args) ctx
    (cbody, _) <- runState (analyzeSuite body) argsctx
    return (Fun cname cargs result cbody annot)
  analyze (AST.Class name args body annot) = do
    cname <- analyze name
    ctx <- copy
    (cargs, argsctx) <- runState (analyzeArray args) ctx
    (cbody, _) <- runState (analyzeSuite body) argsctx
    return (Class cname cargs cbody annot)
  analyze (AST.Conditional guards e annot) = do
    cguards <- analyzeGuards guards
    ctx <- copy
    (celse, _) <- runState (analyzeSuite e) ctx
    return (Conditional cguards celse annot)
  -- TODO Handle when assign_to is an array with multiple elements
  analyze (AST.Assign [to@AST.Var{}] expr annot) = do
    cexpr <- analyze expr
    cto <- analyze to
    let ident = AST.ident_string $ AST.var_ident to
        exprTyp = getType cexpr
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
    ctx <- copy
    (cbody, _) <- runState (analyzeSuite body) ctx
    cexcepts <- analyzeArray excepts
    (celse, _) <- runState (analyzeSuite e) ctx
    (cfin, _) <- runState (analyzeSuite fin) ctx
    return (Try cbody cexcepts celse cfin annot)
  analyze (AST.Raise expr annot) = do
    cexpr <- analyze expr
    return (Statement $ AST.Raise cexpr annot)
  analyze (AST.With wctx body annot) = do
    cwctx <- analyzeContext wctx
    cbody <- analyzeSuite body
    return (With cwctx cbody annot)
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
    ctx <- copy
    (cclause, _) <- runState (analyze clause) ctx
    (csuite, _) <- runState (analyzeSuite suite) ctx
    return (Handler cclause csuite annot)

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
  analyze (AST.Var ident (_, annot)) = do
    cident <- analyze ident
    return (AST.Var cident (Ref $ AST.ident_string ident, annot))
  analyze (AST.Int val lit (_, annot)) =
    return (AST.Int val lit (Const . CType $ Signed Int, annot))
  analyze (AST.LongInt val lit (_, annot)) =
    return (AST.LongInt val lit (Const . CType $ Signed Long, annot))
  analyze (AST.Float val lit (_, annot)) =
    return (AST.Float val lit (Const . CType $ Signed Double, annot))
  analyze (AST.Imaginary val lit annot) =
    return (AST.Imaginary val lit annot)
  analyze (AST.Bool val (_, annot)) =
    return (AST.Bool val (Const $ CType BInt, annot))
  analyze (AST.None annot) =
    return (AST.None annot)
  analyze (AST.Ellipsis annot) =
    return (AST.Ellipsis annot)
  analyze (AST.ByteStrings str (_, annot)) =
    return (AST.ByteStrings str (Const Bytes, annot))
  analyze (AST.Strings str (_, annot)) =
    return (AST.Strings str (Const String, annot))
  analyze (AST.UnicodeStrings str (_, annot)) =
    return (AST.UnicodeStrings str (Const Unicode, annot))
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
