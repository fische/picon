{-# LANGUAGE DeriveDataTypeable, DefaultSignatures #-}

module Language.Cython.AST where

import qualified Language.Python.Common.AST as AST
import qualified Data.Map.Strict as Map
import Data.Data

data CBasicType = Char | Short | Int | Long | LongLong | Float | Double
  deriving (Eq,Ord,Show,Typeable,Data)

data CType = Void | BInt | Signed CBasicType | Unsigned CBasicType | Ptr CType
  deriving (Eq,Ord,Show,Typeable,Data)

data CythonType = Unknown | CType CType | PythonObject
  deriving (Eq,Ord,Show,Typeable,Data)

data Annotation =
  Assign {
    cdef :: Bool,
    ctype :: CythonType
  } |
  Expr {
    ctype :: CythonType
  } |
  Empty
  deriving (Eq,Ord,Show,Typeable,Data)

data Context =
  Context {
    scope :: Map.Map String CythonType
  }
  deriving (Eq,Ord,Show,Typeable,Data)

getAnnotationType :: Annotation -> CythonType
getAnnotationType cannot
  | cannot /= Empty = ctype cannot
  | otherwise = PythonObject

emptyContext :: Context
emptyContext = Context { scope = Map.empty }

replaceIdent :: Context -> AST.Ident (Annotation, s) -> Context
replaceIdent ctx v =
  let ident = AST.ident_string v
      (annot, _) = AST.ident_annot v
      newscope = Map.insert ident (getAnnotationType annot) (scope ctx)
  in ctx { scope = newscope }

class Cythonizable t where
  cythonize :: Context -> t annot -> (Context, t (Annotation, annot))
  default cythonize :: (Functor t) => Context -> t annot
    -> (Context, t (Annotation, annot))
  cythonize ctx node = (ctx, fmap (\annot -> (Empty, annot)) node)

cythonizeArray :: (Cythonizable c) => Context -> [c s]
  -> (Context, [c (Annotation, s)])
cythonizeArray ctx [] = (ctx, [])
cythonizeArray ctx (hd:tl) =
  let (tmpctx, rhd) = cythonize ctx hd
      (rctx, rtl) = cythonizeArray tmpctx tl
  in (rctx, rhd:rtl)

cythonizeMaybe :: (Cythonizable c) => Context -> Maybe (c s)
  -> (Context, Maybe (c (Annotation, s)))
cythonizeMaybe ctx (Just c) =
  let (rctx, rc) = cythonize ctx c
  in (rctx, Just rc)
cythonizeMaybe ctx Nothing = (ctx, Nothing)

cythonizeGuards :: (Cythonizable c) => Context -> [(c s, AST.Suite s)]
  -> (Context, [(c (Annotation, s), AST.Suite (Annotation, s))])
cythonizeGuards ctx [] = (ctx, [])
cythonizeGuards ctx ((f,s):tl) =
  let (tmpctx1, cf) = cythonize ctx f
      (tmpctx2, cs) = cythonizeArray tmpctx1 s
      (rctx, rtl) = cythonizeGuards tmpctx2 tl
  in (rctx, (cf, cs):rtl)

cythonizeContext :: (Cythonizable c) => Context -> [(c s, Maybe (c s))]
  -> (Context, [(c (Annotation, s), Maybe (c (Annotation, s)))])
cythonizeContext ctx [] = (ctx, [])
cythonizeContext ctx ((f,s):tl) =
  let (tmpctx1, cf) = cythonize ctx f
      (tmpctx2, cs) = cythonizeMaybe tmpctx1 s
      (rctx, rtl) = cythonizeContext tmpctx2 tl
  in (rctx, (cf, cs):rtl)

instance Cythonizable AST.Ident

instance Cythonizable AST.Op

instance Cythonizable AST.AssignOp

instance Cythonizable AST.Module where
  cythonize ctx (AST.Module stmts) =
    let (_, rstmts) = cythonizeArray ctx stmts
    in (ctx, AST.Module(rstmts))

instance Cythonizable AST.ImportItem where
  cythonize ctx (AST.ImportItem item as annot) =
    let (_, citem) = cythonizeArray ctx item
        (_, cas) = cythonizeMaybe ctx as
    in (ctx, AST.ImportItem citem cas (Empty, annot))

instance Cythonizable AST.FromItem where
  cythonize ctx (AST.FromItem item as annot) =
    let (_, citem) = cythonize ctx item
        (_, cas) = cythonizeMaybe ctx as
    in (ctx, AST.FromItem citem cas (Empty, annot))

instance Cythonizable AST.FromItems where
  cythonize ctx (AST.ImportEverything annot) =
    (ctx, AST.ImportEverything (Empty, annot))
  cythonize ctx (AST.FromItems items annot) =
    let (_, citems) = cythonizeArray ctx items
    in (ctx, AST.FromItems citems (Empty, annot))

instance Cythonizable AST.ImportRelative where
  cythonize ctx (AST.ImportRelative dots (Just dotted) annot) =
    let (_, rmod) = cythonizeArray ctx dotted
    in (ctx, AST.ImportRelative dots (Just rmod) (Empty, annot))
  cythonize ctx (AST.ImportRelative dots Nothing annot) =
    (ctx, AST.ImportRelative dots Nothing (Empty, annot))

instance Cythonizable AST.Statement where
  cythonize ctx (AST.Import items annot) =
    let (_, citems) = cythonizeArray ctx items
    in (ctx, AST.Import citems (Empty, annot))
  cythonize ctx (AST.FromImport m items annot) =
    let (_, cm) = cythonize ctx m
        (_, citems) = cythonize ctx items
    in (ctx, AST.FromImport cm citems (Empty, annot))
  cythonize ctx (AST.While cond body e annot) =
    let (_, ccond) = cythonize ctx cond
        (_, cbody) = cythonizeArray ctx body
        (_, celse) = cythonizeArray ctx e
    in (ctx, AST.While ccond cbody celse (Empty, annot))
  cythonize ctx (AST.For targets gen body e annot) =
    let (_, ctargets) = cythonizeArray ctx targets
        (_, cgen) = cythonize ctx gen
        (_, cbody) = cythonizeArray ctx body
        (_, celse) = cythonizeArray ctx e
    in (ctx, AST.For ctargets cgen cbody celse (Empty, annot))
  cythonize ctx (AST.Fun name args result body annot) =
    let (_, cname) = cythonize ctx name
        (_, cargs) = cythonizeArray ctx args
        (_, cbody) = cythonizeArray ctx body
        (_, cresult) = cythonizeMaybe ctx result
    in (ctx, AST.Fun cname cargs cresult cbody (Empty, annot))
  cythonize ctx (AST.Class name args body annot) =
    let (_, cname) = cythonize ctx name
        (_, cargs) = cythonizeArray ctx args
        (_, cbody) = cythonizeArray ctx body
    in (ctx, AST.Class cname cargs cbody (Empty, annot))
  cythonize ctx (AST.Conditional guards e annot) =
    let (_, cguards) = cythonizeGuards ctx guards
        (_, celse) = cythonizeArray ctx e
    in (ctx, AST.Conditional cguards celse (Empty, annot))
  cythonize ctx (AST.Assign [to@AST.Var{}] expr annot) =
    let (_, cexpr) = cythonize ctx expr
        exprt = getAnnotationType . fst $ AST.annot cexpr
        cto = fmap (\s -> (Expr{ ctype = exprt }, s)) to
        cannot = Assign { cdef = True, ctype = exprt }
        rctx = replaceIdent ctx (AST.var_ident cto)
    in (rctx, AST.Assign [cto] cexpr (cannot, annot))
  cythonize ctx (AST.Assign tos expr annot) =
    let (_, cexpr) = cythonize ctx expr
        (_, ctos) = cythonizeArray ctx tos
    in (ctx, AST.Assign ctos cexpr (Empty, annot))
  cythonize ctx (AST.AugmentedAssign to op expr annot) =
    let (_, cexpr) = cythonize ctx expr
        (_, cop) = cythonize ctx op
        (_, cto) = cythonize ctx to
    in (ctx, AST.AugmentedAssign cto cop cexpr (Empty, annot))
  cythonize ctx (AST.Decorated decorators def annot) =
    let (_, cdecorators) = cythonizeArray ctx decorators
        (_, cydef) = cythonize ctx def
    in (ctx, AST.Decorated cdecorators cydef (Empty, annot))
  cythonize ctx (AST.Return expr annot) =
    let (_, cexpr) = cythonizeMaybe ctx expr
    in (ctx, AST.Return cexpr (Empty, annot))
  cythonize ctx (AST.Try body excepts e fin annot) =
    let (_, cbody) = cythonizeArray ctx body
        (_, cexcepts) = cythonizeArray ctx excepts
        (_, celse) = cythonizeArray ctx e
        (_, cfin) = cythonizeArray ctx fin
    in (ctx, AST.Try cbody cexcepts celse cfin (Empty, annot))
  cythonize ctx (AST.Raise expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.Raise cexpr (Empty, annot))
  cythonize ctx (AST.With wctx body annot) =
    let (_, cwctx) = cythonizeContext ctx wctx
        (_, cbody) = cythonizeArray ctx body
    in (ctx, AST.With cwctx cbody (Empty, annot))
  cythonize ctx (AST.Pass annot) =
    (ctx, AST.Pass (Empty, annot))
  cythonize ctx (AST.Break annot) =
    (ctx, AST.Break (Empty, annot))
  cythonize ctx (AST.Continue annot) =
    (ctx, AST.Continue (Empty, annot))
  cythonize ctx (AST.Delete exprs annot) =
    let (_, cexprs) = cythonizeArray ctx exprs
    in (ctx, AST.Delete cexprs (Empty, annot))
  cythonize ctx (AST.StmtExpr expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.StmtExpr cexpr (Empty, annot))
  cythonize ctx (AST.Global vars annot) =
    let (_, cvars) = cythonizeArray ctx vars
    in (ctx, AST.Global cvars (Empty, annot))
  cythonize ctx (AST.NonLocal vars annot) =
    let (_, cvars) = cythonizeArray ctx vars
    in (ctx, AST.NonLocal cvars (Empty, annot))
  cythonize ctx (AST.Assert exprs annot) =
    let (_, cexprs) = cythonizeArray ctx exprs
    in (ctx, AST.Assert cexprs (Empty, annot))
  cythonize ctx (AST.Print chevron exprs comma annot) =
    let (_, cexprs) = cythonizeArray ctx exprs
    in (ctx, AST.Print chevron cexprs comma (Empty, annot))
  cythonize ctx (AST.Exec expr globs annot) =
    let (_, cexpr) = cythonize ctx expr
        cglobs (Just (t1, Just t2)) = Just ((snd $ cythonize ctx t1), Just (snd $ cythonize ctx t2))
        cglobs (Just (t1, Nothing)) = Just ((snd $ cythonize ctx t1), Nothing)
        cglobs Nothing = Nothing
    in (ctx, AST.Exec cexpr (cglobs globs) (Empty, annot))

instance Cythonizable AST.RaiseExpr where
  cythonize ctx (AST.RaiseV3 (Just (expr1, Just expr2))) =
    (ctx, AST.RaiseV3 (Just ((snd $ cythonize ctx expr1), Just (snd $ cythonize ctx expr2))))
  cythonize ctx (AST.RaiseV3 (Just (expr1, Nothing))) =
    (ctx, AST.RaiseV3 (Just ((snd $ cythonize ctx expr1), Nothing)))
  cythonize ctx (AST.RaiseV3 Nothing) = (ctx, AST.RaiseV3 Nothing)
  cythonize ctx (AST.RaiseV2 (Just (expr1, Just (expr2, Just expr3)))) =
    (ctx, AST.RaiseV2 (Just ((snd $ cythonize ctx expr1), Just ((snd $ cythonize ctx expr2), Just (snd $ cythonize ctx expr3)))))
  cythonize ctx (AST.RaiseV2 (Just (expr1, Just (expr2, Nothing)))) =
    (ctx, AST.RaiseV2 (Just ((snd $ cythonize ctx expr1), Just ((snd $ cythonize ctx expr2), Nothing))))
  cythonize ctx (AST.RaiseV2 (Just (expr1, Nothing))) =
    (ctx, AST.RaiseV2 (Just ((snd $ cythonize ctx expr1), Nothing)))
  cythonize ctx (AST.RaiseV2 Nothing) = (ctx, AST.RaiseV2 Nothing)

instance Cythonizable AST.Decorator where
  cythonize ctx (AST.Decorator name args annot) =
    let (_, cname) = cythonizeArray ctx name
        (_, cargs) = cythonizeArray ctx args
    in (ctx, AST.Decorator cname cargs (Empty, annot))

instance Cythonizable AST.Parameter where
  cythonize ctx (AST.Param name py_annot dflt annot) =
    let (_, cname) = cythonize ctx name
        (_, cpy_annot) = cythonizeMaybe ctx py_annot
        (_, cdflt) = cythonizeMaybe ctx dflt
    in (ctx, AST.Param cname cpy_annot cdflt (Empty, annot))
  cythonize ctx (AST.VarArgsPos name py_annot annot) =
    let (_, cname) = cythonize ctx name
        (_, cpy_annot) = cythonizeMaybe ctx py_annot
    in (ctx, AST.VarArgsPos cname cpy_annot (Empty, annot))
  cythonize ctx (AST.VarArgsKeyword name py_annot annot) =
    let (_, cname) = cythonize ctx name
        (_, cpy_annot) = cythonizeMaybe ctx py_annot
    in (ctx, AST.VarArgsKeyword cname cpy_annot (Empty, annot))
  cythonize ctx (AST.EndPositional annot) = (ctx, AST.EndPositional (Empty, annot))
  cythonize ctx (AST.UnPackTuple unpack dflt annot) =
    let (_, cunpack) = cythonize ctx unpack
        (_, cdflt) = cythonizeMaybe ctx dflt
    in (ctx, AST.UnPackTuple cunpack cdflt (Empty, annot))

instance Cythonizable AST.ParamTuple where
  cythonize ctx (AST.ParamTupleName name annot) =
    let (_, cname) = cythonize ctx name
    in (ctx, AST.ParamTupleName cname (Empty, annot))
  cythonize ctx (AST.ParamTuple tuple annot) =
    let (_, ctuple) = cythonizeArray ctx tuple
    in (ctx, AST.ParamTuple ctuple (Empty, annot))

instance Cythonizable AST.Argument where
  cythonize ctx (AST.ArgExpr expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.ArgExpr cexpr (Empty, annot))
  cythonize ctx (AST.ArgVarArgsPos expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.ArgVarArgsPos cexpr (Empty, annot))
  cythonize ctx (AST.ArgVarArgsKeyword expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.ArgVarArgsKeyword cexpr (Empty, annot))
  cythonize ctx (AST.ArgKeyword keyword expr annot) =
    let (_, ckeyword) = cythonize ctx keyword
        (_, cexpr) = cythonize ctx expr
    in (ctx, AST.ArgKeyword ckeyword cexpr (Empty, annot))

instance Cythonizable AST.Handler where
  cythonize ctx (AST.Handler clause suite annot) =
    let (_, cclause) = cythonize ctx clause
        (_, csuite) = cythonizeArray ctx suite
    in (ctx, AST.Handler cclause csuite (Empty, annot))

instance Cythonizable AST.ExceptClause where
  cythonize ctx (AST.ExceptClause (Just (expr1, Just expr2)) annot) =
    let (_, cexpr1) = cythonize ctx expr1
        (_, cexpr2) = cythonize ctx expr2
    in (ctx, AST.ExceptClause (Just (cexpr1, Just cexpr2)) (Empty, annot))
  cythonize ctx (AST.ExceptClause (Just (expr1, Nothing)) annot) =
    let (_, cexpr1) = cythonize ctx expr1
    in (ctx, AST.ExceptClause (Just (cexpr1, Nothing)) (Empty, annot))
  cythonize ctx (AST.ExceptClause Nothing annot) =
    (ctx, AST.ExceptClause Nothing (Empty, annot))

instance Cythonizable AST.Comprehension where
  cythonize ctx (AST.Comprehension expr for annot) =
    let (_, cexpr) = cythonize ctx expr
        (_, cfor) = cythonize ctx for
    in (ctx, AST.Comprehension cexpr cfor (Empty, annot))

instance Cythonizable AST.ComprehensionExpr where
  cythonize ctx (AST.ComprehensionExpr expr) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.ComprehensionExpr cexpr)
  cythonize ctx (AST.ComprehensionDict expr) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.ComprehensionDict cexpr)

instance Cythonizable AST.CompFor where
  cythonize ctx (AST.CompFor for in_expr iter annot) =
    let (_, cfor) = cythonizeArray ctx for
        (_, cin_expr) = cythonize ctx in_expr
        (_, citer) = cythonizeMaybe ctx iter
    in (ctx, AST.CompFor cfor cin_expr citer (Empty, annot))

instance Cythonizable AST.CompIf where
  cythonize ctx (AST.CompIf expr iter annot) =
    let (_, cexpr) = cythonize ctx expr
        (_, citer) = cythonizeMaybe ctx iter
    in (ctx, AST.CompIf cexpr citer (Empty, annot))

instance Cythonizable AST.CompIter where
  cythonize ctx (AST.IterFor iter annot) =
    let (_, citer) = cythonize ctx iter
    in (ctx, AST.IterFor citer (Empty, annot))
  cythonize ctx (AST.IterIf iter annot) =
    let (_, citer) = cythonize ctx iter
    in (ctx, AST.IterIf citer (Empty, annot))

instance Cythonizable AST.Expr where
  cythonize ctx (AST.Var ident annot) =
    let (_, cident) = cythonize ctx ident
    in (ctx, AST.Var cident (Empty, annot))
  cythonize ctx (AST.Int val lit annot) =
    (ctx, AST.Int val lit (Expr . CType $ Signed Int, annot))
  cythonize ctx (AST.LongInt val lit annot) =
    (ctx, AST.LongInt val lit (Expr . CType $ Signed Long, annot))
  cythonize ctx (AST.Float val lit annot) =
    (ctx, AST.Float val lit (Expr . CType $ Signed Double, annot))
  cythonize ctx (AST.Imaginary val lit annot) =
    (ctx, AST.Imaginary val lit (Empty, annot))
  cythonize ctx (AST.Bool val annot) =
    (ctx, AST.Bool val (Expr $ CType BInt, annot))
  cythonize ctx (AST.None annot) =
    (ctx, AST.None (Empty, annot))
  cythonize ctx (AST.Ellipsis annot) =
    (ctx, AST.Ellipsis (Empty, annot))
  cythonize ctx (AST.ByteStrings str annot) =
    (ctx, AST.ByteStrings str (Empty, annot))
  cythonize ctx (AST.Strings str annot) =
    (ctx, AST.Strings str (Empty, annot))
  cythonize ctx (AST.UnicodeStrings str annot) =
    (ctx, AST.UnicodeStrings str (Empty, annot))
  cythonize ctx (AST.Call fun args annot) =
    let (_, cfun) = cythonize ctx fun
        (_, cargs) = cythonizeArray ctx args
    in (ctx, AST.Call cfun cargs (Empty, annot))
  cythonize ctx (AST.Subscript e expr annot) =
    let (_, ce) = cythonize ctx e
        (_, cexpr) = cythonize ctx expr
    in (ctx, AST.Subscript ce cexpr (Empty, annot))
  cythonize ctx (AST.SlicedExpr expr slice annot) =
    let (_, cexpr) = cythonize ctx expr
        (_, cslice) = cythonizeArray ctx slice
    in (ctx, AST.SlicedExpr cexpr cslice (Empty, annot))
  cythonize ctx (AST.CondExpr true cond false annot) =
    let (_, ctrue) = cythonize ctx true
        (_, ccond) = cythonize ctx cond
        (_, cfalse) = cythonize ctx false
    in (ctx, AST.CondExpr ctrue ccond cfalse (Empty, annot))
  cythonize ctx (AST.BinaryOp op left right annot) =
    let (_, cop) = cythonize ctx op
        (_, cleft) = cythonize ctx left
        (_, cright) = cythonize ctx right
    in (ctx, AST.BinaryOp cop cleft cright (Empty, annot))
  cythonize ctx (AST.UnaryOp op expr annot) =
    let (_, cop) = cythonize ctx op
        (_, cexpr) = cythonize ctx expr
    in (ctx, AST.UnaryOp cop cexpr (Empty, annot))
  cythonize ctx (AST.Dot expr ident annot) =
    let (_, cexpr) = cythonize ctx expr
        (_, cident) = cythonize ctx ident
    in (ctx, AST.Dot cexpr cident (Empty, annot))
  cythonize ctx (AST.Lambda args body annot) =
    let (_, cargs) = cythonizeArray ctx args
        (_, cbody) = cythonize ctx body
    in (ctx, AST.Lambda cargs cbody (Empty, annot))
  cythonize ctx (AST.Tuple exprs annot) =
    let (_, cexprs) = cythonizeArray ctx exprs
    in (ctx, AST.Tuple cexprs (Empty, annot))
  cythonize ctx (AST.Yield arg annot) =
    let (_, carg) = cythonizeMaybe ctx arg
    in (ctx, AST.Yield carg (Empty, annot))
  cythonize ctx (AST.Generator comp annot) =
    let (_, ccomp) = cythonize ctx comp
    in (ctx, AST.Generator ccomp (Empty, annot))
  cythonize ctx (AST.ListComp comp annot) =
    let (_, ccomp) = cythonize ctx comp
    in (ctx, AST.ListComp ccomp (Empty, annot))
  cythonize ctx (AST.List exprs annot) =
    let (_, cexprs) = cythonizeArray ctx exprs
    in (ctx, AST.List cexprs (Empty, annot))
  cythonize ctx (AST.Dictionary mappings annot) =
    let (_, cmappings) = cythonizeArray ctx mappings
    in (ctx, AST.Dictionary cmappings (Empty, annot))
  cythonize ctx (AST.DictComp comp annot) =
    let (_, ccomp) = cythonize ctx comp
    in (ctx, AST.DictComp ccomp (Empty, annot))
  cythonize ctx (AST.Set exprs annot) =
    let (_, cexprs) = cythonizeArray ctx exprs
    in (ctx, AST.Set cexprs (Empty, annot))
  cythonize ctx (AST.SetComp comp annot) =
    let (_, ccomp) = cythonize ctx comp
    in (ctx, AST.SetComp ccomp (Empty, annot))
  cythonize ctx (AST.Starred expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.Starred cexpr (Empty, annot))
  cythonize ctx (AST.Paren expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.Paren cexpr (Empty, annot))
  cythonize ctx (AST.StringConversion expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.StringConversion cexpr (Empty, annot))

instance Cythonizable AST.YieldArg where
  cythonize ctx (AST.YieldFrom expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.YieldFrom cexpr (Empty, annot))
  cythonize ctx (AST.YieldExpr expr) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.YieldExpr cexpr)

instance Cythonizable AST.DictMappingPair where
  cythonize ctx (AST.DictMappingPair expr1 expr2) =
    let (_, cexpr1) = cythonize ctx expr1
        (_, cexpr2) = cythonize ctx expr2
    in (ctx, AST.DictMappingPair cexpr1 cexpr2)

instance Cythonizable AST.Slice where
  cythonize ctx (AST.SliceProper lower upper stride annot) =
    let (_, clower) = cythonizeMaybe ctx lower
        (_, cupper) = cythonizeMaybe ctx upper
        cstride (Just t) = Just (snd $ cythonizeMaybe ctx t)
        cstride Nothing = Nothing
    in (ctx, AST.SliceProper clower cupper (cstride stride) (Empty, annot))
  cythonize ctx (AST.SliceExpr expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.SliceExpr cexpr (Empty, annot))
  cythonize ctx (AST.SliceEllipsis annot) =
    (ctx, AST.SliceEllipsis (Empty, annot))
