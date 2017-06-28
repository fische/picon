{-# LANGUAGE DeriveDataTypeable, DefaultSignatures #-}

module Language.Cython.AST where

import qualified Language.Python.Common.AST as AST
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
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

getAnnotationType :: Annotation -> CythonType
getAnnotationType cannot
  | cannot /= Empty = ctype cannot
  | otherwise = PythonObject

initCythonAST :: (Functor f) => f annot -> f (Annotation, annot)
initCythonAST = fmap (\s -> (Empty, s))

data Context =
  Context {
    scope :: Map.Map String CythonType
  }
  deriving (Eq,Ord,Show,Typeable,Data)

emptyContext :: Context
emptyContext = Context { scope = Map.empty }

replaceIdent :: Context -> String -> CythonType -> Context
replaceIdent ctx ident typ =
  ctx { scope = Map.insert ident typ (scope ctx) }

-- TODO Compare CTypes, change if needed and return it
assignIdent :: Context -> String -> CythonType
  -> (Context, Bool, CythonType)
assignIdent ctx ident typ =
  let (old, newscope) =
        Map.insertLookupWithKey (\_ _ new -> new) ident typ (scope ctx)
  in (ctx { scope = newscope }, isNothing old, typ)

class Cythonizable t where
  cythonize :: Context -> t (Annotation, annot)
    -> (Context, t (Annotation, annot))
  default cythonize :: Context -> t (Annotation, annot)
    -> (Context, t (Annotation, annot))
  cythonize ctx node = (ctx, node)

cythonizeArray :: (Cythonizable c) => Context -> [c (Annotation, s)]
  -> (Context, [c (Annotation, s)])
cythonizeArray ctx [] = (ctx, [])
cythonizeArray ctx (hd:tl) =
  let (tmpctx, rhd) = cythonize ctx hd
      (rctx, rtl) = cythonizeArray tmpctx tl
  in (rctx, rhd:rtl)

cythonizeMaybe :: (Cythonizable c) => Context -> Maybe (c (Annotation, s))
  -> (Context, Maybe (c (Annotation, s)))
cythonizeMaybe ctx (Just c) =
  let (rctx, rc) = cythonize ctx c
  in (rctx, Just rc)
cythonizeMaybe ctx Nothing = (ctx, Nothing)

cythonizeGuards :: (Cythonizable c) => Context
  -> [(c (Annotation, s), AST.Suite (Annotation, s))]
  -> (Context, [(c (Annotation, s), AST.Suite (Annotation, s))])
cythonizeGuards ctx [] = (ctx, [])
cythonizeGuards ctx ((f,s):tl) =
  let (tmpctx1, cf) = cythonize ctx f
      (tmpctx2, cs) = cythonizeArray tmpctx1 s
      (rctx, rtl) = cythonizeGuards tmpctx2 tl
  in (rctx, (cf, cs):rtl)

cythonizeContext :: (Cythonizable c) => Context
  -> [(c (Annotation, s), Maybe (c (Annotation, s)))]
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
    in (ctx, AST.ImportItem citem cas annot)

instance Cythonizable AST.FromItem where
  cythonize ctx (AST.FromItem item as annot) =
    let (_, citem) = cythonize ctx item
        (_, cas) = cythonizeMaybe ctx as
    in (ctx, AST.FromItem citem cas annot)

instance Cythonizable AST.FromItems where
  cythonize ctx (AST.ImportEverything annot) =
    (ctx, AST.ImportEverything annot)
  cythonize ctx (AST.FromItems items annot) =
    let (_, citems) = cythonizeArray ctx items
    in (ctx, AST.FromItems citems annot)

instance Cythonizable AST.ImportRelative where
  cythonize ctx (AST.ImportRelative dots (Just dotted) annot) =
    let (_, rmod) = cythonizeArray ctx dotted
    in (ctx, AST.ImportRelative dots (Just rmod) annot)
  cythonize ctx (AST.ImportRelative dots Nothing annot) =
    (ctx, AST.ImportRelative dots Nothing annot)

instance Cythonizable AST.Statement where
  cythonize ctx (AST.Import items annot) =
    let (_, citems) = cythonizeArray ctx items
    in (ctx, AST.Import citems annot)
  cythonize ctx (AST.FromImport m items annot) =
    let (_, cm) = cythonize ctx m
        (_, citems) = cythonize ctx items
    in (ctx, AST.FromImport cm citems annot)
  cythonize ctx (AST.While cond body e annot) =
    let (_, ccond) = cythonize ctx cond
        (_, cbody) = cythonizeArray ctx body
        (_, celse) = cythonizeArray ctx e
    in (ctx, AST.While ccond cbody celse annot)
  cythonize ctx (AST.For targets gen body e annot) =
    let (_, ctargets) = cythonizeArray ctx targets
        (_, cgen) = cythonize ctx gen
        (_, cbody) = cythonizeArray ctx body
        (_, celse) = cythonizeArray ctx e
    in (ctx, AST.For ctargets cgen cbody celse annot)
  cythonize ctx (AST.Fun name args result body annot) =
    let (_, cname) = cythonize ctx name
        (_, cargs) = cythonizeArray ctx args
        (_, cbody) = cythonizeArray ctx body
        (_, cresult) = cythonizeMaybe ctx result
    in (ctx, AST.Fun cname cargs cresult cbody annot)
  cythonize ctx (AST.Class name args body annot) =
    let (_, cname) = cythonize ctx name
        (_, cargs) = cythonizeArray ctx args
        (_, cbody) = cythonizeArray ctx body
    in (ctx, AST.Class cname cargs cbody annot)
  cythonize ctx (AST.Conditional guards e annot) =
    let (_, cguards) = cythonizeGuards ctx guards
        (_, celse) = cythonizeArray ctx e
    in (ctx, AST.Conditional cguards celse annot)
  cythonize ctx (AST.Assign [to@AST.Var{}] expr (_, annot)) =
    let (_, cexpr) = cythonize ctx expr
        ident = AST.ident_string $ AST.var_ident cto
        typ = getAnnotationType . fst $ AST.annot cexpr
        (rctx, rdef, rtyp) = assignIdent ctx ident typ
        cto = fmap (\(_, s) -> (Expr{ ctype = rtyp }, s)) to
        cannot = Assign { cdef = rdef, ctype = rtyp }
    in (rctx, AST.Assign [cto] cexpr (cannot, annot))
  cythonize ctx (AST.Assign tos expr annot) =
    let (_, cexpr) = cythonize ctx expr
        (_, ctos) = cythonizeArray ctx tos
    in (ctx, AST.Assign ctos cexpr annot)
  cythonize ctx (AST.AugmentedAssign to op expr annot) =
    let (_, cexpr) = cythonize ctx expr
        (_, cop) = cythonize ctx op
        (_, cto) = cythonize ctx to
    in (ctx, AST.AugmentedAssign cto cop cexpr annot)
  cythonize ctx (AST.Decorated decorators def annot) =
    let (_, cdecorators) = cythonizeArray ctx decorators
        (_, cydef) = cythonize ctx def
    in (ctx, AST.Decorated cdecorators cydef annot)
  cythonize ctx (AST.Return expr annot) =
    let (_, cexpr) = cythonizeMaybe ctx expr
    in (ctx, AST.Return cexpr annot)
  cythonize ctx (AST.Try body excepts e fin annot) =
    let (_, cbody) = cythonizeArray ctx body
        (_, cexcepts) = cythonizeArray ctx excepts
        (_, celse) = cythonizeArray ctx e
        (_, cfin) = cythonizeArray ctx fin
    in (ctx, AST.Try cbody cexcepts celse cfin annot)
  cythonize ctx (AST.Raise expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.Raise cexpr annot)
  cythonize ctx (AST.With wctx body annot) =
    let (_, cwctx) = cythonizeContext ctx wctx
        (_, cbody) = cythonizeArray ctx body
    in (ctx, AST.With cwctx cbody annot)
  cythonize ctx (AST.Pass annot) =
    (ctx, AST.Pass annot)
  cythonize ctx (AST.Break annot) =
    (ctx, AST.Break annot)
  cythonize ctx (AST.Continue annot) =
    (ctx, AST.Continue annot)
  cythonize ctx (AST.Delete exprs annot) =
    let (_, cexprs) = cythonizeArray ctx exprs
    in (ctx, AST.Delete cexprs annot)
  cythonize ctx (AST.StmtExpr expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.StmtExpr cexpr annot)
  cythonize ctx (AST.Global vars annot) =
    let (_, cvars) = cythonizeArray ctx vars
    in (ctx, AST.Global cvars annot)
  cythonize ctx (AST.NonLocal vars annot) =
    let (_, cvars) = cythonizeArray ctx vars
    in (ctx, AST.NonLocal cvars annot)
  cythonize ctx (AST.Assert exprs annot) =
    let (_, cexprs) = cythonizeArray ctx exprs
    in (ctx, AST.Assert cexprs annot)
  cythonize ctx (AST.Print chevron exprs comma annot) =
    let (_, cexprs) = cythonizeArray ctx exprs
    in (ctx, AST.Print chevron cexprs comma annot)
  cythonize ctx (AST.Exec expr globs annot) =
    let (_, cexpr) = cythonize ctx expr
        cglobs (Just (t1, Just t2)) =
          Just ((snd $ cythonize ctx t1), Just (snd $ cythonize ctx t2))
        cglobs (Just (t1, Nothing)) =
          Just ((snd $ cythonize ctx t1), Nothing)
        cglobs Nothing = Nothing
    in (ctx, AST.Exec cexpr (cglobs globs) annot)

instance Cythonizable AST.RaiseExpr where
  cythonize ctx (AST.RaiseV3 (Just (expr1, Just expr2))) =
    let (_, cexpr1) = cythonize ctx expr1
        (_, cexpr2) = cythonize ctx expr2
    in (ctx, AST.RaiseV3 (Just (cexpr1, Just cexpr2)))
  cythonize ctx (AST.RaiseV3 (Just (expr1, Nothing))) =
    let (_, cexpr1) = cythonize ctx expr1
    in (ctx, AST.RaiseV3 (Just (cexpr1, Nothing)))
  cythonize ctx (AST.RaiseV3 Nothing) = (ctx, AST.RaiseV3 Nothing)
  cythonize ctx (AST.RaiseV2 (Just (expr1, Just (expr2, Just expr3)))) =
    let (_, cexpr1) = cythonize ctx expr1
        (_, cexpr2) = cythonize ctx expr2
        (_, cexpr3) = cythonize ctx expr3
    in (ctx, AST.RaiseV2 (Just (cexpr1, Just (cexpr2, Just cexpr3))))
  cythonize ctx (AST.RaiseV2 (Just (expr1, Just (expr2, Nothing)))) =
    let (_, cexpr1) = cythonize ctx expr1
        (_, cexpr2) = cythonize ctx expr2
    in (ctx, AST.RaiseV2 (Just (cexpr1, Just (cexpr2, Nothing))))
  cythonize ctx (AST.RaiseV2 (Just (expr1, Nothing))) =
    let (_, cexpr1) = cythonize ctx expr1
    in (ctx, AST.RaiseV2 (Just (cexpr1, Nothing)))
  cythonize ctx (AST.RaiseV2 Nothing) = (ctx, AST.RaiseV2 Nothing)

instance Cythonizable AST.Decorator where
  cythonize ctx (AST.Decorator name args annot) =
    let (_, cname) = cythonizeArray ctx name
        (_, cargs) = cythonizeArray ctx args
    in (ctx, AST.Decorator cname cargs annot)

instance Cythonizable AST.Parameter where
  cythonize ctx (AST.Param name py_annot dflt annot) =
    let (_, cname) = cythonize ctx name
        (_, cpy_annot) = cythonizeMaybe ctx py_annot
        (_, cdflt) = cythonizeMaybe ctx dflt
    in (ctx, AST.Param cname cpy_annot cdflt annot)
  cythonize ctx (AST.VarArgsPos name py_annot annot) =
    let (_, cname) = cythonize ctx name
        (_, cpy_annot) = cythonizeMaybe ctx py_annot
    in (ctx, AST.VarArgsPos cname cpy_annot annot)
  cythonize ctx (AST.VarArgsKeyword name py_annot annot) =
    let (_, cname) = cythonize ctx name
        (_, cpy_annot) = cythonizeMaybe ctx py_annot
    in (ctx, AST.VarArgsKeyword cname cpy_annot annot)
  cythonize ctx (AST.EndPositional annot) = (ctx, AST.EndPositional annot)
  cythonize ctx (AST.UnPackTuple unpack dflt annot) =
    let (_, cunpack) = cythonize ctx unpack
        (_, cdflt) = cythonizeMaybe ctx dflt
    in (ctx, AST.UnPackTuple cunpack cdflt annot)

instance Cythonizable AST.ParamTuple where
  cythonize ctx (AST.ParamTupleName name annot) =
    let (_, cname) = cythonize ctx name
    in (ctx, AST.ParamTupleName cname annot)
  cythonize ctx (AST.ParamTuple tuple annot) =
    let (_, ctuple) = cythonizeArray ctx tuple
    in (ctx, AST.ParamTuple ctuple annot)

instance Cythonizable AST.Argument where
  cythonize ctx (AST.ArgExpr expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.ArgExpr cexpr annot)
  cythonize ctx (AST.ArgVarArgsPos expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.ArgVarArgsPos cexpr annot)
  cythonize ctx (AST.ArgVarArgsKeyword expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.ArgVarArgsKeyword cexpr annot)
  cythonize ctx (AST.ArgKeyword keyword expr annot) =
    let (_, ckeyword) = cythonize ctx keyword
        (_, cexpr) = cythonize ctx expr
    in (ctx, AST.ArgKeyword ckeyword cexpr annot)

instance Cythonizable AST.Handler where
  cythonize ctx (AST.Handler clause suite annot) =
    let (_, cclause) = cythonize ctx clause
        (_, csuite) = cythonizeArray ctx suite
    in (ctx, AST.Handler cclause csuite annot)

instance Cythonizable AST.ExceptClause where
  cythonize ctx (AST.ExceptClause (Just (expr1, Just expr2)) annot) =
    let (_, cexpr1) = cythonize ctx expr1
        (_, cexpr2) = cythonize ctx expr2
    in (ctx, AST.ExceptClause (Just (cexpr1, Just cexpr2)) annot)
  cythonize ctx (AST.ExceptClause (Just (expr1, Nothing)) annot) =
    let (_, cexpr1) = cythonize ctx expr1
    in (ctx, AST.ExceptClause (Just (cexpr1, Nothing)) annot)
  cythonize ctx (AST.ExceptClause Nothing annot) =
    (ctx, AST.ExceptClause Nothing annot)

instance Cythonizable AST.Comprehension where
  cythonize ctx (AST.Comprehension expr for annot) =
    let (_, cexpr) = cythonize ctx expr
        (_, cfor) = cythonize ctx for
    in (ctx, AST.Comprehension cexpr cfor annot)

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
    in (ctx, AST.CompFor cfor cin_expr citer annot)

instance Cythonizable AST.CompIf where
  cythonize ctx (AST.CompIf expr iter annot) =
    let (_, cexpr) = cythonize ctx expr
        (_, citer) = cythonizeMaybe ctx iter
    in (ctx, AST.CompIf cexpr citer annot)

instance Cythonizable AST.CompIter where
  cythonize ctx (AST.IterFor iter annot) =
    let (_, citer) = cythonize ctx iter
    in (ctx, AST.IterFor citer annot)
  cythonize ctx (AST.IterIf iter annot) =
    let (_, citer) = cythonize ctx iter
    in (ctx, AST.IterIf citer annot)

instance Cythonizable AST.Expr where
  cythonize ctx (AST.Var ident annot) =
    let (_, cident) = cythonize ctx ident
    in (ctx, AST.Var cident annot)
  cythonize ctx (AST.Int val lit (_, annot)) =
    (ctx, AST.Int val lit (Expr . CType $ Signed Int, annot))
  cythonize ctx (AST.LongInt val lit (_, annot)) =
    (ctx, AST.LongInt val lit (Expr . CType $ Signed Long, annot))
  cythonize ctx (AST.Float val lit (_, annot)) =
    (ctx, AST.Float val lit (Expr . CType $ Signed Double, annot))
  cythonize ctx (AST.Imaginary val lit annot) =
    (ctx, AST.Imaginary val lit annot)
  cythonize ctx (AST.Bool val (_, annot)) =
    (ctx, AST.Bool val (Expr $ CType BInt, annot))
  cythonize ctx (AST.None annot) =
    (ctx, AST.None annot)
  cythonize ctx (AST.Ellipsis annot) =
    (ctx, AST.Ellipsis annot)
  cythonize ctx (AST.ByteStrings str annot) =
    (ctx, AST.ByteStrings str annot)
  cythonize ctx (AST.Strings str annot) =
    (ctx, AST.Strings str annot)
  cythonize ctx (AST.UnicodeStrings str annot) =
    (ctx, AST.UnicodeStrings str annot)
  cythonize ctx (AST.Call fun args annot) =
    let (_, cfun) = cythonize ctx fun
        (_, cargs) = cythonizeArray ctx args
    in (ctx, AST.Call cfun cargs annot)
  cythonize ctx (AST.Subscript e expr annot) =
    let (_, ce) = cythonize ctx e
        (_, cexpr) = cythonize ctx expr
    in (ctx, AST.Subscript ce cexpr annot)
  cythonize ctx (AST.SlicedExpr expr slice annot) =
    let (_, cexpr) = cythonize ctx expr
        (_, cslice) = cythonizeArray ctx slice
    in (ctx, AST.SlicedExpr cexpr cslice annot)
  cythonize ctx (AST.CondExpr true cond false annot) =
    let (_, ctrue) = cythonize ctx true
        (_, ccond) = cythonize ctx cond
        (_, cfalse) = cythonize ctx false
    in (ctx, AST.CondExpr ctrue ccond cfalse annot)
  cythonize ctx (AST.BinaryOp op left right annot) =
    let (_, cop) = cythonize ctx op
        (_, cleft) = cythonize ctx left
        (_, cright) = cythonize ctx right
    in (ctx, AST.BinaryOp cop cleft cright annot)
  cythonize ctx (AST.UnaryOp op expr annot) =
    let (_, cop) = cythonize ctx op
        (_, cexpr) = cythonize ctx expr
    in (ctx, AST.UnaryOp cop cexpr annot)
  cythonize ctx (AST.Dot expr ident annot) =
    let (_, cexpr) = cythonize ctx expr
        (_, cident) = cythonize ctx ident
    in (ctx, AST.Dot cexpr cident annot)
  cythonize ctx (AST.Lambda args body annot) =
    let (_, cargs) = cythonizeArray ctx args
        (_, cbody) = cythonize ctx body
    in (ctx, AST.Lambda cargs cbody annot)
  cythonize ctx (AST.Tuple exprs annot) =
    let (_, cexprs) = cythonizeArray ctx exprs
    in (ctx, AST.Tuple cexprs annot)
  cythonize ctx (AST.Yield arg annot) =
    let (_, carg) = cythonizeMaybe ctx arg
    in (ctx, AST.Yield carg annot)
  cythonize ctx (AST.Generator comp annot) =
    let (_, ccomp) = cythonize ctx comp
    in (ctx, AST.Generator ccomp annot)
  cythonize ctx (AST.ListComp comp annot) =
    let (_, ccomp) = cythonize ctx comp
    in (ctx, AST.ListComp ccomp annot)
  cythonize ctx (AST.List exprs annot) =
    let (_, cexprs) = cythonizeArray ctx exprs
    in (ctx, AST.List cexprs annot)
  cythonize ctx (AST.Dictionary mappings annot) =
    let (_, cmappings) = cythonizeArray ctx mappings
    in (ctx, AST.Dictionary cmappings annot)
  cythonize ctx (AST.DictComp comp annot) =
    let (_, ccomp) = cythonize ctx comp
    in (ctx, AST.DictComp ccomp annot)
  cythonize ctx (AST.Set exprs annot) =
    let (_, cexprs) = cythonizeArray ctx exprs
    in (ctx, AST.Set cexprs annot)
  cythonize ctx (AST.SetComp comp annot) =
    let (_, ccomp) = cythonize ctx comp
    in (ctx, AST.SetComp ccomp annot)
  cythonize ctx (AST.Starred expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.Starred cexpr annot)
  cythonize ctx (AST.Paren expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.Paren cexpr annot)
  cythonize ctx (AST.StringConversion expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.StringConversion cexpr annot)

instance Cythonizable AST.YieldArg where
  cythonize ctx (AST.YieldFrom expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.YieldFrom cexpr annot)
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
    in (ctx, AST.SliceProper clower cupper (cstride stride) annot)
  cythonize ctx (AST.SliceExpr expr annot) =
    let (_, cexpr) = cythonize ctx expr
    in (ctx, AST.SliceExpr cexpr annot)
  cythonize ctx (AST.SliceEllipsis annot) =
    (ctx, AST.SliceEllipsis annot)
