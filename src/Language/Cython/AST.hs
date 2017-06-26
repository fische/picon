{-# LANGUAGE DeriveDataTypeable, DefaultSignatures #-}

module Language.Cython.AST where

import qualified Language.Python.Common.AST as AST
import Data.Data

data CBasicType = Char | Short | Int | Long | LongLong | Float | Double
  deriving (Eq,Ord,Show,Typeable,Data)
data CType = BInt | Unsigned CBasicType | Signed CBasicType | Ptr CType
  deriving (Eq,Ord,Show,Typeable,Data)
data CythonType = CType CType | PythonObject
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

class Cythonizable t where
  cythonize :: t annot -> t (Annotation, annot)
  default cythonize :: (Functor t) => t annot -> t (Annotation, annot)
  cythonize = fmap (\annot -> (Empty, annot))

instance Cythonizable AST.Ident
instance Cythonizable AST.Op
instance Cythonizable AST.AssignOp

instance Cythonizable AST.Module where
  cythonize (AST.Module stmts) =
    AST.Module (cythonizeArray stmts)

cythonizeArray :: (Cythonizable c) => [c s] -> [c (Annotation, s)]
cythonizeArray = fmap cythonize

instance Cythonizable AST.ImportItem where
  cythonize (AST.ImportItem item as annot) =
    let citem = cythonizeArray item
        cas (Just ident) = Just (cythonize ident)
        cas _ = Nothing
    in AST.ImportItem citem (cas as) (Empty, annot)

instance Cythonizable AST.FromItem where
  cythonize (AST.FromItem item as annot) =
    let citem = cythonize item
        cas (Just ident) = Just (cythonize ident)
        cas _ = Nothing
    in AST.FromItem citem (cas as) (Empty, annot)

instance Cythonizable AST.FromItems where
  cythonize (AST.ImportEverything annot) = AST.ImportEverything (Empty, annot)
  cythonize (AST.FromItems items annot) =
    let citems = cythonizeArray items
    in AST.FromItems citems (Empty, annot)

instance Cythonizable AST.ImportRelative where
  cythonize (AST.ImportRelative dots m annot) =
    let cmod (Just dotted) = Just (cythonizeArray dotted)
        cmod _ = Nothing
    in AST.ImportRelative dots (cmod m) (Empty, annot)

cythonizeGuards :: (Cythonizable c) => [(c s, AST.Suite s)]
  -> [(c (Annotation, s), AST.Suite (Annotation, s))]
cythonizeGuards [] = []
cythonizeGuards ((f,s):tl) =
  let cf = cythonize f
      cs = cythonizeArray s
  in (cf, cs) : (cythonizeGuards tl)

cythonizeContext :: (Cythonizable c) => [(c s, Maybe (c s))]
  -> [(c (Annotation, s), Maybe (c (Annotation, s)))]
cythonizeContext [] = []
cythonizeContext ((f,s):tl) =
  let cf = cythonize f
      cs (Just c) = Just (cythonize c)
      cs _ = Nothing
  in (cf, (cs s)) : (cythonizeContext tl)

cythonizeMaybe :: (Cythonizable c) => Maybe (c s) -> Maybe (c (Annotation, s))
cythonizeMaybe (Just c) = Just (cythonize c)
cythonizeMaybe Nothing = Nothing

instance Cythonizable AST.Statement where
  cythonize (AST.Import items annot) =
    AST.Import (cythonizeArray items) (Empty, annot)
  cythonize (AST.FromImport m items annot) =
    AST.FromImport (cythonize m) (cythonize items) (Empty, annot)
  cythonize (AST.While cond body e annot) =
    let ccond = cythonize cond
        cbody = cythonizeArray body
        celse = cythonizeArray e
    in AST.While ccond cbody celse (Empty, annot)
  cythonize (AST.For targets gen body e annot) =
    let ctargets = cythonizeArray targets
        cgen = cythonize gen
        cbody = cythonizeArray body
        celse = cythonizeArray e
    in AST.For ctargets cgen cbody celse (Empty, annot)
  cythonize (AST.Fun name args result body annot) =
    let cname = cythonize name
        cargs = cythonizeArray args
        cbody = cythonizeArray body
    in AST.Fun cname cargs (cythonizeMaybe result) cbody (Empty, annot)
  cythonize (AST.Class name args body annot) =
    let cname = cythonize name
        cargs = cythonizeArray args
        cbody = cythonizeArray body
    in AST.Class cname cargs cbody (Empty, annot)
  cythonize (AST.Conditional guards e annot) =
    let cguards = cythonizeGuards guards
        celse = cythonizeArray e
    in AST.Conditional cguards celse (Empty, annot)
  cythonize (AST.Assign [to] expr annot) =
    let cexpr = cythonize expr
        cto = cythonize to
        cannot = Assign { cdef = True, ctype = (getExprType cexpr) }
    in AST.Assign [cto] cexpr (cannot, annot)
  cythonize (AST.Assign tos expr annot) = -- TODO Handle mutliple vars
    let cexpr = cythonize expr
        ctos = cythonizeArray tos
    in AST.Assign ctos cexpr (Empty, annot)
  cythonize (AST.AugmentedAssign to op expr annot) =
    let cto = cythonize to
        cop = cythonize op
        cexpr = cythonize expr
    in AST.AugmentedAssign cto cop cexpr (Empty, annot)
  cythonize (AST.Decorated decorators def annot) =
    let cdecorators = cythonizeArray decorators
        cydef = cythonize def
    in AST.Decorated cdecorators cydef (Empty, annot)
  cythonize (AST.Return expr annot) =
    AST.Return (cythonizeMaybe expr) (Empty, annot)
  cythonize (AST.Try body excepts e fin annot) =
    let cbody = cythonizeArray body
        cexcepts = cythonizeArray excepts
        celse = cythonizeArray e
        cfin = cythonizeArray fin
    in AST.Try cbody cexcepts celse cfin (Empty, annot)
  cythonize (AST.Raise expr annot) =
    AST.Raise (cythonize expr) (Empty, annot)
  cythonize (AST.With ctx body annot) =
    let cctx = cythonizeContext ctx
        cbody = cythonizeArray body
    in AST.With cctx cbody (Empty, annot)
  cythonize (AST.Pass annot) =
    AST.Pass (Empty, annot)
  cythonize (AST.Break annot) =
    AST.Break (Empty, annot)
  cythonize (AST.Continue annot) =
    AST.Continue (Empty, annot)
  cythonize (AST.Delete exprs annot) =
    AST.Delete (cythonizeArray exprs) (Empty, annot)
  cythonize (AST.StmtExpr expr annot) =
    AST.StmtExpr (cythonize expr) (Empty, annot)
  cythonize (AST.Global vars annot) =
    AST.Global (cythonizeArray vars) (Empty, annot)
  cythonize (AST.NonLocal vars annot) =
    AST.NonLocal (cythonizeArray vars) (Empty, annot)
  cythonize (AST.Assert exprs annot) =
    AST.Assert (cythonizeArray exprs) (Empty, annot)
  cythonize (AST.Print chevron exprs comma annot) =
    AST.Print chevron (cythonizeArray exprs) comma (Empty, annot)
  cythonize (AST.Exec expr globs annot) =
    let cexpr = cythonize expr
        cglobs (Just (t1, Just t2)) = Just ((cythonize t1), Just (cythonize t2))
        cglobs (Just (t1, Nothing)) = Just ((cythonize t1), Nothing)
        cglobs Nothing = Nothing
    in AST.Exec cexpr (cglobs globs) (Empty, annot)

instance Cythonizable AST.RaiseExpr where
  cythonize (AST.RaiseV3 (Just (expr1, Just expr2))) =
    AST.RaiseV3 (Just ((cythonize expr1), Just (cythonize expr2)))
  cythonize (AST.RaiseV3 (Just (expr1, Nothing))) =
    AST.RaiseV3 (Just ((cythonize expr1), Nothing))
  cythonize (AST.RaiseV3 Nothing) = AST.RaiseV3 Nothing
  cythonize (AST.RaiseV2 (Just (expr1, Just (expr2, Just expr3)))) =
    AST.RaiseV2 (Just ((cythonize expr1), Just ((cythonize expr2), Just (cythonize expr3))))
  cythonize (AST.RaiseV2 (Just (expr1, Just (expr2, Nothing)))) =
    AST.RaiseV2 (Just ((cythonize expr1), Just ((cythonize expr2), Nothing)))
  cythonize (AST.RaiseV2 (Just (expr1, Nothing))) =
    AST.RaiseV2 (Just ((cythonize expr1), Nothing))
  cythonize (AST.RaiseV2 Nothing) = AST.RaiseV2 Nothing

instance Cythonizable AST.Decorator where
  cythonize (AST.Decorator name args annot) =
    let cname = cythonizeArray name
        cargs = cythonizeArray args
    in AST.Decorator cname cargs (Empty, annot)

instance Cythonizable AST.Parameter where
  cythonize (AST.Param name py_annot dflt annot) =
    let cname = cythonize name
        cpy_annot = cythonizeMaybe py_annot
        cdflt = cythonizeMaybe dflt
    in AST.Param cname cpy_annot cdflt (Empty, annot)
  cythonize (AST.VarArgsPos name py_annot annot) =
    let cname = cythonize name
        cpy_annot = cythonizeMaybe py_annot
    in AST.VarArgsPos cname cpy_annot (Empty, annot)
  cythonize (AST.VarArgsKeyword name py_annot annot) =
    let cname = cythonize name
        cpy_annot = cythonizeMaybe py_annot
    in AST.VarArgsKeyword cname cpy_annot (Empty, annot)
  cythonize (AST.EndPositional annot) = AST.EndPositional (Empty, annot)
  cythonize (AST.UnPackTuple unpack dflt annot) =
    let cunpack = cythonize unpack
        cdflt = cythonizeMaybe dflt
    in AST.UnPackTuple cunpack cdflt (Empty, annot)

instance Cythonizable AST.ParamTuple where
  cythonize (AST.ParamTupleName name annot) =
    AST.ParamTupleName (cythonize name) (Empty, annot)
  cythonize (AST.ParamTuple tuple annot) =
    AST.ParamTuple (cythonizeArray tuple) (Empty, annot)

instance Cythonizable AST.Argument where
  cythonize (AST.ArgExpr expr annot) =
    AST.ArgExpr (cythonize expr) (Empty, annot)
  cythonize (AST.ArgVarArgsPos expr annot) =
    AST.ArgVarArgsPos (cythonize expr) (Empty, annot)
  cythonize (AST.ArgVarArgsKeyword expr annot) =
    AST.ArgVarArgsKeyword (cythonize expr) (Empty, annot)
  cythonize (AST.ArgKeyword keyword expr annot) =
    AST.ArgKeyword (cythonize keyword)(cythonize expr) (Empty, annot)

instance Cythonizable AST.Handler where
  cythonize (AST.Handler clause suite annot) =
    AST.Handler (cythonize clause) (cythonizeArray suite) (Empty, annot)

instance Cythonizable AST.ExceptClause where
  cythonize (AST.ExceptClause (Just (expr1, Just expr2)) annot) =
    let cexpr1 = cythonize expr1
        cexpr2 = cythonize expr2
    in AST.ExceptClause (Just (cexpr1, Just cexpr2)) (Empty, annot)
  cythonize (AST.ExceptClause (Just (expr1, Nothing)) annot) =
    let cexpr1 = cythonize expr1
    in AST.ExceptClause (Just (cexpr1, Nothing)) (Empty, annot)
  cythonize (AST.ExceptClause Nothing annot) =
    AST.ExceptClause Nothing (Empty, annot)

instance Cythonizable AST.Comprehension where
  cythonize (AST.Comprehension expr for annot) =
    let cexpr = cythonize expr
        cfor = cythonize for
    in AST.Comprehension cexpr cfor (Empty, annot)

instance Cythonizable AST.ComprehensionExpr where
  cythonize (AST.ComprehensionExpr expr) =
    AST.ComprehensionExpr (cythonize expr)
  cythonize (AST.ComprehensionDict expr) =
    AST.ComprehensionDict (cythonize expr)

instance Cythonizable AST.CompFor where
  cythonize (AST.CompFor for in_expr iter annot) =
    let cfor = cythonizeArray for
        cin_expr = cythonize in_expr
        citer = cythonizeMaybe iter
    in AST.CompFor cfor cin_expr citer (Empty, annot)

instance Cythonizable AST.CompIf where
  cythonize (AST.CompIf expr iter annot) =
    let cexpr = cythonize expr
        citer = cythonizeMaybe iter
    in AST.CompIf cexpr citer (Empty, annot)

instance Cythonizable AST.CompIter where
  cythonize (AST.IterFor iter annot) =
    AST.IterFor (cythonize iter) (Empty, annot)
  cythonize (AST.IterIf iter annot) =
    AST.IterIf (cythonize iter) (Empty, annot)

-- TODO Handle complex numbers
instance Cythonizable AST.Expr where
  cythonize (AST.Var ident annot) =
    AST.Var (cythonize ident) (Empty, annot)
  cythonize (AST.Int val lit annot) =
    AST.Int val lit (Expr . CType $ Signed Int, annot)
  cythonize (AST.LongInt val lit annot) =
    AST.LongInt val lit (Expr . CType $ Signed Long, annot)
  cythonize (AST.Float val lit annot) =
    AST.Float val lit (Expr . CType $ Signed Double, annot)
  cythonize (AST.Imaginary val lit annot) =
    AST.Imaginary val lit (Empty, annot)
  cythonize (AST.Bool val annot) =
    AST.Bool val (Expr $ CType BInt, annot)
  cythonize (AST.None annot) =
    AST.None (Empty, annot)
  cythonize (AST.Ellipsis annot) =
    AST.Ellipsis (Empty, annot)
  cythonize (AST.ByteStrings str annot) =
    AST.ByteStrings str (Empty, annot)
  cythonize (AST.Strings str annot) =
    AST.Strings str (Empty, annot)
  cythonize (AST.UnicodeStrings str annot) =
    AST.UnicodeStrings str (Empty, annot)
  cythonize (AST.Call fun args annot) =
    AST.Call (cythonize fun) (cythonizeArray args) (Empty, annot)
  cythonize (AST.Subscript e expr annot) =
    AST.Subscript (cythonize e) (cythonize expr) (Empty, annot)
  cythonize (AST.SlicedExpr expr slice annot) =
    AST.SlicedExpr (cythonize expr) (cythonizeArray slice) (Empty, annot)
  cythonize (AST.CondExpr true cond false annot) =
    let ctrue = cythonize true
        ccond = cythonize cond
        cfalse = cythonize false
    in AST.CondExpr ctrue ccond cfalse (Empty, annot)
  cythonize (AST.BinaryOp op left right annot) =
    let cop = cythonize op
        cleft = cythonize left
        cright = cythonize right
    in AST.BinaryOp cop cleft cright (Empty, annot)
  cythonize (AST.UnaryOp op expr annot) =
    let cop = cythonize op
        cexpr = cythonize expr
    in AST.UnaryOp cop cexpr (Empty, annot)
  cythonize (AST.Dot expr ident annot) =
    let cexpr = cythonize expr
        cident = cythonize ident
    in AST.Dot cexpr cident (Empty, annot)
  cythonize (AST.Lambda args body annot) =
    let cargs = cythonizeArray args
        cbody = cythonize body
    in AST.Lambda cargs cbody (Empty, annot)
  cythonize (AST.Tuple exprs annot) =
    AST.Tuple (cythonizeArray exprs) (Empty, annot)
  cythonize (AST.Yield arg annot) =
    AST.Yield (cythonizeMaybe arg) (Empty, annot)
  cythonize (AST.Generator comp annot) =
    AST.Generator (cythonize comp) (Empty, annot)
  cythonize (AST.ListComp comp annot) =
    AST.ListComp (cythonize comp) (Empty, annot)
  cythonize (AST.List exprs annot) =
    AST.List (cythonizeArray exprs) (Empty, annot)
  cythonize (AST.Dictionary mappings annot) =
    AST.Dictionary (cythonizeArray mappings) (Empty, annot)
  cythonize (AST.DictComp comp annot) =
    AST.DictComp (cythonize comp) (Empty, annot)
  cythonize (AST.Set exprs annot) =
    AST.Set (cythonizeArray exprs) (Empty, annot)
  cythonize (AST.SetComp comp annot) =
    AST.SetComp (cythonize comp) (Empty, annot)
  cythonize (AST.Starred expr annot) =
    AST.Starred (cythonize expr) (Empty, annot)
  cythonize (AST.Paren expr annot) =
    AST.Paren (cythonize expr) (Empty, annot)
  cythonize (AST.StringConversion expr annot) =
    AST.StringConversion (cythonize expr) (Empty, annot)

getExprType :: AST.Expr (Annotation, s) -> CythonType
getExprType expr
  | cannot /= Empty = ctype cannot
  | otherwise = PythonObject
  where (cannot, _) = AST.expr_annot expr

instance Cythonizable AST.YieldArg where
  cythonize (AST.YieldFrom expr annot) =
    AST.YieldFrom (cythonize expr) (Empty, annot)
  cythonize (AST.YieldExpr expr) =
    AST.YieldExpr (cythonize expr)

instance Cythonizable AST.DictMappingPair where
  cythonize (AST.DictMappingPair expr1 expr2) =
    AST.DictMappingPair (cythonize expr1) (cythonize expr2)

instance Cythonizable AST.Slice where
  cythonize (AST.SliceProper lower upper stride annot) =
    let clower = cythonizeMaybe lower
        cupper = cythonizeMaybe upper
        cstride (Just t) = Just (cythonizeMaybe t)
        cstride Nothing = Nothing
    in AST.SliceProper clower cupper (cstride stride) (Empty, annot)
  cythonize (AST.SliceExpr expr annot) =
    AST.SliceExpr (cythonize expr) (Empty, annot)
  cythonize (AST.SliceEllipsis annot) =
    AST.SliceEllipsis (Empty, annot)
