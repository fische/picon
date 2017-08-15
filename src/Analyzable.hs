{-# LANGUAGE FlexibleInstances, DefaultSignatures #-}

module Analyzable (
  Analyzable(..)
) where

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.SrcLocation (SrcSpan(..))
import Language.Cython.Type

import Context

getExprType :: Context -> AST.Expr a -> Type
getExprType ctx AST.Var{AST.var_ident = ident} =
  getVariableReference (AST.ident_string ident) ctx
getExprType ctx AST.Call{ AST.call_fun = f } =
  getReturnType (getExprType ctx f) ctx
getExprType _ AST.Int{} = Type . CType $ Signed Int
getExprType _ AST.LongInt{} = Type . CType $ Signed Long
getExprType _ AST.Float{} = Type . CType $ Signed Double
getExprType _ AST.Bool{} = Type $ CType BInt
getExprType _ AST.ByteStrings{} = Type Bytes
getExprType _ AST.Strings{} = Type String
getExprType _ AST.UnicodeStrings{} = Type Unicode
getExprType _ _ = Type PythonObject

class Analyzable t where
  analyze :: t -> Context -> Context
  default analyze :: t -> Context -> Context
  analyze _ = id

instance (Analyzable t) => Analyzable [t] where
  analyze [] ctx = ctx
  analyze (hd:tl) ctx = analyze tl $ analyze hd ctx

instance (Analyzable t) => Analyzable (Maybe t) where
  analyze Nothing ctx = ctx
  analyze (Just v) ctx = analyze v ctx

instance (Analyzable t1, Analyzable t2) => Analyzable (t1, t2) where
  analyze (v1, v2) ctx = analyze v2 $ analyze v1 ctx

instance Analyzable (AST.Ident SrcSpan)
instance Analyzable (AST.Op SrcSpan)
instance Analyzable (AST.AssignOp SrcSpan)

instance Analyzable (AST.Module SrcSpan) where
  analyze (AST.Module stmts) ctx = analyze stmts ctx

instance Analyzable (AST.ImportItem SrcSpan) where
  analyze (AST.ImportItem item as _) ctx = analyze as $ analyze item ctx
--
instance Analyzable (AST.FromItem SrcSpan) where
  analyze (AST.FromItem item as _) ctx = analyze as $ analyze item ctx

instance Analyzable (AST.FromItems SrcSpan) where
  analyze (AST.ImportEverything _) ctx = ctx
  analyze (AST.FromItems items _) ctx = analyze items ctx

instance Analyzable (AST.ImportRelative SrcSpan) where
  analyze (AST.ImportRelative _ m _) ctx = analyze m ctx

instance {-# OVERLAPPING #-} Analyzable
  [(AST.Expr SrcSpan, AST.Suite SrcSpan)] where
  analyze [] ctx = ctx
  analyze ((cond, body):tl) ctx =
    let bodyCtx = exitBlock ctx . analyze body $ analyze cond ctx
    in exitBlock bodyCtx $ analyze tl bodyCtx

instance Analyzable (AST.Statement SrcSpan) where
  analyze (AST.Import items _) ctx = analyze items ctx
  analyze (AST.FromImport m items _) ctx = analyze items $ analyze m ctx
  analyze (AST.While cond body e _) ctx =
    let bodyCtx = exitBlock ctx . analyze body $ analyze cond ctx
    in exitBlock bodyCtx $ analyze e bodyCtx
  analyze (AST.For targets gen body e _) ctx =
    let bodyCtx = exitBlock ctx . analyze body . analyze gen $ analyze targets ctx
    in exitBlock bodyCtx $ analyze e bodyCtx
  analyze (AST.Fun name args _ body _) ctx =
    let parse = analyze body . analyze args
    in stashFunction (AST.ident_string name) parse ctx
  analyze (AST.Class _ args body _) ctx = analyze body $ analyze args ctx
  analyze (AST.Conditional guards e _) ctx =
    let guardsCtx = analyze guards ctx
    in exitBlock guardsCtx $ analyze e guardsCtx
  analyze (AST.Assign [to@AST.Var{}] expr _) ctx =
    let ident = AST.ident_string $ AST.var_ident to
        exprCtx = analyze expr ctx
    in assignVariable ident (getExprType exprCtx expr) exprCtx
  -- TODO Handle when assigning multiple wariables at the same time
  analyze (AST.Assign tos expr _) ctx = analyze expr $ analyze tos ctx
  analyze (AST.AugmentedAssign to op expr _) ctx =
    analyze expr . analyze op $ analyze to ctx
  analyze (AST.Decorated decorators def _) ctx =
    analyze decorators $ analyze def ctx
  analyze (AST.Return expr _) ctx =
    let exprCtx = analyze expr ctx
        exprType = maybe (Type . CType $ Void) (getExprType exprCtx) expr
    in returnVariable exprType exprCtx
  analyze (AST.Try body excepts e fin _) ctx =
    analyze fin . analyze e . analyze excepts $ analyze body ctx
  analyze (AST.Raise expr _) ctx = analyze expr ctx
  analyze (AST.With wctx body _) ctx = analyze body $ analyze wctx ctx
  analyze (AST.Delete exprs _) ctx = analyze exprs ctx
  analyze (AST.StmtExpr expr _) ctx = analyze expr ctx
  analyze (AST.Global vars _) ctx = analyze vars ctx
  analyze (AST.NonLocal vars _) ctx = analyze vars ctx
  analyze (AST.Assert exprs _) ctx = analyze exprs ctx
  analyze (AST.Print _ exprs _ _) ctx = analyze exprs ctx
  analyze (AST.Exec expr t _) ctx = analyze t $ analyze expr ctx
  analyze _ ctx = ctx

instance Analyzable (AST.RaiseExpr SrcSpan) where
  analyze (AST.RaiseV3 expr) ctx = analyze expr ctx
  analyze (AST.RaiseV2 expr) ctx = analyze expr ctx

instance Analyzable (AST.Decorator SrcSpan) where
  analyze (AST.Decorator _ args _) ctx = analyze args ctx

instance Analyzable (AST.Parameter SrcSpan) where
  analyze (AST.Param _ _ dflt _) ctx = analyze dflt ctx
  analyze (AST.UnPackTuple unpack dflt _) ctx = analyze dflt $ analyze unpack ctx
  analyze _ ctx = ctx

instance Analyzable (AST.ParamTuple SrcSpan) where
  analyze (AST.ParamTuple tuple _) ctx = analyze tuple ctx
  analyze _ ctx = ctx

instance Analyzable (AST.Argument SrcSpan) where
  analyze (AST.ArgExpr expr _) ctx = analyze expr ctx
  analyze (AST.ArgVarArgsPos expr _) ctx = analyze expr ctx
  analyze (AST.ArgVarArgsKeyword expr _) ctx = analyze expr ctx
  analyze (AST.ArgKeyword _ expr _) ctx = analyze expr ctx

instance Analyzable (AST.Handler SrcSpan) where
  analyze (AST.Handler clause suite _) ctx = analyze suite $ analyze clause ctx

instance Analyzable (AST.ExceptClause SrcSpan) where
  analyze (AST.ExceptClause expr _) ctx = analyze expr ctx

instance Analyzable (AST.Comprehension SrcSpan) where
  analyze (AST.Comprehension expr for _) ctx = analyze for $ analyze expr ctx

instance Analyzable (AST.ComprehensionExpr SrcSpan) where
  analyze (AST.ComprehensionExpr expr) ctx = analyze expr ctx
  analyze (AST.ComprehensionDict expr) ctx = analyze expr ctx

instance Analyzable (AST.CompFor SrcSpan) where
  analyze (AST.CompFor for in_expr iter _) ctx =
    analyze iter . analyze in_expr $ analyze for ctx

instance Analyzable (AST.CompIf SrcSpan) where
  analyze (AST.CompIf expr iter _) ctx = analyze iter $ analyze expr ctx

instance Analyzable (AST.CompIter SrcSpan) where
  analyze (AST.IterFor iter _) ctx = analyze iter ctx
  analyze (AST.IterIf iter _) ctx = analyze iter ctx

instance Analyzable (AST.Expr SrcSpan) where
  analyze (AST.Call fun [] _) ctx =
    let funCtx = analyze fun ctx
    in call (getExprType funCtx fun) funCtx
  -- TODO Handle with args
  analyze (AST.Call fun args _) ctx = analyze args $ analyze fun ctx
  analyze (AST.Subscript e expr _) ctx = analyze expr $ analyze e ctx
  analyze (AST.SlicedExpr expr slice _) ctx = analyze slice $ analyze expr ctx
  analyze (AST.CondExpr true cond false _) ctx =
    analyze false . analyze cond $ analyze true ctx
  analyze (AST.BinaryOp op left right _) ctx =
    analyze right . analyze left $ analyze op ctx
  analyze (AST.UnaryOp op expr _) ctx = analyze expr $ analyze op ctx
  analyze (AST.Dot expr _ _) ctx = analyze expr ctx
  analyze (AST.Lambda args body _) ctx = analyze body $ analyze args ctx
  analyze (AST.Tuple exprs _) ctx = analyze exprs ctx
  analyze (AST.Yield arg _) ctx = analyze arg ctx
  analyze (AST.Generator comp _) ctx = analyze comp ctx
  analyze (AST.ListComp comp _) ctx = analyze comp ctx
  analyze (AST.List exprs _) ctx = analyze exprs ctx
  analyze (AST.Dictionary mappings _) ctx = analyze mappings ctx
  analyze (AST.DictComp comp _) ctx = analyze comp ctx
  analyze (AST.Set exprs _) ctx = analyze exprs ctx
  analyze (AST.SetComp comp _) ctx = analyze comp ctx
  analyze (AST.Starred expr _) ctx = analyze expr ctx
  analyze (AST.Paren expr _) ctx = analyze expr ctx
  analyze (AST.StringConversion expr _) ctx = analyze expr ctx
  analyze _ ctx = ctx

instance Analyzable (AST.YieldArg SrcSpan) where
  analyze (AST.YieldFrom expr _) ctx = analyze expr ctx
  analyze (AST.YieldExpr expr) ctx = analyze expr ctx

instance Analyzable (AST.DictMappingPair SrcSpan) where
  analyze (AST.DictMappingPair expr1 expr2) ctx = analyze expr2 $ analyze expr1 ctx

instance Analyzable (AST.Slice SrcSpan) where
  analyze (AST.SliceProper lower upper stride _) ctx =
    analyze stride . analyze upper $ analyze lower ctx
  analyze (AST.SliceExpr expr _) ctx = analyze expr ctx
  analyze (AST.SliceEllipsis _) ctx = ctx
