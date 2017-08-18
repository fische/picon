{-# LANGUAGE FlexibleInstances, DefaultSignatures #-}

module Analyzable (
  Analyzable(..),
  module Analyzable.Context
) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import qualified Language.Python.Common.AST as AST
import Language.Python.Common.SrcLocation (SrcSpan(..))
import Language.Cython.Type

import Analyzable.Context

getOpType :: AST.Op a -> Maybe Type
getOpType AST.And{} = Just . Type $ CType BInt
getOpType AST.Or{} = Just . Type $ CType BInt
getOpType AST.Not{} = Just . Type $ CType BInt
getOpType AST.Exponent{} = Just . Type $ CType BInt
getOpType AST.LessThan{} = Just . Type $ CType BInt
getOpType AST.GreaterThan{} = Just . Type $ CType BInt
getOpType AST.Equality{} = Just . Type $ CType BInt
getOpType AST.GreaterThanEquals{} = Just . Type $ CType BInt
getOpType AST.LessThanEquals{} = Just . Type $ CType BInt
getOpType AST.NotEquals{} = Just . Type $ CType BInt
getOpType AST.NotEqualsV2{} = Just . Type $ CType BInt
getOpType AST.In{} = Just . Type $ CType BInt
getOpType AST.Is{} = Just . Type $ CType BInt
getOpType AST.IsNot{} = Just . Type $ CType BInt
getOpType AST.NotIn{} = Just . Type $ CType BInt
getOpType _ = Nothing

getExprType :: Context -> AST.Expr a -> Type
getExprType ctx AST.Var{AST.var_ident = ident} =
  getVariableReference (AST.ident_string ident) ctx
getExprType ctx AST.Call{ AST.call_fun = f } =
  getReturnType (getExprType ctx f) ctx
getExprType ctx AST.BinaryOp{ AST.operator = o, AST.left_op_arg = l } =
  fromMaybe (getExprType ctx l) $ getOpType o
getExprType ctx AST.UnaryOp{ AST.operator = o, AST.op_arg = a } =
  fromMaybe (getExprType ctx a) $ getOpType o
getExprType _ AST.Int{} = Type . CType $ Signed Int
getExprType _ AST.LongInt{} = Type . CType $ Signed Long
getExprType _ AST.Float{} = Type . CType $ Signed Double
getExprType _ AST.Bool{} = Type $ CType BInt
getExprType _ AST.ByteStrings{} = Type Bytes
getExprType _ AST.Strings{} = Type String
getExprType _ AST.UnicodeStrings{} = Type Unicode
getExprType _ _ = Type PythonObject

getKeywordArgumentType :: Context -> [AST.Argument a] ->
  Map.Map Argument Type -> Map.Map Argument Type
getKeywordArgumentType _ [] args = args
getKeywordArgumentType ctx
  (AST.ArgKeyword{AST.arg_keyword = k, AST.arg_expr = expr}:tl) args =
    getKeywordArgumentType ctx tl $
      Map.insert (Keyword $ AST.ident_string k) (getExprType ctx expr) args
getKeywordArgumentType _ (AST.ArgExpr{}:_) _ =
  error "positional argument can not be placed after keyword arguments"
getKeywordArgumentType _ (AST.ArgVarArgsPos{}:_) _ =
  error "excess positional parameter is not yet supported"
getKeywordArgumentType _ (AST.ArgVarArgsKeyword{}:_) _ =
  error "excess keyword parameter is not yet supported"

getPositionalArgumentType :: Context -> [AST.Argument a] ->
  Map.Map Argument Type -> Map.Map Argument Type
getPositionalArgumentType _ [] args = args
getPositionalArgumentType ctx (AST.ArgExpr{AST.arg_expr = expr}:tl) args =
  getPositionalArgumentType ctx tl $
    Map.insert (Position $ Map.size args) (getExprType ctx expr) args
getPositionalArgumentType ctx l@(AST.ArgKeyword{}:_) args =
  getKeywordArgumentType ctx l args
getPositionalArgumentType _ (AST.ArgVarArgsPos{}:_) _ =
  error "excess positional parameter is not yet supported"
getPositionalArgumentType _ (AST.ArgVarArgsKeyword{}:_) _ =
  error "excess keyword parameter is not yet supported"

getArgumentType :: Context -> [AST.Argument a] -> Map.Map Argument Type
getArgumentType ctx args = getPositionalArgumentType ctx args Map.empty

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
    let parse = analyze body . analyze args . enablePositionalParametersFlag
    in stashFunction (AST.ident_string name) parse ctx
  -- TODO Handle inheritance
  analyze (AST.Class name _ body _) ctx =
    exitClass ctx . analyze body $ enterClass (AST.ident_string name) ctx
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
  analyze (AST.Global _ _) _ =
    error "global bindings are not yet supported"
  analyze (AST.NonLocal _ _) _ =
    error "nonlocal bindings are not yet supported"
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
  analyze (AST.Param ident _ dflt _) ctx =
    let dfltCtx = analyze dflt ctx
        dfltType = fmap (getExprType dfltCtx) dflt
    in addParameter (AST.ident_string ident) dfltType dfltCtx
  analyze (AST.EndPositional{}) ctx = disablePositionalParametersFlag ctx
  analyze (AST.UnPackTuple{}) _ = error "tuples are not yet supported"
  analyze (AST.VarArgsPos{}) _ =
    error "excess positional parameter is not yet supported"
  analyze (AST.VarArgsKeyword{}) _ =
    error "excess keyword parameter is not yet supported"

instance Analyzable (AST.Argument SrcSpan) where
  analyze (AST.ArgExpr expr _) ctx = analyze expr ctx
  analyze (AST.ArgKeyword _ expr _) ctx = analyze expr ctx
  analyze (AST.ArgVarArgsPos _ _) _ =
    error "excess positional parameter is not yet supported"
  analyze (AST.ArgVarArgsKeyword _ _) _ =
    error "excess keyword parameter is not yet supported"

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
  analyze (AST.Call fun args _) ctx =
    let funCtx = analyze fun $ analyze args ctx
    in call (getExprType funCtx fun) (getArgumentType funCtx args) funCtx
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
