module Language.LSP.CodeAction

public export
data IdrisAction
  = CaseSplit
  | ExprSearch
  | GenerateDef
  | MakeCase
  | MakeClause
  | MakeLemma
  | MakeWith
  | FillHole

export
Eq IdrisAction where
  CaseSplit   == CaseSplit   = True
  ExprSearch  == ExprSearch  = True
  GenerateDef == GenerateDef = True
  MakeCase    == MakeCase    = True
  MakeClause  == MakeClause  = True
  MakeLemma   == MakeLemma   = True
  MakeWith    == MakeWith    = True
  FillHole    == FillHole    = True
  _ == _ = False

export
Show IdrisAction where
  show CaseSplit   = "CaseSplit"
  show ExprSearch  = "ExprSearch"
  show GenerateDef = "GenerateDef"
  show MakeCase    = "MakeCase"
  show MakeClause  = "MakeClause"
  show MakeLemma   = "MakeLemma"
  show MakeWith    = "MakeWith"
  show FillHole    = "FillHole"
