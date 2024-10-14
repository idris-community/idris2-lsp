module Language.LSP.CodeAction

public export
data IdrisAction
  = CaseSplit
  | ExprSearch
  | GenerateDef
  | GenerateDefNext
  | MakeCase
  | MakeClause
  | MakeLemma
  | MakeWith
  | RefineHole
  | Intro

export
Eq IdrisAction where
  CaseSplit       == CaseSplit   = True
  ExprSearch      == ExprSearch  = True
  GenerateDef     == GenerateDef = True
  GenerateDefNext == GenerateDefNext = True
  MakeCase        == MakeCase    = True
  MakeClause      == MakeClause  = True
  MakeLemma       == MakeLemma   = True
  MakeWith        == MakeWith    = True
  RefineHole      == RefineHole  = True
  Intro           == Intro       = True
  _ == _ = False

export
Show IdrisAction where
  show CaseSplit       = "CaseSplit"
  show ExprSearch      = "ExprSearch"
  show GenerateDef     = "GenerateDef"
  show GenerateDefNext = "GenerateDefNext"
  show MakeCase        = "MakeCase"
  show MakeClause      = "MakeClause"
  show MakeLemma       = "MakeLemma"
  show MakeWith        = "MakeWith"
  show RefineHole      = "RefineHole"
  show Intro           = "Intro"
