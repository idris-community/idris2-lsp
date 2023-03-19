module Language.LSP.Completion.Info

import Core.Name

||| Category for Core.Name
|||
||| For short string suggestion, the suggestion list
||| should be short, only names in current and nested
||| namespaces should appear.
public export
data NameCategory
  = CurrentNsName
  | InNestedNs
  | OtherName
  | FieldName

toNat : NameCategory -> Nat
toNat CurrentNsName = 0
toNat InNestedNs    = 1
toNat OtherName     = 2
toNat FieldName     = 3

export
Eq NameCategory where
  a == b = toNat a == toNat b

export
Ord NameCategory where
  compare a b = compare (toNat a) (toNat b)

||| Completion entry
|||
||| This extract of information is used in generation of the
||| completion response.
public export
record Entry where
  constructor MkEntry
  category      : NameCategory
  fullname      : Name
  type          : String
  arguments     : List (Maybe String)
  -- List of explicit arguments, when unnamed is Nothing, when user defined is Just str
  documentation : String
