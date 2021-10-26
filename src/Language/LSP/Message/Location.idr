module Language.LSP.Message.Location

import Data.URI
import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.URI
import Language.LSP.Message.Utils
import Language.Reflection

%hide Prelude.Range
%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#position
public export
record Position where
  constructor MkPosition
  line      : Int
  character : Int
%runElab deriveJSON defaultOpts `{Position}

export
Eq Position where
  (MkPosition line1 char1) == (MkPosition line2 char2) = line1 == line2 && char1 == char2

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#range
public export
record Range where
  constructor MkRange
  start : Position
  end   : Position
%runElab deriveJSON defaultOpts `{Range}

export
Eq Range where
  (MkRange start1 end1) == (MkRange start2 end2) = start1 == start2 && end1 == end2

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#location
public export
record Location where
  constructor MkLocation
  uri   : URI
  range : Range
%runElab deriveJSON defaultOpts `{Location}

export
Eq Location where
  (MkLocation uri1 range1) == (MkLocation uri2 range2) = uri1 == uri2 && range1 == range2

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#locationLink
public export
record LocationLink where
  constructor MkLocationLink
  originSelectionRange : Maybe Range
  targetUri            : URI
  targetRange          : Range
  targetSelectionRange : Range
%runElab deriveJSON defaultOpts `{LocationLink}
