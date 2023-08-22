||| Common types and utility funtions for the LSP server implementation.
|||
||| (C) The Idris Community, 2021
module Server.Utils

import Core.Context
import Core.Core
import Core.FC
import Core.Name
import Core.Metadata
import Data.Bits
import Data.List
import Data.String
import Idris.Pretty
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.Message
import Libraries.Data.PosMap
import Server.Configuration
import System.File
import System
import System.Info

||| Gets a specific component of a reference, using the supplied projection.
export
gets : (l : label) -> Ref l a => (a -> b) -> Core b
gets l f = f <$> get l

export
uncons' : List a -> Maybe (a, List a)
uncons' [] = Nothing
uncons' (x :: xs) = Just (x, xs)

export
findInTreeLoc' : FilePos -> FilePos -> PosMap (NonEmptyFC, a) -> List (NonEmptyFC, a)
findInTreeLoc' sp ep m = sortBy (\x, y => cmp (measure x) (measure y)) $ dominators (sp, ep) m
  where
    cmp : FileRange -> FileRange -> Ordering
    cmp ((sr1, sc1), (er1, ec1)) ((sr2, sc2), (er2, ec2)) =
      compare (er1 - sr1, ec1 - sc1) (er2 - sr2, ec2 - sc2)

export
findPointInTreeLoc' : FilePos -> PosMap (NonEmptyFC, a) -> List (NonEmptyFC, a)
findPointInTreeLoc' p = findInTreeLoc' p p

export
findInTreeLoc : FilePos -> FilePos -> PosMap (NonEmptyFC, a) -> Maybe (NonEmptyFC, a)
findInTreeLoc sp ep m = head' $ findInTreeLoc' sp ep m

export
findPointInTreeLoc : FilePos -> PosMap (NonEmptyFC, a) -> Maybe (NonEmptyFC, a)
findPointInTreeLoc p = findInTreeLoc p p

export
findInTree : FilePos -> FilePos -> PosMap (NonEmptyFC, a) -> Maybe a
findInTree sp ep m = snd <$> findInTreeLoc sp ep m

export
findPointInTree : FilePos -> PosMap (NonEmptyFC, a) -> Maybe a
findPointInTree p = findInTree p p

export
anyAt : (a -> Bool) -> a -> b -> Bool
anyAt p loc _ = p loc

export
anyWithName : Name -> (NonEmptyFC -> Bool) -> NonEmptyFC -> (Name, b) -> Bool
anyWithName name p loc (n, _) = p loc && name == n

export
Cast FilePos Position where
  cast (line, col) = MkPosition line col

export
Cast Position FilePos where
  cast (MkPosition line col) = (line, col)

export
Cast FileRange Range where
  cast (start, end) = MkRange (cast start) (cast end)

export
Cast Range FileRange where
  cast (MkRange start end) = (cast start, cast end)

export
Cast FC Range where
  cast (MkFC _ start end) = MkRange { start = cast start, end = cast end }
  cast (MkVirtualFC _ start end) = MkRange { start = cast start, end = cast end }
  cast EmptyFC = MkRange { start = MkPosition 0 0, end = MkPosition 0 0 }

export
Cast NonEmptyFC Range where
  cast (_, start, end) = MkRange { start = cast start, end = cast end }

public export
Measure (Range, a) where
  measure (r, _) = (cast r.start, cast r.end)

export
prettyTerm : IPTerm -> Doc IdrisAnn
prettyTerm = prettyBy Syntax

export
searchCache : Ref LSPConf LSPConfiguration => Range -> IdrisAction -> Core (List CodeAction)
searchCache r type = do
  cache <- gets LSPConf cachedActions
  let inRange = dominators (cast r) cache
  pure $ concatMap (snd . snd) $ filter (\(_, t, _) => type == t) inRange

export
toStart : FC -> FC
toStart (MkFC f _ _) = MkFC f (0, 0) (0, 0)
toStart (MkVirtualFC f _ _) = MkVirtualFC f (0, 0) (0, 0)
toStart EmptyFC = EmptyFC
