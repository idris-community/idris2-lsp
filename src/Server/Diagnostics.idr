module Server.Diagnostics

import Core.Context
import Core.Core
import Core.Directory
import Core.Env
import Core.FC
import Core.Metadata
import Data.OneOf
import Data.String
import Idris.Error
import Idris.Pretty
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Idris.Doc.String
import Language.JSON
import Language.LSP.Message
import Libraries.Data.String.Extra
import Parser.Support
import Server.Configuration
import Server.Log
import Server.Utils
import System.File
import System.Path

keyword : Doc IdrisAnn -> Doc IdrisAnn
keyword = annotate $ Syntax Keyword

buildDiagnostic : DiagnosticSeverity -> Maybe FC -> Doc IdrisAnn -> Maybe (List DiagnosticRelatedInformation) -> Diagnostic
buildDiagnostic severity loc error related =
  MkDiagnostic
    { range              = cast $ fromMaybe replFC loc
    , severity           = Just severity
    , code               = Nothing
    , codeDescription    = Nothing
    , source             = Just "idris2"
    , message            = renderString $ unAnnotateS $ layoutUnbounded error
    , tags               = Nothing
    , relatedInformation = related
    , data_              = Nothing
    }

buildRelatedInformation : URI -> FC -> String -> DiagnosticRelatedInformation
buildRelatedInformation uri fc msg = MkDiagnosticRelatedInformation (MkLocation uri (cast fc)) msg

||| Computes a list of `DiagnosticRelatedInformation` from a compiler error.
|||
||| @uri The URI of the source file.
||| @err The compiler error.
getRelatedErrors : (uri : URI) -> (err : Error) -> List DiagnosticRelatedInformation
getRelatedErrors uri (Fatal err) = getRelatedErrors uri err
getRelatedErrors uri (CantConvert fc _ _ l r) =
  [ buildRelatedInformation uri (getLoc l) "Mismatched type"
  , buildRelatedInformation uri (getLoc r) "Mismatched type"
  ]
getRelatedErrors uri (CantSolveEq fc _ _ l r) =
  [ buildRelatedInformation uri (getLoc l) "Can't solve constraint"
  , buildRelatedInformation uri (getLoc r) "Can't solve constraint type"
  ]
getRelatedErrors uri (PatternVariableUnifies fc fct _ n _) =
  [ buildRelatedInformation uri fct ("Unifies with pattern variable " ++ show n) ]
getRelatedErrors uri (CyclicMeta fc _ _ _) = []
getRelatedErrors uri (WhenUnifying fc _ _ x y _) =
  [ buildRelatedInformation uri (getLoc x) "Error while unification"
  , buildRelatedInformation uri (getLoc y) "Error while unification"
  ]
getRelatedErrors uri (ValidCase _ _ (Right err)) = getRelatedErrors uri err
getRelatedErrors uri (InType _ _ err) = getRelatedErrors uri err
getRelatedErrors uri (InCon _ _ err) = getRelatedErrors uri err
getRelatedErrors uri (InLHS _ _ err) = getRelatedErrors uri err
getRelatedErrors uri (InRHS _ _ err) = getRelatedErrors uri err
getRelatedErrors _ _ = []

pShowMN : {vars : _} -> Term vars -> Env Term vars -> Doc IdrisAnn -> Doc IdrisAnn
pShowMN (Local _ _ _ p) env acc =
  case dropAllNS (nameAt p) of
       MN _ _ => acc <++> parens ("implicitly bound at" <++> pretty (getBinderLoc p env))
       _      => acc
pShowMN _ _ acc = acc

pshow : {vars : _}
     -> Ref Ctxt Defs
     => Ref Syn SyntaxInfo
     => Env Term vars
     -> Term vars
     -> Core (Doc IdrisAnn)
pshow env tm = do
  defs <- get Ctxt
  ntm <- normaliseHoles defs env tm
  itm <- resugar env ntm
  pure $ pShowMN ntm env $ prettyTerm itm

pshowNoNorm : {vars : _}
           -> Ref Ctxt Defs
           => Ref Syn SyntaxInfo
           => Env Term vars
           -> Term vars
           -> Core (Doc IdrisAnn)
pshowNoNorm env tm = do
  defs <- get Ctxt
  itm <- resugar env tm
  pure $ pShowMN tm env $ prettyTerm itm

||| Computes a LSP `Diagnostic` from a compiler warning.
|||
||| @caps The client capabilities related to diagnostics
||| @uri The URI of the source file.
||| @err The compiler warning.
export
warningToDiagnostic : Ref Ctxt Defs
            => Ref Syn SyntaxInfo
            => Ref ROpts REPLOpts
            => (caps : Maybe PublishDiagnosticsClientCapabilities)
            -> (uri : URI)
            -> (warning : Warning)
            -> Core Diagnostic
warningToDiagnostic caps uri warning = do
  defs <- get Ctxt
  warningAnn <- pwarning (killWarningLoc warning)
  let loc = getWarningLoc warning
  let wdir = defs.options.dirs.working_dir
  p <- maybe (pure uri.path) (pure . (wdir </>) <=< nsToSource replFC)
         ((\case PhysicalIdrSrc ident => Just ident; _ => Nothing) . fst <=< isNonEmptyFC =<< loc)
  if uri.path == p
     then do let related = Nothing -- TODO related diagnostics?
             pure $ buildDiagnostic Warning loc warningAnn related
     else pure $ buildDiagnostic Warning (toStart <$> loc) ("In" <++> pretty p <+> colon <++> warningAnn) Nothing


||| Computes a LSP `Diagnostic` from a compiler error.
|||
||| @caps The client capabilities related to diagnostics
||| @uri The URI of the source file.
||| @err The compiler error.
export
errorToDiagnostic : Ref Ctxt Defs
            => Ref Syn SyntaxInfo
            => Ref ROpts REPLOpts
            => (caps : Maybe PublishDiagnosticsClientCapabilities)
            -> (uri : URI)
            -> (error : Error)
            -> Core Diagnostic
errorToDiagnostic caps uri err = do
  defs <- get Ctxt
  error <- perror (killErrorLoc err)
  let loc = getErrorLoc err
  let wdir = defs.options.dirs.working_dir
  p <- maybe (pure uri.path) (pure . (wdir </>) <=< nsToSource replFC)
         ((\case PhysicalIdrSrc ident => Just ident; _ => Nothing) . fst <=< isNonEmptyFC =<< loc)
  if uri.path == p
     then do let related = (flip toMaybe (getRelatedErrors uri err) <=< relatedInformation) =<< caps
             pure $ buildDiagnostic Error loc error related
     else pure $ buildDiagnostic Error (toStart <$> loc) ("In" <++> pretty p <+> colon <++> error) Nothing
