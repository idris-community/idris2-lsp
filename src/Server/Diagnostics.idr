module Server.Diagnostics

import Core.Context
import Core.Core
import Core.Directory
import Core.Env
import Core.FC
import Core.Metadata
import Data.OneOf
import Data.String
import Idris.Pretty
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Idris.Doc.String
import Language.JSON
import Language.LSP.Message
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
    { range = cast $ fromMaybe replFC loc
    , severity = Just severity
    , code = Nothing
    , codeDescription = Nothing
    , source = Just "idris2"
    , message = renderString $ unAnnotateS $ layoutUnbounded error
    , tags = Nothing
    , relatedInformation = related
    , data_ = Nothing
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
getRelatedErrors uri (PatternVariableUnifies fc _ n tm) =
  [ buildRelatedInformation uri (getLoc tm) ("Unifies with pattern variable " ++ show n) ]
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

pShowMN : {vars : _} -> Term vars -> Env t vars -> Doc IdrisAnn -> Doc IdrisAnn
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
pshow env tm
    = do defs <- get Ctxt
         ntm <- normaliseHoles defs env tm
         itm <- resugar env ntm
         pure (pShowMN ntm env $ prettyTerm itm)

pshowNoNorm : {vars : _}
           -> Ref Ctxt Defs
           => Ref Syn SyntaxInfo
           => Env Term vars
           -> Term vars
           -> Core (Doc IdrisAnn)
pshowNoNorm env tm
    = do defs <- get Ctxt
         itm <- resugar env tm
         pure (pShowMN tm env $ prettyTerm itm)

ploc : FC -> Doc IdrisAnn
ploc fc = annotate FileCtxt (pretty fc)

pwarning : Ref Ctxt Defs
        => Ref Syn SyntaxInfo
        => Ref ROpts REPLOpts
        => Warning
        -> Core (Doc IdrisAnn)
pwarning (UnreachableClause fc env tm) =
  pure $ errorDesc (reflow "Unreachable clause:" <++> code !(pshow env tm))
pwarning (ShadowingGlobalDefs _ ns) =
  pure $ vcat $
    reflow "We are about to implicitly bind the following lowercase names."
      :: reflow "You may be unintentionally shadowing the associated global definitions:"
      :: map (\ (n, ns) => indent 2 $ hsep $ pretty n
                             :: reflow "is shadowing"
                             :: punctuate comma (map pretty (forget ns)))
             (forget ns)
pwarning (Deprecated s fcAndName) =
  do docs <- traverseOpt (\(fc, name) => getDocsForName fc name justUserDoc) fcAndName
     pure . vsep $ catMaybes [ Just $ pretty "Deprecation warning:" <++> pretty s
                             , map (const UserDocString) <$> docs
                             ]
pwarning (GenericWarn s) = pure $ pretty s
pwarning (ParserWarning fc msg) = pure $ pretty msg

export
perror : Ref Ctxt Defs
      => Ref Syn SyntaxInfo
      => Ref ROpts REPLOpts
      => Error
      -> Core (Doc IdrisAnn)
perror (Fatal err) = perror err
perror (CantConvert fc _ env l r) =
  pure $ errorDesc (hsep [ reflow "Mismatch between" <+> colon
                         , code !(pshow env l)
                         , "and"
                         , code !(pshow env r) <+> dot
                         ])
perror (CantSolveEq fc _ env l r) =
  pure $ errorDesc (hsep [ reflow "Can't solve constraint between" <+> colon
                         , code !(pshow env l)
                         , "and"
                         , code !(pshow env r) <+> dot
                         ])
perror (PatternVariableUnifies fc env n tm) =
  pure $ errorDesc (hsep [ reflow "Pattern variable"
                         , code (prettyVar n)
                         , reflow "unifies with" <+> colon
                         , code !(pshow env tm) <+> dot
                         ]) <+> line <+> reflow "Suggestion: Use the same name for both pattern variables, since they unify."
  where
    prettyVar : Name -> Doc IdrisAnn
    prettyVar (PV n _) = prettyVar n
    prettyVar n = pretty n
perror (CyclicMeta fc env n tm)
    = pure $ errorDesc (reflow "Cycle detected in solution of metavariable" <++> meta (pretty !(prettyName n)) <++> equals
        <++> code !(pshow env tm))
perror (WhenUnifying _ _ env x y err)
    = pure $ errorDesc (reflow "When unifying" <++> code !(pshow env x) <++> "and"
        <++> code !(pshow env y)) <+> dot <+> line <+> !(perror err)
perror (ValidCase fc env (Left tm))
    = pure $ errorDesc (code !(pshow env tm) <++> reflow "is not a valid impossible case.")
perror (ValidCase _ env (Right err))
    = pure $ errorDesc (reflow "Impossible pattern gives an error" <+> colon) <+> line <+> !(perror err)
perror (UndefinedName fc x)
    = pure $ errorDesc (reflow "Undefined name" <++> code (pretty x) <+> dot)
perror (InvisibleName fc n (Just ns))
     = pure $ errorDesc ("Name" <++> code (pretty n) <++> reflow "is inaccessible since"
         <++> code (pretty ns) <++> reflow "is not explicitly imported.")
         <+> line <+> reflow "Suggestion: add an explicit" <++> keyword "export" <++> "or" <++> keyword ("public" <++> "export")
         <++> reflow "modifier. By default, all names are" <++> keyword "private" <++> reflow "in namespace blocks."
perror (InvisibleName fc x Nothing)
    = pure $ errorDesc ("Name" <++> code (pretty x) <++> reflow "is private.")
        <+> line <+> reflow "Suggestion: add an explicit" <++> keyword "export" <++> "or" <++> keyword ("public" <++> "export")
        <++> reflow "modifier. By default, all names are" <++> keyword "private" <++> reflow "in namespace blocks."
perror (BadTypeConType fc n)
    = pure $ errorDesc (reflow "Return type of" <++> code (pretty n) <++> reflow "must be" <++> code "Type" <+> dot)
perror (BadDataConType fc n fam)
    = pure $ errorDesc (reflow "Return type of" <++> code (pretty n) <++> reflow "must be in"
        <++> code (pretty !(toFullNames fam)))
perror (NotCovering fc n IsCovering)
    = pure $ errorDesc (reflow "Internal error" <++> parens (reflow "Coverage of" <++> code (pretty n)))
perror (NotCovering fc n (MissingCases cs))
    = pure $ errorDesc (code (pretty !(prettyName n)) <++> reflow "is not covering.")
        <+> line <+> reflow "Missing cases" <+> colon <+> line
        <+> indent 4 (vsep !(traverse (pshow []) cs)) <+> line
perror (NotCovering fc n (NonCoveringCall ns))
    = pure $ errorDesc (pretty !(prettyName n) <++> reflow "is not covering.")
        <+> line <+> reflow "Calls non covering function" <+>
        case ns of
             [fn] => space <+> pretty fn
             _ => pretty 's' <+> colon <++> concatWith (surround (comma <+> space)) (pretty <$> ns)
perror (NotTotal fc n r)
    = pure $ errorDesc (code (pretty !(prettyName n)) <++> reflow "is not total," <++> pretty r)
perror (LinearUsed fc count n)
    = pure $ errorDesc (reflow "There are" <++> pretty count <++> reflow "uses of linear name"
        <++> code (pretty (sugarName n)) <+> dot)
        <+> line <+> reflow "Suggestion: linearly bounded variables must be used exactly once."
perror (LinearMisuse fc n exp ctx)
    = if isErased exp
         then pure $ errorDesc (code (pretty n) <++> reflow "is not accessible in this context.")
         else pure $ errorDesc (reflow "Trying to use" <++> prettyRig exp <++> "name"
                <++> code (pretty (sugarName n)) <++> "in" <++> prettyRel ctx <++> "context.")
  where
    prettyRig : RigCount -> Doc ann
    prettyRig = elimSemi "irrelevant"
                         "linear"
                         (const "unrestricted")

    prettyRel : RigCount -> Doc ann
    prettyRel = elimSemi "irrelevant"
                         "relevant"
                         (const "non-linear")
perror (BorrowPartial fc env tm arg)
    = pure $ errorDesc (code !(pshow env tm) <++> reflow "borrows argument" <++> code !(pshow env arg)
        <++> reflow "so must be fully applied.")
perror (BorrowPartialType fc env tm)
    = pure $ errorDesc (code !(pshow env tm) <++> reflow "borrows, so must return a concrete type.")
perror (AmbiguousName fc ns)
    = pure $ errorDesc (reflow "Ambiguous name" <++> code (pretty ns))
perror (AmbiguousElab fc env ts_in)
    = do pp <- getPPrint
         setPPrint (record { fullNamespace = True } pp)
         ts_show <- for ts_in $ \(gam, t) => do
                      defs <- get Ctxt
                      setCtxt gam
                      res <- pshow env t
                      put Ctxt defs
                      pure res
         let res = vsep [ errorDesc (reflow "Ambiguous elaboration. Possible results" <+> colon)
                        , indent 4 (vsep ts_show)
                        ]
         setPPrint pp
         pure res
perror (AmbiguousSearch fc env tgt ts)
    = pure $ vsep [ errorDesc (reflow "Multiple solutions found in search of" <+> colon)
                  , indent 4 !(pshowNoNorm env tgt)
                  , reflow "Possible correct results" <+> colon
                  , indent 4 (vsep !(traverse (pshowNoNorm env) ts))
                  ]
perror (AmbiguityTooDeep fc n ns)
    = pure $ errorDesc (reflow "Maximum ambiguity depth exceeded in" <++> code (pretty !(getFullName n))
        <+> colon) <+> line <+> concatWith (surround (pretty " --> ")) (pretty <$> !(traverse getFullName ns))
        <+> line <+> reflow "Suggestion: the default ambiguity depth limit is 3, the" <++> code "%ambiguity_depth"
        <++> reflow "pragma can be used to extend this limit, but beware compilation times can be severely impacted."
perror (AllFailed ts)
    = case allUndefined ts of
           Just e => perror e
           _ => pure $ errorDesc (reflow "Sorry, I can't find any elaboration which works. All errors" <+> colon) <+> line
                  <+> vsep !(traverse pAlterror ts)
  where
    pAlterror : (Maybe Name, Error) -> Core (Doc IdrisAnn)
    pAlterror (Just n, err)
       = pure $ "If" <++> code (pretty !(aliasName !(getFullName n))) <+> colon <++> !(perror err)
    pAlterror (Nothing, err)
       = pure $ reflow "Possible error" <+> colon <+> line <+> indent 4 !(perror err)

    allUndefined : List (Maybe Name, Error) -> Maybe Error
    allUndefined [] = Nothing
    allUndefined [(_, UndefinedName loc e)] = Just (UndefinedName loc e)
    allUndefined ((_, UndefinedName _ e) :: es) = allUndefined es
    allUndefined _ = Nothing
perror (RecordTypeNeeded fc _)
    = pure $ errorDesc (reflow "Can't infer type for this record update.")
perror (DuplicatedRecordUpdatePath fc ps)
    = pure $ vcat $
        errorDesc (reflow "Duplicated record update paths:")
          :: map (indent 2 . concatWith (surround (pretty "->")) . map pretty) ps
perror (NotRecordField fc fld Nothing)
    = pure $ errorDesc (code (pretty fld) <++> reflow "is not part of a record type.")
perror (NotRecordField fc fld (Just ty))
    = pure $ errorDesc (reflow "Record type" <++> code (pretty !(getFullName ty)) <++> reflow "has no field"
        <++> code (pretty fld) <+> dot)
perror (NotRecordType fc ty)
    = pure $ errorDesc (code (pretty !(getFullName ty)) <++> reflow "is not a record type.")
perror (IncompatibleFieldUpdate fc flds)
    = pure $ reflow "Field update" <++> concatWith (surround (pretty "->")) (pretty <$> flds)
             <++> reflow "not compatible with other updates at" <+> colon <+> line <+> ploc fc
perror (InvalidArgs fc env [n] tm)
    = pure $ errorDesc (code (pretty n) <++> reflow "is not a valid argument in" <++> !(pshow env tm)
        <+> dot)
perror (InvalidArgs fc env ns tm)
    = pure $ errorDesc (concatWith (surround (comma <+> space)) (code . pretty <$> ns)
        <++> reflow "are not valid arguments in" <++> !(pshow env tm) <+> dot)
perror (TryWithImplicits fc env imps)
    = pure $ errorDesc (reflow "Need to bind implicits"
        <++> concatWith (surround (comma <+> space)) !(traverse (tshow env) imps) <+> dot)
  where
    tshow : {vars : _} ->
            Env Term vars -> (Name, Term vars) -> Core (Doc IdrisAnn)
    tshow env (n, ty) = pure $ pretty n <++> colon <++> code !(pshow env ty)
perror (BadUnboundImplicit fc env n ty)
    = pure $ errorDesc (reflow "Can't bind name" <++> code (pretty (nameRoot n)) <++> reflow "with type" <++> code !(pshow env ty)
        <+> colon) <+> line <+> reflow "Suggestion: try an explicit bind."
perror (CantSolveGoal fc gam env g reason)
    = do defs <- get Ctxt
         setCtxt gam
         let (_ ** (env', g')) = dropEnv env g
         let res = errorDesc (reflow "Can't find an implementation for" <++> code !(pshow env' g') <+> dot)
         put Ctxt defs
         case reason of
              Nothing => pure res
              Just r => do rdesc <- perror r
                           pure (res <+> line <+>
                                 (reflow "Possible cause:" <++> rdesc))
  where
    -- For display, we don't want to see the full top level type; just the
    -- return type
    dropEnv : {vars : _} ->
              Env Term vars -> Term vars ->
              (ns ** (Env Term ns, Term ns))
    dropEnv env (Bind _ n b@(Pi _ _ _ _) sc) = dropEnv (b :: env) sc
    dropEnv env (Bind _ n b@(Let _ _ _ _) sc) = dropEnv (b :: env) sc
    dropEnv env tm = (_ ** (env, tm))
perror (DeterminingArg fc n i env g)
    = pure $ errorDesc (reflow "Can't find an implementation for" <++> code !(pshow env g) <+> line
        <+> reflow "since I can't infer a value for argument" <++> code (pretty n) <+> dot)
perror (UnsolvedHoles hs)
    = pure $ errorDesc (reflow "Unsolved holes" <+> colon) <+> line <+> !(prettyHoles hs)
  where
    prettyHoles : List (FC, Name) -> Core (Doc IdrisAnn)
    prettyHoles [] = pure emptyDoc
    prettyHoles ((fc, n) :: hs)
        = pure $ meta (pretty n) <++> reflow "introduced at:" <++> ploc fc <+> !(prettyHoles hs)
perror (CantInferArgType fc env n h ty)
    = pure $ errorDesc (reflow "Can't infer type for argument" <++> code (pretty n)) <+> line
        <+> "Got" <++> code !(pshow env ty) <++> reflow "with hole" <++> meta (pretty h) <+> dot
perror (SolvedNamedHole fc env h tm)
    = pure $ errorDesc (reflow "Named hole" <++> meta (pretty h) <++> reflow "has been solved by unification.") <+> line
        <+> "Result" <+> colon <++> code !(pshow env tm)
perror (VisibilityError fc vx x vy y)
    = pure $ errorDesc (keyword (pretty vx) <++> code (pretty (sugarName !(toFullNames x)))
        <++> reflow "cannot refer to" <++> keyword (pretty vy) <++> code (pretty (sugarName !(toFullNames y))))
perror (NonLinearPattern fc n)
    = pure $ errorDesc (reflow "Non linear pattern" <++> code (pretty (sugarName n)) <+> dot)
perror (BadPattern fc n)
    = pure $ errorDesc (reflow "Pattern not allowed here" <+> colon <++> code (pretty n) <+> dot)
perror (NoDeclaration fc n)
    = pure $ errorDesc (reflow "No type declaration for" <++> code (pretty n) <+> dot)
perror (AlreadyDefined fc n)
    = pure $ errorDesc (code (pretty n) <++> reflow "is already defined.")
perror (NotFunctionType fc env tm)
    = pure $ errorDesc (code !(pshow env tm) <++> reflow "is not a function type.")
perror (RewriteNoChange fc env rule ty)
    = pure $ errorDesc (reflow "Rewriting by" <++> code !(pshow env rule)
        <++> reflow "did not change type" <++> code !(pshow env ty) <+> dot)
perror (NotRewriteRule fc env rule)
    = pure $ errorDesc (code !(pshow env rule) <++> reflow "is not a rewrite rule type.")
perror (CaseCompile fc n DifferingArgNumbers)
    = pure $ errorDesc (reflow "Patterns for" <++> code (pretty !(prettyName n)) <++> reflow "have differing numbers of arguments.")
perror (CaseCompile fc n DifferingTypes)
    = pure $ errorDesc (reflow "Patterns for" <++> code (pretty !(prettyName n)) <++> reflow "require matching on different types.")
perror (CaseCompile fc n UnknownType)
    = pure $ errorDesc (reflow "Can't infer type to match in" <++> code (pretty !(prettyName n)) <+> dot)
perror (CaseCompile fc n (NotFullyApplied cn))
    = pure $ errorDesc (pretty "Constructor" <++> code (pretty !(toFullNames cn)) <++> reflow "is not fully applied.")
perror (CaseCompile fc n (MatchErased (_ ** (env, tm))))
    = pure $ errorDesc (reflow "Attempt to match on erased argument" <++> code !(pshow env tm) <++> pretty "in"
        <++> code (pretty !(prettyName n)) <+> dot)
perror (BadDotPattern fc env reason x y)
    = pure $ errorDesc (reflow "Can't match on" <++> code !(pshow env x)
        <++> parens (pretty reason) <+> dot)
perror (MatchTooSpecific fc env tm)
    = pure $ errorDesc (reflow "Can't match on" <++> code !(pshow env tm)
        <++> reflow "as it has a polymorphic type.")
perror (BadImplicit fc str)
    = pure $ errorDesc (reflow "Can't infer type for unbound implicit name" <++> code (pretty str) <+> dot)
        <+> line <+> reflow "Suggestion: try making it a bound implicit."
perror (BadRunElab fc env script desc)
    = pure $ errorDesc (reflow "Bad elaborator script" <++> code !(pshow env script) <++> parens (pretty desc) <+> dot)
perror (GenericMsg fc str) = pure $ pretty str
perror (TTCError msg)
    = pure $ errorDesc (reflow "Error in TTC file" <+> colon <++> pretty (show msg))
        <++> parens (pretty "the most likely case is that the ./build directory in your current project contains files from a previous build of idris2 or the idris2 executable is from a different build than the installed .ttc files")
perror (FileErr fname err)
    = pure $ errorDesc (reflow "File error in" <++> pretty fname <++> colon) <++> pretty (show err)
perror (CantFindPackage fname)
    = pure $ errorDesc ("Can't find package" <++> pretty fname)
perror (LitFail _)
    = pure $ errorDesc (reflow "Can't parse literate.")
perror (LexFail _ msg)
    = pure $ errorDesc (pretty msg)
perror (ParseFail ((fc, msg) ::: Nil))
    = pure $ errorDesc (pretty msg) <+> line
perror (ParseFail errs)
    = pure $ errorDesc (reflow "Couldn't parse any alternatives" <+> colon) <+> line <+> !listErrors
  where
    prettyErrors : Nat -> Nat -> List (FC, String) -> Core (Doc IdrisAnn)
    prettyErrors showCount _ []   = pure emptyDoc
    prettyErrors showCount 0 errs = pure $ meta (pretty "... (\{show $ length errs} others)")
    prettyErrors showCount (S k) ((fc, msg) :: hs)
        = do let idx = show $ showCount `minus` k
             pure $ warning (pretty "\{idx}: \{msg}") <+> line <+> !(prettyErrors showCount k hs)

    listErrors : Core (Doc IdrisAnn)
    listErrors = do showCount <- logErrorCount . session . options <$> get Ctxt
                    prettyErrors showCount showCount . nub . reverse $ forget errs
perror (ModuleNotFound fc ns)
    = pure $ errorDesc ("Module" <++> annotate FileCtxt (pretty ns) <++> reflow "not found")
perror (CyclicImports ns)
    = pure $ errorDesc (reflow "Module imports form a cycle" <+> colon) <++> concatWith (surround (pretty " -> ")) (pretty <$> ns)
perror ForceNeeded = pure $ errorDesc (reflow "Internal error when resolving implicit laziness")
perror (InternalError str) = pure $ errorDesc (reflow "INTERNAL ERROR" <+> colon) <++> pretty str
perror (UserError str) = pure $ errorDesc (pretty "Error" <+> colon) <++> pretty str
perror (NoForeignCC fc specs) = do
    let cgs = fst <$> availableCGs (options !(get Ctxt))
    let res = vsep [ errorDesc (reflow ("The given specifier '" ++ show specs ++ "' was not accepted by any backend. Available backends") <+> colon)
                   , indent 2 (concatWith (\ x, y => x <+> ", " <+> y) (map reflow cgs))
                   , reflow "Some backends have additional specifier rules, refer to their documentation."
                   ]
    pure res
perror (BadMultiline _ str)
    = pure $ errorDesc (reflow "Invalid multiline string" <++> colon <++> line <++> pretty str)

perror (InType fc n err)
    = pure $ hsep [ errorDesc (reflow "While processing type of" <++> code (pretty !(prettyName n))) <+> dot
                  , !(perror err)
                  ]
perror (InCon fc n err)
    = pure $ hsep [ errorDesc (reflow "While processing constructor" <++> code (pretty !(prettyName n))) <+> dot
                  , !(perror err)
                  ]
perror (InLHS fc n err)
    = pure $ hsep [ errorDesc (reflow "While processing left hand side of" <++> code (pretty !(prettyName n))) <+> dot
                  , !(perror err)
                  ]
perror (InRHS fc n err)
    = pure $ hsep [ errorDesc (reflow "While processing right hand side of" <++> code (pretty !(prettyName n))) <+> dot
                  , !(perror err)
                  ]
perror (MaybeMisspelling err ns) = pure $ !(perror err) <++> case ns of
  (n ::: []) => reflow "Did you mean:" <++> pretty n <+> "?"
  _ => let (xs, x) = unsnoc ns in
       reflow "Did you mean any of:"
       <++> concatWith (surround (comma <+> space)) (map pretty xs)
       <+> comma <++> reflow "or" <++> pretty x <+> "?"
perror (WarningAsError warn) = pwarning warn
perror (Timeout str) = pure $ errorDesc (reflow "Timeout in" <++> pretty str)


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
  warningAnn <- pwarning warning
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
  error <- perror err
  let loc = getErrorLoc err
  let wdir = defs.options.dirs.working_dir
  p <- maybe (pure uri.path) (pure . (wdir </>) <=< nsToSource replFC)
         ((\case PhysicalIdrSrc ident => Just ident; _ => Nothing) . fst <=< isNonEmptyFC =<< loc)
  if uri.path == p
     then do let related = (flip toMaybe (getRelatedErrors uri err) <=< relatedInformation) =<< caps
             pure $ buildDiagnostic Error loc error related
     else pure $ buildDiagnostic Error (toStart <$> loc) ("In" <++> pretty p <+> colon <++> error) Nothing
