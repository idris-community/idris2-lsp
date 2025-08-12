module Language.LSP.SignatureHelp

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Data.List
import Data.String
import Idris.Doc.String
import Idris.REPL.Opts
import Idris.Syntax
import Language.LSP.Message
import Libraries.Data.PosMap
import Server.Configuration
import Server.Utils
import Server.Log

-- Color codes in JSON messages makes the JSON parser fail, we have to
-- turn temporarly the syntax highlight and coloring off when asking for
-- for documentation.
getNonColoredDocsForName : Ref Ctxt Defs
                        => Ref ROpts REPLOpts
                        => Ref Syn SyntaxInfo
                        => FC -> Name -> Core String
getNonColoredDocsForName fc n = do
  ropts <- get ROpts
  put ROpts ({ color := False, synHighlightOn := False } ropts)
  docs <- getDocsForName fc n MkConfig
  put ROpts ropts
  pure $ show docs

-- Find the unique Name in the type term for the function return type which represent
-- the datatype. This function should be called only for a closed term of a DCon.
findRefNamesInTerm : Term vars -> List Name
findRefNamesInTerm (Local fc isLet idx p) = []
findRefNamesInTerm (Ref fc x name)        = [name]
findRefNamesInTerm (Meta fc x y xs)       = concatMap findRefNamesInTerm xs
findRefNamesInTerm (Bind fc x b scope)    = findRefNamesInTerm scope
findRefNamesInTerm (App fc fn arg)        = findRefNamesInTerm fn ++ findRefNamesInTerm arg
findRefNamesInTerm (As fc x as pat)       = findRefNamesInTerm as ++ findRefNamesInTerm pat
findRefNamesInTerm (TDelayed fc x y)      = findRefNamesInTerm y
findRefNamesInTerm (TDelay fc x ty arg)   = findRefNamesInTerm ty
findRefNamesInTerm (TForce fc x y)        = findRefNamesInTerm y
findRefNamesInTerm (PrimVal fc c)         = []
findRefNamesInTerm (Erased fc imp)        = []
findRefNamesInTerm (TType fc u)           = []

renderDataTypeInfo : Ref Ctxt Defs
                  => Ref ROpts REPLOpts
                  => Ref Syn SyntaxInfo
                  => Name -> Def -> Core (Maybe String)
renderDataTypeInfo n d@(DCon tag arity newtypeArg) = do
  -- Render the related whole data structure information for the selected data constructor.
  -- Helps to understand code, when we have an easy way to look at all the constructors.
  context <- gets Ctxt gamma
  fullName <- getFullName n
  case !(lookupCtxtExact fullName context) of
    Nothing => pure Nothing
    Just df => case findRefNamesInTerm (getFn df.type) of
      [rn] => do
        fullName <- getFullName rn
        case !(lookupDefName fullName context) of
          -- Render the TCon for this DCon
          [(defName, _, tcon@(TCon {}))] => renderDataTypeInfo defName tcon
          _ => pure Nothing
      _ => pure Nothing
renderDataTypeInfo n d@(TCon arity parampos detpos flags mutwith datacons detagabbleBy) = do
  -- Render the data structure information for the type.
  context <- gets Ctxt gamma
  constructors <- for (fromMaybe [] datacons) $ \dn => case !(lookupDefName dn context) of
    [(_, _, DCon {})] => do
      fullName <- getFullName dn
      pure $ Just " | \{show fullName} ..."
    _ => pure Nothing
  pure $ Just $ String.unlines (show n :: catMaybes constructors)
renderDataTypeInfo n other = pure Nothing

||| Generate the signature help for a callable function. In Idris many many things are
||| callable functions.
export
signatureHelp : Ref Ctxt Defs
             => Ref MD Metadata
             => Ref LSPConf LSPConfiguration
             => Ref ROpts REPLOpts
             => Ref Syn SyntaxInfo
             => SignatureHelpParams -> Core (Maybe SignatureHelp)
signatureHelp params =
  catch (do logI GotoDefinition "Checking for \{show params.textDocument.uri}"
            let line = params.position.line
            let col = params.position.character
            nameLocs <- gets MD nameLocMap
            let Just ((fname, nstart, nend), name) = findPointInTreeLoc (line, col) nameLocs
              | Nothing => logD SignatureHelp "No name found at \{show line}:\{show col}}" >> pure Nothing

            -- Check of the name is a local name.
            localResult <- findTypeAt $ anyWithName name $ within (line, col)
            docs <- getNonColoredDocsForName (MkFC fname nstart nend) name

            -- Generate the data type information if it is not in the generated doc already.
            -- When there is no Constructor info, the data structure information is hidden
            -- and only partial information can be retrieved.
            dataTypeInformation <-
              if "Constructors:" `isInfixOf` docs
                then pure []
                else do
                  defs <- lookupDefName name !(gets Ctxt gamma)
                  infos <- for defs $ (\(n, _, d) => do
                    infoStr <- renderDataTypeInfo n d
                    pure $ map (\str => ("Data structure of", str)) infoStr)
                  pure $ catMaybes infos

            -- Use one signature block, as it seems the nvim client doesn't render more than one... :(
            let signatureDocs : List (String, String)
                signatureDocs = case localResult of
                  Just (n, _, t) => [("Local", "\{show n} : \{show t}"), ("Shadowing", docs)]
                  Nothing => [("Global definition(s)", docs)]

            let signatures =
                  MkSignatureInformation
                    { label = show name
                    , documentation
                        = Just $ make
                        $ concatMap
                            (\(header, body) => "\n" ++ header ++ "\n" ++ body ++ "\n")
                            (signatureDocs ++ dataTypeInformation)
                    , parameters_ = Nothing
                    , activeParameter = Nothing
                    }
            pure $ Just $ MkSignatureHelp
              { signatures = [signatures]
              , activeSignature = Nothing
              , activeParameter = Nothing
              })
  (\err => pure Nothing)
