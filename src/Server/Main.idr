||| Implementation of a Idris2 LSP Server.
|||
||| (C) The Idris Community, 2021
module Server.Main

import Core.Context
import Core.Core
import Core.InitPrimitives
import Core.Metadata
import Core.UnifyState
import Compiler.Common
import Data.List1
import Data.String
import Idris.CommandLine
import Idris.Env
import Idris.REPL.Opts
import Idris.REPL.Common
import Idris.Package.Types
import Idris.SetOptions
import Idris.Syntax
import Idris.Version
import IdrisPaths
import Language.JSON
import Language.LSP.Message
import Libraries.Utils.Path
import Server.Configuration
import Server.Log
import Server.ProcessMessage
import Server.Response
import Server.Utils
import System
import System.Directory
import System.File

data Header = ContentLength Int | ContentType String | StartContent

Show Header where
  show (ContentLength l) = "Content-Length: " ++ show l
  show (ContentType s) = "Content-Type: " ++ s
  show StartContent = "stop"

headerPart : List Header -> String
headerPart [] = ""
headerPart (ContentLength l :: xs) = "Content-Length: " ++ show l ++ "\n" ++ headerPart xs
headerPart (ContentType s :: xs) = "Content-Type: " ++ s ++ "\n" ++ headerPart xs
headerPart (StartContent :: _) = "\n"

parseHeader : String -> Maybe Header
parseHeader "\r\n" = Just StartContent
parseHeader str =
  if "Content-Length:" `isPrefixOf` str
     then let (_ ::: xs) = split (== ':') str in
              ContentLength <$> parseInteger (fastAppend xs)
     else if "Content-Type:" `isPrefixOf` str
             then let (_ ::: xs) = split (== ':') str in
                      Just $ ContentType (fastAppend xs)
             else Nothing

parseHeaderPart : (h : File) -> Core (Either FileError (Maybe Int))
parseHeaderPart h = do
  Right line <- fGetHeader h
    | Left err => pure $ Left err
  case parseHeader line of
    Just (ContentLength l) => parseHeaderPart h *> pure (Right (Just l))
    Just (ContentType s) => parseHeaderPart h
    Just StartContent => pure $ Right Nothing
    Nothing => pure $ Right Nothing

handleMessage : Ref LSPConf LSPConfiguration
            => Ref Ctxt Defs
            => Ref UST UState
            => Ref Syn SyntaxInfo
            => Ref MD Metadata
            => Ref ROpts REPLOpts
            => Core ()
handleMessage = do
  inputHandle <- gets LSPConf inputHandle
  Right (Just l) <- parseHeaderPart inputHandle
    | _ => do logD Channel "Cannot parse message header"
              sendUnknownResponseMessage parseError
  Right msg <- coreLift $ fGetChars inputHandle l
    | Left err => do
        logE Server "Cannot retrieve body of message: \{show err}"
        sendUnknownResponseMessage $ internalError "Error while recovering the content part of a message"
  logD Channel "Received message: \{msg}"
  let Just msg = parse msg
    | _ => do logE Channel "Cannot parse message"
              sendUnknownResponseMessage parseError
  let JObject fields = msg
    | _ => do logE Channel "Message is not a JSON object"
              sendUnknownResponseMessage $ invalidRequest "Message is not object"
  let Just (JString "2.0") = lookup "jsonrpc" fields
    | _ => do logE Channel "Message has no jsonrpc field"
              sendUnknownResponseMessage (invalidRequest "jsonrpc is not \"2.0\"")
  case lookup "method" fields of
    Just methodJSON => do -- request or notification
      case lookup "id" fields of
        Just idJSON => do -- request
          let Just id = fromJSON {a=OneOf [Int, String]} idJSON
            | _ => do logE Channel "Message id is not of the correct type"
                      sendUnknownResponseMessage (invalidRequest "id is not int or string")
          let Just method = fromJSON {a=Method Client Request} methodJSON
            | _ => do logE Channel "Method not found"
                      sendResponseMessage Initialize $ Failure (extend id) methodNotFound
          logI Channel "Received request for method \{show (toJSON method)}"
          let Just params = fromMaybeJSONParameters method (lookup "params" fields)
            | _ => do logE Channel "Message with method \{show (toJSON method)} has invalid parameters"
                      sendResponseMessage method $ Failure (extend id) (invalidParams "Invalid params for send \{show methodJSON}")
          -- handleRequest can be modified to use a callback if needed
          result <- catch (handleRequest method params) $ \err => do
            logE Server "Error while handling request: \{show err}"
            resetContext (Virtual Interactive)
            pure $ Left (MkResponseError (Custom 4) (show err) JNull)
          sendResponseMessage method $ case result of
            Left error => Failure (extend id) error
            Right result => Success (extend id) result

        Nothing => do -- notification
          let Just method = fromJSON {a=Method Client Notification} methodJSON
            | _ => do logE Channel "Method not found"
                      sendUnknownResponseMessage methodNotFound
          logI Channel "Received notification for method \{show (toJSON method)}"
          let Just params = fromMaybeJSONParameters method (lookup "params" fields)
            | _ => do logE Channel "Message with method \{show (toJSON method)} has invalid parameters"
                      sendUnknownResponseMessage $ invalidParams "Invalid params for send \{show methodJSON}"
          catch (handleNotification method params) $ \err => do
            logE Server "Error while handling notification: \{show err}"
            resetContext (Virtual Interactive)

    Nothing => do -- response
      let Just idJSON = lookup "id" fields
        | _ => do logE Channel "Received message with neither method nor id"
                  sendUnknownResponseMessage (invalidRequest "Message does not have method or id")
      logW Server "Ignoring response with id \{show idJSON}"

runServer : Ref LSPConf LSPConfiguration
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref MD Metadata
         => Ref ROpts REPLOpts
         => Core ()
runServer = handleMessage >> runServer

main : IO ()
main = do
  coreRun (do l <- newRef LSPConf defaultConfig
              defs <- initDefs
              c <- newRef Ctxt defs
              s <- newRef Syn initSyntax
              addPrimitives
              bprefix <- coreLift $ idrisGetEnv "IDRIS2_PREFIX"
              the (Core ()) $ case bprefix of
                   Just p => setPrefix p
                   Nothing => setPrefix yprefix
              bpath <- coreLift $ idrisGetEnv "IDRIS2_PATH"
              the (Core ()) $ case bpath of
                   Just path => do traverseList1_ addExtraDir (map trim (split (==pathSeparator) path))
                   Nothing => pure ()
              bdata <- coreLift $ idrisGetEnv "IDRIS2_DATA"
              the (Core ()) $ case bdata of
                   Just path => do traverseList1_ addDataDir (map trim (split (==pathSeparator) path))
                   Nothing => pure ()
              blibs <- coreLift $ idrisGetEnv "IDRIS2_LIBS"
              the (Core ()) $ case blibs of
                   Just path => do traverseList1_ addLibDir (map trim (split (==pathSeparator) path))
                   Nothing => pure ()
              pdirs <- coreLift $ idrisGetEnv "IDRIS2_PACKAGE_PATH"
              the (Core ()) $ case pdirs of
                   Just path => do traverseList1_ addPackageDir (map trim (split (==pathSeparator) path))
                   Nothing => pure ()
              cg <- coreLift $ idrisGetEnv "IDRIS2_CG"
              the (Core ()) $ case cg of
                   Just e => case getCG (options defs) e of
                                  Just cg => setCG cg
                                  Nothing => throw (InternalError ("Unknown code generator " ++ show e))
                   Nothing => pure ()
              addPkgDir "prelude" anyBounds
              addPkgDir "base" anyBounds
              addDataDir (prefix_dir (dirs (options defs)) </> ("idris2-" ++ showVersion False version) </> "support")
              addLibDir (prefix_dir (dirs (options defs)) </> ("idris2-" ++ showVersion False version) </> "lib")
              Just cwd <- coreLift $ currentDir
                | Nothing => throw (InternalError "Can't get current directory")
              addLibDir cwd
              o <- newRef ROpts (Opts.defaultOpts Nothing (REPL False) [])
              u <- newRef UST initUState
              m <- newRef MD (initMetadata (Virtual Interactive))
              runServer)
    (\err => fPutStrLn stderr ("CRITICAL UNCAUGHT ERROR " ++ show err) *> exitWith (ExitFailure 1))
    (\res => pure ())
