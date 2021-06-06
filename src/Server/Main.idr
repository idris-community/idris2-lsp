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
import Data.Strings
import Idris.CommandLine
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
headerPart (ContentLength l :: xs) = "Content-Length: " ++ show l ++ "\r\n" ++ headerPart xs
headerPart (ContentType s :: xs) = "Content-Type: " ++ s ++ "\r\n" ++ headerPart xs
headerPart (StartContent :: _) = "\r\n"

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
    | _ => sendUnknownResponseMessage parseError
  Right msg <- coreLift $ fGetChars inputHandle l
    | Left err => do
      logShow Error (show err)
      sendUnknownResponseMessage (internalError "Error while recovering the content part of a message")
  logString Debug msg
  let Just msg = parse msg
    | _ => sendUnknownResponseMessage parseError
  let JObject fields = msg
    | _ => sendUnknownResponseMessage (invalidRequest "Message is not object")
  let Just (JString "2.0") = lookup "jsonrpc" fields
    | _ => sendUnknownResponseMessage (invalidRequest "jsonrpc is not \"2.0\"")
  case lookup "method" fields of
    Just methodJSON => do -- request or notification
      case lookup "id" fields of
        Just idJSON => do -- request
          let Just id = fromJSON {a=OneOf [Int, String]} idJSON
            | _ => sendUnknownResponseMessage (invalidRequest "id is not int or string")
          let Just method = fromJSON {a=Method Client Request} methodJSON
            | _ => sendResponseMessage Initialize $ Failure (extend id) methodNotFound
          logString Info $ "Received request for method " ++ show (toJSON method)
          let Just params = fromMaybeJSONParameters method (lookup "params" fields)
            | _ => sendResponseMessage method $ Failure (extend id) (invalidParams "Invalid params for send \{show methodJSON}")
          -- handleRequest can be modified to use a callback if needed
          result <- catch (handleRequest method params) $ \err => do
            logString Error (show err)
            resetContext (Virtual Interactive)
            pure $ Left (MkResponseError (Custom 4) (show err) JNull)
          sendResponseMessage method $ case result of
            Left error => Failure (extend id) error
            Right result => Success (extend id) result

        Nothing => do -- notification
          let Just method = fromJSON {a=Method Client Notification} methodJSON
            | _ => sendUnknownResponseMessage methodNotFound
          logString Info $ "Received notification for method " ++ show (toJSON method)
          let Just params = fromMaybeJSONParameters method (lookup "params" fields)
            | _ => sendUnknownResponseMessage (invalidParams "Invalid params for send \{show methodJSON}")
          catch (handleNotification method params) $ \err => do
            logString Error (show err)
            resetContext (Virtual Interactive)

    Nothing => do -- response
      let Just idJSON = lookup "id" fields
        | _ => sendUnknownResponseMessage (invalidRequest "Message does not have method or id")
      logString Warning "Ignoring response with id \{show idJSON}"

runServer : Ref LSPConf LSPConfiguration
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref MD Metadata
         => Ref ROpts REPLOpts
         => Core ()
runServer = do
  handleMessage
  runServer

main : IO ()
main = do
  coreRun (do l <- newRef LSPConf defaultConfig
              defs <- initDefs
              c <- newRef Ctxt defs
              s <- newRef Syn initSyntax
              addPrimitives
              setPrefix yprefix
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
