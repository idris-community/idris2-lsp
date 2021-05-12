||| Implementation of a Idris2 LSP Server.
|||
||| (C) The Idris Community, 2021
module Server.Main

import Core.Context
import Core.Core
import Core.InitPrimitives
import Core.Metadata
import Core.UnifyState
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

runServer : Ref LSPConf LSPConfiguration
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref MD Metadata
         => Ref ROpts REPLOpts
         => Core ()
runServer = do
  inputHandle <- gets LSPConf inputHandle
  Right (Just l) <- parseHeaderPart inputHandle
    | _ => do logString Error "Error while parsing header part"
              sendUnknownResponseMessage parseError
              runServer
  Right msg <- coreLift $ fGetChars inputHandle l
    | Left err => do logShow Error (show err)
                     sendUnknownResponseMessage (internalError "Error while recovering the content part of a message")
                     runServer
  logString Debug msg
  let Just msg = parse msg
    | Nothing => do logString Error "Error while parsing content"
                    sendUnknownResponseMessage parseError
                    runServer
  let Just (Client ** type ** method ** msg) = the (Maybe (from ** type ** method : Method from type ** Message type method)) $ fromJSON msg
    | Just (Server ** type ** method ** msg) => do
        logString Warning "Received server message"
        sendUnknownResponseMessage (invalidRequest "Received a message meant to be sent by the server")
        runServer
    | Nothing => do logString Error "Error while parsing message"
                    sendUnknownResponseMessage parseError
                    runServer
  logString Info $ "Received message for method " ++ show (toJSON method)
  catch (processMessage method msg)
        (\err => do
          logString Error (show err)
          resetContext "(interactive)")
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
              m <- newRef MD (initMetadata "(interactive)")
              runServer)
    (\err => fPutStrLn stderr ("CRITICAL UNCAUGHT ERROR " ++ show err) *> exitWith (ExitFailure 1))
    (\res => pure ())
