module Language.LSP.CodeAction.Utils

import Core.Core
import Core.Context.Context
import Language.LSP.CodeAction
import Language.LSP.Message.CodeAction
import Language.LSP.Message.Location
import Server.Configuration
import Server.Log
import Server.Utils

||| Check for code actions that require the request to be on a single line.
||| @topic Logging topic of the code action
||| @params Code action params
||| @onErr Default value returned in case of error
||| @onOk Function that handles the single line
export
withSingleLine : Ref LSPConf LSPConfiguration
              => (topic : Topic)
              -> (params : CodeActionParams)
              -> (onErr : Core r)
              -> (onOk : Int -> Core r)
              -> Core r
withSingleLine topic params onErr onOk =
  let startLine = params.range.start.line
      endLine = params.range.end.line
   in case startLine == endLine of
           True => onOk startLine
           False => do logW topic "The action requires a single line but provided the range \{show startLine} - \{show endLine}"
                       onErr

||| Check for a single cached code action for the given request type.
||| @topic Logging topic of the code action
||| @params Code action params
||| @action Tag of the cached code action
||| @new Handler for cache misses
export
withSingleCache : Ref LSPConf LSPConfiguration
               => (topic : Topic)
               -> (params : CodeActionParams)
               -> (action : IdrisAction)
               -> (new : Core (Maybe (Range, CodeAction)))
               -> Core (Maybe CodeAction)
withSingleCache topic params action new =
  case !(searchCache params.range action) of
       [] => do Just (loc, act) <- new
                  | Nothing => pure Nothing
                update LSPConf ({ cachedActions $= insert (loc, action, [act]) })
                pure $ Just act
       act :: _ => do logD topic "Found cached action"
                      pure $ Just act

||| Check for a multiple cached code actions for the given request type.
||| @topic Logging topic of the code action
||| @params Code action params
||| @action Tag of the cached code action
||| @new Handler for cache misses
export
withMultipleCache : Ref LSPConf LSPConfiguration
                 => (topic : Topic)
                 -> (params : CodeActionParams)
                 -> (action : IdrisAction)
                 -> (new : Core (List (Range, List CodeAction)))
                 -> Core (List CodeAction)
withMultipleCache topic params action new =
  case !(searchCache params.range action) of
       [] => do locs <- new
                for_ locs $ \(loc, acts) =>
                  update LSPConf ({ cachedActions $= insert (loc, action, acts) })
                pure $ concat $ snd <$> locs
       acts => do logD topic "Found cached action"
                  pure acts
