||| input reader that can run while completions are building
||| so that the server can read messages without waiting
||| for the current operation to complete
module Server.Input

import Data.IORef
import Data.List
import Data.Buffer
import System.Concurrency

%default total

export
data InputPort : Type where [external]

||| backend's input port
%foreign "scheme,chez:standard-input-port"
         "scheme,racket:current-input-port"
newInputPort : PrimIO InputPort

export
openInput : IO InputPort
openInput = primIO newInputPort

||| binds call to chez's read-u8 or racket's read-byte to read a byte from the input port
%foreign "scheme,chez:(lambda (port) (let ((b (get-u8 port))) (if (eof-object? b) -1 b)))"
         "scheme,racket:(lambda (port) (let ((b (read-byte port))) (if (eof-object? b) -1 b)))"
readByte : InputPort -> PrimIO Int

export covering
readInputLine : InputPort -> IO String
readInputLine port = go []
  where
    go : List Char -> IO String
    go chars = do
      b <- primIO $ readByte port
      if b < 0 then pure ""
        else if b == 10 then pure $ pack $ reverse ('\n' :: chars)
        else go (chr b :: chars)

export covering
readInputBody : InputPort -> Int -> IO (Maybe String)
readInputBody port size = do
  Just buffer <- newBuffer size
    | Nothing => pure Nothing
  readBody buffer 0
  where
    readBody : Buffer -> Int -> IO (Maybe String)
    readBody buffer offset =
      if offset == size then Just <$> getString buffer 0 size
      else do
        b <- primIO $ readByte port
        if b < 0 then pure Nothing else do
          setBits8 buffer offset (cast b)
          readBody buffer (offset + 1)

||| only the reader thread and the server loop share this queue
||| compiler state stays on the server thread
export
record Inbox where
  constructor MkInbox
  lock : Mutex
  messages : IORef (List (Either String String), List (Either String String))

export
newInbox : IO Inbox
newInbox = do
  lock <- makeMutex
  messages <- newIORef ([], [])
  pure $ MkInbox lock messages

export
enqueue : Inbox -> Either String String -> IO ()
enqueue inbox message = atomically inbox.lock $ do
  (front, back) <- readIORef inbox.messages
  writeIORef inbox.messages (front, message :: back)

export
dequeue : Inbox -> IO (Maybe (Either String String))
dequeue inbox = do
  mutexAcquire inbox.lock
  (front, back) <- readIORef inbox.messages
  let pending = if null front then reverse back else front
  let rest = if null front then [] else back
  result <- case pending of
    [] => pure Nothing
    message :: messages => do
      writeIORef inbox.messages (messages, rest)
      pure $ Just message
  mutexRelease inbox.lock
  pure result

export
hasInput : Inbox -> IO Bool
hasInput inbox = do
  mutexAcquire inbox.lock
  (front, back) <- readIORef inbox.messages
  mutexRelease inbox.lock
  pure $ not (null front && null back)
