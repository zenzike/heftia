{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect.Class (sendIns, type (<:), type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor)
import Control.Effect.Class.Machinery.TH (makeEffectF, makeEffectH)
import Control.Effect.Class.Reader (Ask, AskI, LocalS, ask, local)
import Control.Effect.Class.State (State (..), StateI, modify)
import Control.Effect.Freer (
    Fre,
    interpose,
    interpret,
    raise,
    runFreerEffects,
    type (<|),
 )
import Control.Effect.Handler.Heftia.Reader (interpretAsk, interpretReader, liftReader)
import Control.Effect.Handler.Heftia.State (evalState)
import Control.Effect.Heftia (
    Hef,
    elaborated,
    hoistHeftiaEffects,
    hoistInterpose,
    interposeH,
    interposeIns,
    interpretH,
    liftLowerH,
    type (<<|),
 )
import Control.Monad (when)
import Data.Function ((&))
import Data.Hefty.Sum (SumH)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.LogLevel (LogLevel (..))
import Prelude hiding (log)

class Log f where
    log :: LogLevel -> Text -> f ()
makeEffectF ''Log

logToIO ::
    (IO <: Fre r m, Ask LogLevel (Fre r m), Monad m) =>
    Fre (LogI ': r) m ~> Fre r m
logToIO =
    interpret \(Log level msg) -> do
        currentLevel <- ask
        when (level <= currentLevel) do
            sendIns $ T.putStrLn msg

class Time f where
    currentTime :: f UTCTime
makeEffectF ''Time

timeToIO :: (IO <: Fre r m, Monad m) => Fre (TimeI ': r) m ~> Fre r m
timeToIO = interpret \case
    CurrentTime -> sendIns getCurrentTime

logWithMetadata :: (LogI <| es, Time (Fre es m), Monad m) => Fre es m ~> Fre es m
logWithMetadata = interpose \(Log level msg) -> do
    t <- currentTime
    log level $ "[" <> T.pack (show level) <> " " <> T.pack (show t) <> "] " <> msg

-- | An effect that introduces a scope that represents a chunk of logs.
class LogChunk f where
    logChunk :: f a -> f a

makeEffectH ''LogChunk

-- | Output logs in log chunks as they are.
passthroughLogChunk ::
    HFunctor (SumH r) =>
    Hef (LogChunkS ': r) (Fre r' m) ~> Hef r (Fre r' m)
passthroughLogChunk = interpretH \(LogChunk m) -> m

-- | Limit the number of logs in a log chunk to the first @n@ logs.
limitLogChunk ::
    forall es es' m.
    (LogChunkS <<| es, LogI <| es', Monad m, HFunctor (SumH es)) =>
    Int ->
    Hef es (Fre es' m) ~> Hef es (Fre es' m)
limitLogChunk n =
    hoistHeftiaEffects (evalState 0)
        . interposeLogChunk
        . hoistHeftiaEffects raise
  where
    interposeLogChunk ::
        Hef es (Fre (StateI Int ': es') m)
            ~> Hef es (Fre (StateI Int ': es') m)
    interposeLogChunk = interposeH \(LogChunk a) ->
        logChunk $
            a & interposeIns \(Log level msg) -> liftLowerH do
                count <- get
                when (count <= n) do
                    if count == n
                        then log Info "LOG OMITTED..."
                        else log level msg
                    modify @Int (+ 1)

class FileSystem f where
    mkdir :: FilePath -> f ()
    writeFS :: FilePath -> String -> f ()

makeEffectF ''FileSystem

runDummyFS :: (IO <: Fre r m, Monad m) => Fre (FileSystemI ': r) m ~> Fre r m
runDummyFS = interpret \case
    Mkdir path -> sendIns $ putStrLn $ "<runDummyFS> mkdir " <> path
    WriteFS path content -> sendIns $ putStrLn $ "<runDummyFS> writeFS " <> path <> " : " <> content

saveLogChunk ::
    forall es es' m.
    ( LogChunkS <<| es
    , LogI <| es'
    , FileSystem (Fre es' m)
    , Time (Fre es' m)
    , Monad m
    , HFunctor (SumH es)
    ) =>
    Hef es (Fre es' m) ~> Hef es (Fre es' m)
saveLogChunk =
    interpretReader ("./log_chunks/" :: FilePath)
        . interposeLogChunk
        . liftReader
  where
    interposeLogChunk ::
        Hef (LocalS FilePath ': es) (Fre (AskI FilePath ': es') m)
            ~> Hef (LocalS FilePath ': es) (Fre (AskI FilePath ': es') m)
    interposeLogChunk =
        interposeH \(LogChunk a) -> logChunk do
            chunkBeginAt <- currentTime & raise & liftLowerH
            local @FilePath (++ iso8601Show chunkBeginAt ++ "/") do
                newLogChunkDir <- ask & liftLowerH
                mkdir newLogChunkDir & raise & liftLowerH
                a & hoistInterpose \(Log level msg) -> do
                    logAt <- currentTime & raise
                    saveDir <- ask
                    writeFS (saveDir ++ iso8601Show logAt ++ ".log") (show (level, msg)) & raise
                    log level msg

main :: IO ()
main = runFreerEffects
    . interpretAsk Debug
    . logToIO
    . timeToIO
    . logWithMetadata
    . runDummyFS
    . elaborated
    . passthroughLogChunk
    . saveLogChunk
    . limitLogChunk 2
    $ do
        logChunk do
            log Warn "WARNING!!"
            log Error "ERROR!!"
            log Info "HELLO"
            log Debug "Would you like some coffee?"
