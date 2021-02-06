{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Server.Log where

import System.IO (hPrint, stderr)
import Data.Text (Text)

class Monad m => Logging m where
    printStdout :: Show a => a -> m ()
    printStderr :: Show a => a -> m ()

    logInfo :: Show a => a -> m ()
    logInfo = printStdout

    logInfoT :: Text -> m ()
    logInfoT = logInfo

    logError :: Show a => a -> m ()
    logError = printStderr

    logErrorT :: Text -> m ()
    logErrorT = logError

instance Logging IO where
    printStdout = print
    printStderr = hPrint stderr
