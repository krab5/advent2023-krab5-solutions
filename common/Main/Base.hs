module Main.Base where

import MonadResult
import System.Environment
import System.Exit
import System.CPUTime
import Control.Monad
import Control.Exception
import Control.DeepSeq
import Text.Printf

type Logger = String -> IO ()
type Processor = Logger -> String -> IO ()

getTime :: Integer -> Integer -> Double
getTime start stop = ((fromInteger stop) - (fromInteger start)) / 1.0e9

read1File :: Processor -> Int -> String -> IO Int
read1File process1 k fn = do
    printf "[%d] Processing file %s...\n" k fn
    result <- try $ readFile fn
    case result of
      Left (exc :: SomeException) -> printf "[%d] Exception raised while reading file: %s\n[%d] Ignoring...\n" k (displayException exc) k
      Right ct -> do
        start <- getCPUTime
        r <- (process1 (\x -> printf "[%d] %s\n" k x) ct)
        stop <- r `deepseq` getCPUTime
        printf "[%d] Done [processing time: %g ms]\n" k (getTime start stop)
    return $ k + 1

doMain :: Processor -> IO ()
doMain process1 = do
    args <- getArgs
    n <- foldM (read1File process1) 1 args
    printf "Done [processed %d files].\n" (n - 1)
    return ()

doMainPre :: MonadResult m => (Logger -> String -> IO (m a)) -> (Logger -> m a -> IO ()) -> (Logger -> a -> IO ()) -> IO ()
doMainPre preProc preErr doProcess = do
    args <- getArgs
    n <- foldM do1 1 args
    printf "Done [processed %d files].\n" (n - 1)
    return ()
    where do1 :: Int -> String -> IO Int
          do1 k fn = 
            let logger = \x -> putStrLn ("[" ++ show k ++ "] " ++ x) in do 
              logger (printf "Processing file %s..." fn)
              result <- try $ readFile fn
              case result of
                Left (exc :: SomeException) -> do
                    logger (printf "Exception raised while reading file: %s" (displayException exc))
                    logger "Ignoring..." 
                Right ct -> do
                    procstart <- getCPUTime
                    res <- preProc logger ct
                    if isFailed res
                        then do
                            preErr logger res
                            logger "Preprocessing failed; aborting."
                        else let extracted = extract res in do
                                r <- doProcess logger extracted
                                procstop <- r `deepseq` getCPUTime
                                logger (printf "Done [processing time: %g ms]" (getTime procstart procstop))
              return $ k + 1

                        




