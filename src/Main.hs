module Main where

import Data.Foldable
import Network.HTTP.Client
import Network.HTTP.Client.Internal
import Network.HTTP.Conduit
import Conduit
import UnliftIO.Resource (liftResourceT)
import UnliftIO.Concurrent (forkIO)

import qualified Data.ByteString.Lazy.Char8 as BSC

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
        { managerResponseTimeout = responseTimeoutMicro 5000000
        , managerRawConnection = do
              mkConn <- managerRawConnection defaultManagerSettings
              pure $ \ha s i -> onDestroy (putStrLn "destroying") <$> mkConn ha s i
        }
    runResourceT $ for_ [(1::Int) ..] $ \i -> do
        _ <- liftIO getLine
        initialRequest <- parseRequest "POST http://localhost:12777"
        let request = initialRequest
                { requestBody = RequestBodyLBS (BSC.pack (show i))
                }
        forkIO $ do
            response <- liftResourceT (http request manager)
            liftResourceT (responseBody response `connect` sinkNull)

-- | Execute an action when a 'Connection' gets destroyed.
onDestroy :: IO () -> Connection -> Connection
onDestroy act conn = conn { connectionClose = act >> connectionClose conn }
