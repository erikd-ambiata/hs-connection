{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.Async
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe (fromMaybe)
import           Network.Connection
import           System.Exit

main :: IO ()
main = do
    _ <- forConcurrently [1 .. 8] $ \ x ->
            forM_ [1 .. 10000] $
                runTest (if even x then httpConn else httpsConn) x
    putStrLn "Done"


httpConn :: ConnectionParams
httpConn = ConnectionParams
    { connectionHostname  = "localhost"
    , connectionPort      = 2000
    , connectionUseSecure = Nothing
    , connectionUseSocks  = Nothing
    }

httpsConn :: ConnectionParams
httpsConn = ConnectionParams
    { connectionHostname  = "localhost"
    , connectionPort      = 3000
    , connectionUseSecure = Just $ TLSSettingsSimple disableCertificateValidation False False
    , connectionUseSocks  = Nothing
    }
    where
        disableCertificateValidation = True

runTest :: ConnectionParams -> Int -> Int -> IO ()
runTest params x y = do
    ctx <- initConnectionContext
    con <- connectTo ctx params
    connectionPut con $ getPlus x y
    resp <- connectionGet con 4096
    unless (validateResponse resp (== bsShow (x + y))) $ do
        print resp
        exitFailure
    connectionClose con


getPlus :: Int -> Int -> ByteString
getPlus x y = BS.concat
    [ "GET /plus/", bsShow x, "/", bsShow y, " HTTP/1.0\r\n\r\n" ]


bsShow :: Show a => a -> ByteString
bsShow = BS.pack . show

validateResponse :: ByteString -> (ByteString -> Bool) -> Bool
validateResponse bs validate =
    let bss = filter (not . BS.null) $ BS.lines bs in
    all (== True)
        [ isHttp200 bs
        , hasServerType bss "HTTP"
        , validate (fromMaybe "Nothing" $ getResult bss)
        ]

isHttp200 :: ByteString -> Bool
isHttp200 = BS.isPrefixOf "HTTP/1.0 200 OK"

hasServerType :: [ByteString] -> ByteString -> Bool
hasServerType bss expected =
    case filter (BS.isPrefixOf "X-server-type") bss of
        [x] -> expected `BS.isInfixOf` x
        _ -> False

getResult :: [ByteString] -> Maybe ByteString
getResult bss =
    case filter (BS.isPrefixOf "Result:") bss of
        [x] -> Just . BS.dropWhile (== ' ') $ BS.dropWhile (/= ' ') x
        _ -> Nothing
