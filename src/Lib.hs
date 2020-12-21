module Lib
  ( beguile
  ) where

import Control.Concurrent.Async (race_)
import Data.Binary.Builder (empty)
import Data.ByteString (ByteString, split)
import Data.ByteString.Conversion.From (fromByteString)
import Data.Char (ord)
import Data.Conduit (ConduitT, connect)
import Data.Conduit.Network
  ( ClientSettings
  , appSink
  , appSource
  , clientSettings
  , runTCPClient
  )
import Data.Void (Void)
import Data.Word (Word16, Word8)
import Network.HTTP.Types (methodConnect, status200, status400, status500)
import Network.Wai
  ( Application
  , Request
  , Response
  , ResponseReceived
  , rawPathInfo
  , requestMethod
  , responseBuilder
  )
import Network.Wai.Conduit (responseRawSource)
import Network.Wai.Handler.Warp (run)
import Options (Options(..), runCommand, simpleOption)

data BeguileOptions = BeguileOptions
  { optPort :: Word16
  }

instance Options BeguileOptions where
  defineOptions = pure BeguileOptions <*> simpleOption "p" 3000 "Port"

beguile :: IO ()
beguile =
  runCommand $ \options _ -> do run (fromIntegral $ optPort $ options) app

app :: Application
app request respond =
  case parseProxyRequest request >>= parseDestinationAddress of
    Just clientSettingsForDestination -> do
      let serve = proxyClient clientSettingsForDestination respond
      let unendingTCPProxyTask = responseRawSource serve badServerResponse
      respond unendingTCPProxyTask
    Nothing -> respond badRequestResponse

proxyClient ::
     ClientSettings
  -> (Response -> IO ResponseReceived)
  -> (ConduitT () ByteString IO () -> ConduitT ByteString Void IO () -> IO ())
proxyClient clientSettings respond fromClient toClient = do
  respond okResponse
  runTCPClient clientSettings serve
  where
    serve connectionToDestination = do
      let fromDestination = appSource connectionToDestination
      let toDestination = appSink connectionToDestination
      let clientToDestination = connect fromClient toDestination
      let destinationToClient = connect fromDestination toClient
      race_ clientToDestination destinationToClient

parseProxyRequest :: Request -> Maybe ByteString
parseProxyRequest request =
  if requestMethod request == methodConnect
    then Just $ rawPathInfo request
    else Nothing

parseDestinationAddress :: ByteString -> Maybe ClientSettings
parseDestinationAddress path = do
  (host, portString) <- destinationAddressParts path
  port <- fromByteString portString :: Maybe Word16
  Just $ clientSettings (fromIntegral port) host

destinationAddressParts :: ByteString -> Maybe (ByteString, ByteString)
destinationAddressParts path =
  case split addressPartSeparator path of
    [host, port] -> Just (host, port)
    _ -> Nothing

addressPartSeparator :: Word8
addressPartSeparator = fromIntegral $ ord $ ':'

badServerResponse :: Response
badServerResponse = responseBuilder status500 [] empty

badRequestResponse :: Response
badRequestResponse = responseBuilder status400 [] empty

okResponse :: Response
okResponse = responseBuilder status200 [] empty
