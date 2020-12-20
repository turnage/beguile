module Lib
  ( beguile
  ) where

import Data.Binary.Builder
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

errorResponse :: Response
errorResponse = responseBuilder status400 [] empty

okResponse :: Response
okResponse = responseBuilder status200 [] empty

app :: Application
app request respond =
  if requestMethod request == methodConnect
    then respond okResponse
    else respond errorResponse

beguile :: IO ()
beguile = run 3000 app
