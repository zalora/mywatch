{-# LANGUAGE OverloadedStrings #-}

module LogFormat (
  logFormat
) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Types (Status(statusCode))
import Network.Wai (Request, httpVersion, requestHeaders, requestMethod,
  rawPathInfo, requestHeaderReferer, requestHeaderUserAgent)
import System.Log.FastLogger (LogStr, toLogStr)
import qualified Data.ByteString.Char8 as BS

-- Sligthly modified Common Log Format.
-- User ID extracted from the From header.
logFormat :: BS.ByteString -> Request -> Status -> Maybe Integer -> LogStr
logFormat t req st msize = ""
  <> toLogStr (fromMaybe "-" $ lookup "X-Forwarded-For" headers)
  <> " - "
  <> toLogStr (fromMaybe "-" $ lookup "From" headers)
  <> " ["
  <> toLogStr t
  <> "] \""
  <> toLogStr (requestMethod req)
  <> " "
  <> toLogStr (rawPathInfo req)
  <> " "
  <> toLogStr (show $ httpVersion req)
  <> "\" "
  <> toLogStr (show $ statusCode st)
  <> " "
  <> toLogStr (maybe "-" show msize)
  <> " \""
  <> toLogStr (fromMaybe "" $ requestHeaderReferer req)
  <> "\" \""
  <> toLogStr (fromMaybe "" $ requestHeaderUserAgent req)
  <> "\"\n"
  where headers = requestHeaders req

