{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Char              (toUpper)
import           Data.Default           (def)
import           Data.Digest.Pure.SHA   (sha1, showDigest)
import qualified Data.Text              as Text
import           Network.HTTP.Req

main :: IO ()
main = do
  pwd <- showDigest . sha1 <$> BL.getContents
  let pref = Text.pack (take 5 pwd)
  suffs <- parse . responseBody <$> getPwned pref
  if B.pack (toUpper <$> drop 5 pwd) `elem` suffs
     then putStrLn "pwned :/"
     else putStrLn "phew! :)"
  where
    parse = fmap (B.takeWhile (/= ':')) . B.lines
    getPwned pref = 
      let url = https "api.pwnedpasswords.com" /: "range" /: pref
       in runReq def (req GET url NoReqBody bsResponse mempty)

