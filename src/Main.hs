{-# LANGUAGE OverloadedStrings #-}

module Main where

import Proto.GoogleAuthMigration_Fields (secret,name,issuer,algorithm,digits,type',counter)
import Proto.GoogleAuthMigration (MigrationPayload(..),Algorithm(..),OtpType(..),DigitCount(..))
import Data.ProtoLens (defMessage,showMessage)
import Data.ProtoLens.Encoding (decodeMessage)
import Lens.Micro
import Text.URI (mkQueryKey,mkURI)
import Text.URI.Lens (queryParam,uriQuery,unRText)
import Data.Text (Text(..))
import Data.ByteString (ByteString(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T (encodeUtf8,decodeUtf8)
import qualified Codec.Binary.Base64 as Base64 (decode)
import qualified Codec.Binary.Base32 as Base32 (encode)

getMigrationData migrationURI = do uri <- mkURI migrationURI
                                   dataKey <- mkQueryKey "data"
                                   b64bs <- T.encodeUtf8 <$> uri ^? uriQuery . queryParam dataKey . unRText
                                   either mempty return $ Base64.decode b64bs

decodeMigrationPayload :: ByteString -> Maybe MigrationPayload
decodeMigrationPayload bs = either (const Nothing) return $ decodeMessage bs

main :: IO ()
main = undefined


