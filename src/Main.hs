{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Proto.GoogleAuthMigration_Fields (algorithm,configs,counter,digits,issuer,name,otpType,secret)
import Proto.GoogleAuthMigration (Algorithm(..),DigitCount(..),MigrationPayload(..),OTPConfig(..),OTPType(..))
import Data.ProtoLens (defMessage,showMessage)
import Data.ProtoLens.Encoding (decodeMessage)
import Data.Maybe (fromMaybe)
import Lens.Micro
import Text.URI (emptyURI,isPathAbsolute,mkHost,mkPathPiece,mkQueryKey,mkQueryKey,mkQueryValue,mkScheme,mkURI,QueryParam(..),render,URI)
import Text.URI.Lens (queryParam,uriQuery,unRText,uriScheme,uriAuthority,authHost,uriPath)
import Data.Text (Text(..))
import Data.ByteString (ByteString(..))
import qualified Data.ByteString as BS
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

getConfigs bs = do payload <- decodeMigrationPayload bs
                   let cs = payload ^. configs
                   return cs

otpURI :: OTPConfig -> Maybe URI
otpURI otp = do sp <- QueryParam <$> mkQueryKey "secret" <*> mkQueryValue secret'
                kp <- QueryParam <$> mkQueryKey "issuer" <*> mkQueryValue issuer'
                ap <- QueryParam <$> mkQueryKey "algorithm" <*> mkQueryValue algorithm'
                np <- mkPathPiece name'
                bare <- mkURI $ T.append "otpauth://" otptype'
                let uri = bare & uriPath .~ [np] & uriQuery .~ [sp,kp,ap]
                return uri
  where secret' = T.decodeUtf8 $ Base32.encode (otp ^. secret)
        name' = otp ^. name
        issuer' = otp ^. issuer
        algorithm' = T.pack . show $ otp ^. algorithm
        digits' = fromDigitCount $ otp ^. digits
        otptype' = fromOTPType $ otp ^. otpType
        counter' = T.pack . show $ otp ^. counter
        fromOTPType ot = case ot of
                              HOTP -> "hotp"
                              _    -> "totp"
        fromDigitCount dc = case dc of
                                 EIGHT -> "8" 
                                 _     -> "6"

extractURIs bs = fromMaybe []  (getMigrationData bs >>= getConfigs >>= mapM (fmap render . otpURI ) )

main :: IO ()
main = do args <- fmap (map T.pack) getArgs
          let uris = concatMap extractURIs args
          mapM_ T.putStrLn uris
