{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Binary (encode, decode)
import Data.Binary.Get (runGet, getWord32be, getWord8)
import Data.Binary.Put (runPut, putWord32be)
import Data.Bits ((.&.), shiftR)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32, Word8)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- Constants
keyFileName :: String
keyFileName = "ft_otp.key"

timeStep :: Word32
timeStep = 30

-- HOTP implementation following RFC 4226
hotp :: BS.ByteString -> Word32 -> String
hotp secret counter =
  let counterBytes = LBS.toStrict $ runPut $ putWord32be counter
      hmac = SHA1.hmac secret counterBytes
      offset = fromIntegral (BS.last hmac .&. 0x0f)
      truncated = runGet getWord32be $ LBS.fromStrict $ BS.drop offset hmac
      code = (truncated .&. 0x7fffffff) `mod` 1000000
  in printf "%06d" code

-- TOTP implementation following RFC 6238
totp :: BS.ByteString -> IO String
totp secret = do
  currentTime <- getPOSIXTime
  let timeCounter = floor currentTime `div` fromIntegral timeStep
  pure $ hotp secret (fromIntegral timeCounter)

-- Validate hexadecimal key
validateHexKey :: String -> Either String BS.ByteString
validateHexKey hexStr =
  if length hexStr < 64
    then Left "key must be 64 hexadecimal characters."
    else case B16.decode (C8.pack hexStr) of
           Right decoded -> Right decoded
           Left _ -> Left "key must be valid hexadecimal."

-- Simple encryption/decryption (XOR with fixed key for demonstration)
-- In production, use proper encryption
encryptKey :: BS.ByteString -> BS.ByteString
encryptKey = BS.map (`xor` 0xAA)
  where xor a b = fromIntegral $ fromIntegral a `xor` fromIntegral b

decryptKey :: BS.ByteString -> BS.ByteString
decryptKey = encryptKey -- XOR is its own inverse

-- Save encrypted key to file
saveKey :: BS.ByteString -> IO ()
saveKey key = do
  let encrypted = encryptKey key
  BS.writeFile keyFileName encrypted
  putStrLn $ "Key was successfully saved in " ++ keyFileName ++ "."

-- Load and decrypt key from file
loadKey :: String -> IO (Either String BS.ByteString)
loadKey filename = do
  exists <- doesFileExist filename
  if not exists
    then pure $ Left $ "File " ++ filename ++ " does not exist."
    else do
      encrypted <- BS.readFile filename
      pure $ Right $ decryptKey encrypted

-- Generate TOTP from key file
generateTOTP :: String -> IO ()
generateTOTP filename = do
  result <- loadKey filename
  case result of
    Left err -> do
      hPutStrLn stderr $ "./ft_otp: error: " ++ err
      exitFailure
    Right key -> do
      code <- totp key
      putStrLn code

-- Generate key from hex file
generateFromHex :: String -> IO ()
generateFromHex filename = do
  exists <- doesFileExist filename
  if not exists
    then do
      hPutStrLn stderr $ "./ft_otp: error: file " ++ filename ++ " does not exist."
      exitFailure
    else do
      content <- readFile filename
      let hexStr = filter (/= '\n') content
      case validateHexKey hexStr of
        Left err -> do
          hPutStrLn stderr $ "./ft_otp: error: " ++ err
          exitFailure
        Right key -> saveKey key

-- Main function with argument parsing
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-g", filename] -> generateFromHex filename
    ["-k", filename] -> generateTOTP filename
    _ -> do
      hPutStrLn stderr "Usage: ft_otp [-g hex_file | -k key_file]"
      hPutStrLn stderr "  -g: Generate and save key from hexadecimal file"
      hPutStrLn stderr "  -k: Generate TOTP from saved key file"
      exitFailure