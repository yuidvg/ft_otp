{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Crypto.Hash.SHA1 qualified as SHA1
import Data.Binary.Get (getWord32be, runGet)
import Data.Binary.Put (putWord32be, runPut)
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as LBS
import Data.Time.Clock.POSIX
import Data.Word
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Printf

-- Constants
keyFileName :: String
keyFileName = "ft_otp.key"

timeStep :: Word32
timeStep = 30

digit :: Word32
digit = 6

totp :: ByteString -> IO Word32
totp key =
  getPOSIXTime >>= \currentTime ->
    let timeCounter = floor currentTime `div` fromIntegral timeStep :: Integer
     in pure $ hotp key (fromIntegral timeCounter)

-- HOTP implementation following RFC 4226
hotp :: ByteString -> Word32 -> Word32
hotp secret counter = dynamicTruncate $ hmacSha1 secret (LBS.toStrict $ runPut $ putWord32be counter)

dynamicTruncate :: ByteString -> Word32
dynamicTruncate hs =
  let sBits = dt hs
   in sBits `mod` 10 ^ digit

dt :: ByteString -> Word32
dt hs =
  let offsetBits = BS.index hs 19 .&. 0x0f
      offset = fromIntegral offsetBits
      p = runGet getWord32be $ LBS.fromStrict $ BS.drop offset hs
   in p .&. 0x7fffffff

hmacSha1 :: ByteString -> ByteString -> ByteString
hmacSha1 = SHA1.hmac

-- Validate hexadecimal key
validateHexKey :: String -> Either String ByteString
validateHexKey hexStr =
  if length hexStr < 64
    then Left "key must be 64 hexadecimal characters."
    else case B16.decode (C8.pack hexStr) of
      Right decoded -> Right decoded
      Left _ -> Left "key must be valid hexadecimal."

-- Simple encryption/decryption (XOR with fixed key for demonstration)
-- In production, use proper encryption
encryptKey :: ByteString -> ByteString
encryptKey = BS.map (Data.Bits.xor 0xAA)

decryptKey :: ByteString -> ByteString
decryptKey = encryptKey -- XOR is its own inverse

-- Save encrypted key to file
saveKey :: ByteString -> IO ()
saveKey key = do
  let encrypted = encryptKey key
  BS.writeFile keyFileName encrypted
  putStrLn $ "Key was successfully saved in " ++ keyFileName ++ "."

-- Load and decrypt key from file
loadKey :: String -> IO (Either String ByteString)
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
    Right key -> do
      code <- totp key
      print code
    Left err -> do
      hPutStrLn stderr $ "./ft_otp: error: " ++ err
      exitFailure

-- Generate key from hex file
generateKey :: String -> IO ()
generateKey filename = do
  exists <- doesFileExist filename
  if exists
    then do
      content <- BS.readFile filename
      let hexStr = C8.unpack $ BS.filter (/= 10) content
      case validateHexKey hexStr of
        Left err -> do
          hPutStrLn stderr $ "./ft_otp: error: " ++ err
          exitFailure
        Right key -> saveKey key
    else do
      hPutStrLn stderr $ "./ft_otp: error: file " ++ filename ++ " does not exist."
      exitFailure

-- Main function with argument parsing
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-g", filename] -> generateKey filename
    ["-k", filename] -> generateTOTP filename
    _ -> do
      hPutStrLn stderr "Usage: ft_otp [-g hex_file | -k key_file]"
      hPutStrLn stderr "  -g: Generate and save key from hexadecimal file"
      hPutStrLn stderr "  -k: Generate TOTP from saved key file"
      exitFailure