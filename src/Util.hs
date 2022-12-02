module Util(toHexHash, safeHead) where
import Data.ByteString (ByteString, unpack)
import Crypto.Hash.SHA1 (hash)
import Text.Printf (printf)

toHexHash :: ByteString -> String
toHexHash = toHex . hash
  where
    toHex :: ByteString -> String
    toHex bytes = unpack bytes >>= printf "%02x"

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:xs) = Just a