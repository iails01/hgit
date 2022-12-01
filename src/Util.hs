module Util(toHexHash) where
import           Crypto.Hash.SHA1 (hash)
import           Data.ByteString  (ByteString, unpack)
import           Text.Printf      (printf)

toHexHash :: ByteString -> String
toHexHash = toHex . hash
  where
    toHex :: ByteString -> String
    toHex bytes = unpack bytes >>= printf "%02x"
