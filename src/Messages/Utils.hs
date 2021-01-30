module Messages.Utils (
    encode'', decode''
) where

import Data.Aeson (ToJSON, FromJSON, decode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder(toLazyByteString)

-- ToJSON/FromJSON utils
encode'' :: ToJSON a => a -> Text
encode'' = toStrict . encodeToLazyText

decode'' :: FromJSON a => Text -> Maybe a
decode'' = decode . toLazyByteString . encodeUtf8Builder
