module Strings where
  import qualified Data.ByteString as B
  import qualified Data.ByteString.UTF8 as UTF8

  type UTF8BString = UTF8.ByteString

  (@+) :: UTF8BString -> UTF8BString -> UTF8BString
  (@+) = B.append

  stringToUtf8 :: String -> UTF8BString
  stringToUtf8 = UTF8.fromString

  utf8ToString :: UTF8BString -> String
  utf8ToString = UTF8.toString
