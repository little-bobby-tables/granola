module Strings where
  import qualified Data.ByteString as B
  import qualified Data.ByteString.UTF8 as UTF8

  type UTF8BSting = UTF8.ByteString

  (@+) :: UTF8BSting -> UTF8BSting -> UTF8BSting
  (@+) = B.append

  stringToUtf8 :: String -> UTF8BSting
  stringToUtf8 = UTF8.fromString

  utf8ToString :: UTF8BSting -> String
  utf8ToString = UTF8.toString
