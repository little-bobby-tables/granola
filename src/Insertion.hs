module Insertion (DataModifier, addTag, removeTag) where
  import Strings (UTF8BSting, (@+), stringToUtf8)

  import Data.Set (Set)
  import qualified Data.Set as Set

  import Data.List (inits)

  import Control.Monad (forM_)

  import Database.Redis (Connection, runRedis, zincrby)

  type DataModifier = String -> IO ()

  prefixes :: String -> Set UTF8BSting
  prefixes tag =
    let tagPrefixes = filter ((> 1) . length) $ concatMap inits $ words tag
    in Set.map stringToUtf8 $ Set.fromList tagPrefixes

  addTag :: Connection -> DataModifier
  addTag redis tag = changeScore redis tag 1

  removeTag :: Connection -> DataModifier
  removeTag redis tag = changeScore redis tag (-1)

  changeScore :: Connection -> String -> Integer -> IO ()
  changeScore redis tag by = runRedis redis $ do
    forM_ (prefixes tag) $ \prefix ->
      zincrby ("search:" @+ prefix) by (stringToUtf8 tag)
