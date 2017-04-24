{-# LANGUAGE LambdaCase #-}

module Autocomplete where
  import qualified Data.ByteString as B
  import qualified Data.ByteString.UTF8 as UTF8

  import Database.Redis

  import Control.Monad
  import Control.Monad.IO.Class (liftIO)

  import Data.Set (Set)
  import qualified Data.Set as Set

  import Data.List (inits)

  type AutocompleteModifier = String -> IO ()

  addTag :: Connection -> AutocompleteModifier
  addTag redis tag = changeScore redis tag 1

  removeTag :: Connection -> AutocompleteModifier
  removeTag redis tag = changeScore redis tag (-1)

  changeScore :: Connection -> String -> Integer -> IO ()
  changeScore redis tag by =
    let utf8Tag = UTF8.fromString tag
    in runRedis redis $ do
      forM_ (prefixes tag) $ \prefix ->
        zincrby ("search:" @+ prefix) by utf8Tag

  (@+) :: B.ByteString -> B.ByteString -> B.ByteString
  (@+) = B.append

  prefixes :: String -> Set B.ByteString
  prefixes tag =
    let tagPrefixes = filter ((> 1) . length) $ concatMap inits $ words tag
    in Set.map (UTF8.fromString) $ Set.fromList tagPrefixes

  search :: B.ByteString -> IO [(B.ByteString, Int)]
  search prefix = do
    conn <- checkedConnect defaultConnectInfo
    liftIO $ runRedis conn $ highestRanking 10 >>= \case
        Right tags -> return $ map (\(tag, score) -> (tag, round score)) tags
        Left _ -> return []
      where highestRanking = withScoreAboveZero 0
            withScoreAboveZero = searchByKey (1 / 0) 1
            searchByKey = zrevrangebyscoreWithscoresLimit ("search:" @+ prefix)
