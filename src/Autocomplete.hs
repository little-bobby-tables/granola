{-# LANGUAGE LambdaCase #-}

module Autocomplete where
  import qualified Data.ByteString as B
  import qualified Data.ByteString.UTF8 as UTF8

  import Database.Redis

  import Control.Monad

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

  search :: Connection -> UTF8.ByteString -> IO [(String, Int)]
  search redis prefix =
    runRedis redis $ highestRanking 10 >>= \case
        Right tags -> return $ map (\(tag, score) ->
          (UTF8.toString tag, round score)) tags
        Left _ -> return []
      where
        highestRanking = withScoreAboveZero 0
        withScoreAboveZero = searchByKey (1 / 0) 1
        searchByKey = zrevrangebyscoreWithscoresLimit ("search:" @+ prefix)

  prefixes :: String -> Set UTF8.ByteString
  prefixes tag =
    let tagPrefixes = filter ((> 1) . length) $ concatMap inits $ words tag
    in Set.map (UTF8.fromString) $ Set.fromList tagPrefixes

  (@+) :: UTF8.ByteString -> UTF8.ByteString -> UTF8.ByteString
  (@+) = B.append
