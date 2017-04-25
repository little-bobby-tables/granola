{-# LANGUAGE LambdaCase #-}

module Retrieval where
  import Strings (UTF8BString, (@+), utf8ToString)

  import Database.Redis (Connection, RedisCtx, runRedis,
    zrevrangebyscoreWithscoresLimit)

  search :: Connection -> UTF8BString -> IO [(String, Int)]
  search redis query =
    let terms = words (utf8ToString query)
    in case (length terms) of
      1 -> searchByKeyWithLimit redis ("search:" @+ query) 10
      _ -> return []

  searchByKeyWithLimit :: Connection -> UTF8BString -> Integer -> IO [(String, Int)]
  searchByKeyWithLimit redis key count =
    runRedis redis $ highestRanking >>= \case
      Right tags -> return $ map (\(tag, score) ->
        (utf8ToString tag, round score)) tags
      Left _ -> return []
    where
      highestRanking = searchWithScoreAboveZero 0 count
      searchWithScoreAboveZero = zrevrangebyscoreWithscoresLimit key (1 / 0) 1
