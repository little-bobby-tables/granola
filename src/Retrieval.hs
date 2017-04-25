{-# LANGUAGE LambdaCase #-}

module Retrieval where
  import Strings (UTF8BSting, (@+), utf8ToString)

  import Database.Redis (Connection, runRedis,
    zrevrangebyscoreWithscoresLimit)

  search :: Connection -> UTF8BSting -> IO [(String, Int)]
  search redis prefix =
    runRedis redis $ highestRanking 10 >>= \case
        Right tags -> return $ map (\(tag, score) ->
          (utf8ToString tag, round score)) tags
        Left _ -> return []
      where
        highestRanking = withScoreAboveZero 0
        withScoreAboveZero = searchByKey (1 / 0) 1
        searchByKey = zrevrangebyscoreWithscoresLimit ("search:" @+ prefix)
