{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Retrieval where
  import Strings (UTF8BString, (@+), utf8ToString, stringToUtf8)

  import Data.List (intercalate)

  import Database.Redis (Connection, Aggregate(Max), runRedis,
    zinterstore, expire, zrevrangebyscoreWithscoresLimit)

  search :: Connection -> UTF8BString -> IO [(String, Int)]
  search redis query =
    let terms = words (utf8ToString query)
    in case (length terms) of
      1 ->
        runRedis redis $ getTopTagsAt ("search:" @+ query) 10
      _ ->
        let intersectionKey = stringToUtf8 (intercalate "|" terms)
            termKeys = map (("search:" @+) . stringToUtf8) terms
        in runRedis redis $
          zinterstore intersectionKey termKeys Max
          >> expire intersectionKey 60
          >> getTopTagsAt intersectionKey 10
      where
        getTopTagsAt key count = withScoreAboveZero key 0 count >>= \case
          Right tags -> return $ (\(tag, score) ->
            (utf8ToString tag, round score)) <$> tags
          Left _ -> return []
        withScoreAboveZero key = zrevrangebyscoreWithscoresLimit key (1 / 0) 1
