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

  (@+) :: B.ByteString -> B.ByteString -> B.ByteString
  (@+) = B.append

  prefixes :: B.ByteString -> Set B.ByteString
  prefixes tag =
    let utf8Tag = UTF8.toString tag
        wordPrefixes = filter ((> 1) . length) $ concatMap inits $ words utf8Tag
    in Set.fromList $ map (UTF8.fromString) $ wordPrefixes

  add :: B.ByteString -> IO ()
  add tag = changeScore tag 1

  remove :: B.ByteString -> IO ()
  remove tag = changeScore tag (-1)

  changeScore :: B.ByteString -> Integer -> IO ()
  changeScore tag by = do
    conn <- checkedConnect defaultConnectInfo
    runRedis conn $ do
      forM_ (prefixes tag) $ \prefix -> zincrby ("search:" @+ prefix) by tag

  search :: B.ByteString -> IO [(B.ByteString, Int)]
  search prefix = do
    conn <- checkedConnect defaultConnectInfo
    liftIO $ runRedis conn $ highestRanking 10 >>= \case
        Right tags -> return $ map (\(tag, score) -> (tag, round score)) tags
        Left _ -> return []
      where highestRanking = withScoreAboveZero 0
            withScoreAboveZero = searchByKey (1 / 0) 1
            searchByKey = zrevrangebyscoreWithscoresLimit ("search:" @+ prefix)
