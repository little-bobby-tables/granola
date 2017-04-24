{-# LANGUAGE LambdaCase #-}

module Server (runServer, app) where

  import qualified Data.ByteString.UTF8 as UTF8
  import qualified Data.ByteString.Lazy.UTF8 as LUTF8

  import qualified Database.Redis as Redis

  import qualified Autocomplete as A

  import Control.Monad (join, forM_)

  import Data.Aeson (encode)

  import Network.Wai (Application, Response, pathInfo, queryString, responseLBS)
  import Network.Wai.Handler.Warp (Port, run)
  import Network.HTTP.Types (ok200, badRequest400, notFound404)
  import Network.HTTP.Types.Header (hContentType)

  runServer :: IO ()
  runServer = do
    redis <- Redis.checkedConnect Redis.defaultConnectInfo
    run 3030 (app redis)

  app :: Redis.Connection -> Application
  app redis request respond = respond =<<
    case pathInfo request of
      "mod":_ ->
        let tags = join $ lookup "tags" $ queryString request
        in case tags of
          Just tagString -> modTags redis tagString >> (return $ responseLBS ok200 [] "")
          Nothing -> return $ responseLBS badRequest400 [] ""
      _ -> return $ responseLBS notFound404 [] ""

  modTags :: Redis.Connection -> UTF8.ByteString -> IO ()
  modTags redis encodedTags =
    let tags = read (UTF8.toString encodedTags) :: [UTF8.ByteString]
    in forM_ tags $ \tag -> A.add tag
