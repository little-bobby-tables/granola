{-# LANGUAGE LambdaCase #-}

module Server (runServer, app) where
  import Strings (UTF8BString, utf8ToString)

  import qualified Database.Redis as Redis

  import Data.Aeson (encode)

  import Insertion (DataModifier, addTag, removeTag)
  import Retrieval (search)

  import Control.Monad (join, forM_)

  import Network.Wai (Application, pathInfo, queryString, responseLBS)
  import Network.Wai.Handler.Warp (run)
  import Network.HTTP.Types (ok200, badRequest400)
  import Network.HTTP.Types.Header (hContentType)

  runServer :: IO ()
  runServer = do
    redis <- Redis.checkedConnect Redis.defaultConnectInfo
    run 3030 (app redis)

  app :: Redis.Connection -> Application
  app redis request respond = respond =<<
    case pathInfo request of
      "modify":_ -> do
        modify (param "add") (addTag redis)
        modify (param "remove") (removeTag redis)
        return $ responseLBS ok200 [] ""
      "search":_ -> do
        case (param "q") of
          Just query -> search redis query >>= \results ->
            return $ responseLBS ok200
              [(hContentType, "application/json")] (encode results)
          Nothing ->
            return $ responseLBS badRequest400 [] ""
      _ ->
        return $ responseLBS badRequest400 [] ""
    where
      param name = join $ lookup name (queryString request)

  modify :: Maybe UTF8BString -> DataModifier -> IO ()
  modify (Just encodedTagList) modifier =
    let tagList = read (utf8ToString encodedTagList) :: [String]
    in forM_ tagList modifier
  modify Nothing _ = return ()
