{-# LANGUAGE LambdaCase #-}

module Server (runServer, app) where
  import qualified Data.ByteString.UTF8 as UTF8

  import qualified Database.Redis as Redis

  import Autocomplete (AutocompleteModifier, addTag, removeTag)

  import Control.Monad (join, forM_)

  import Network.Wai (Application, pathInfo, queryString, responseLBS)
  import Network.Wai.Handler.Warp (run)
  import Network.HTTP.Types (ok200, badRequest400)

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
          Just query -> return $ responseLBS ok200 [] ""
          Nothing -> return $ responseLBS badRequest400 [] ""
      _ ->
        return $ responseLBS badRequest400 [] ""
    where
      param name = join $ lookup name (queryString request)

  modify :: Maybe UTF8.ByteString -> AutocompleteModifier -> IO ()
  modify (Just encodedTagList) modifier =
    let tagList = read (UTF8.toString encodedTagList) :: [String]
    in forM_ tagList modifier
  modify Nothing _ = return ()
