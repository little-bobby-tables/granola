{-# LANGUAGE LambdaCase #-}

module Server (runServer, app) where

  import qualified Data.ByteString.UTF8 as UTF8
  import qualified Data.ByteString.Lazy.UTF8 as LUTF8


  import qualified Database.Redis as Redis

  import Autocomplete (AutocompleteModifier, addTag, removeTag)

  import Control.Monad (join, forM_)

  import Data.Aeson (encode)

  import Network.Wai (Application, Response, pathInfo, queryString, responseLBS)
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
      _ ->
        return $ responseLBS badRequest400 [] ""
    where
      param name = join $ lookup name (queryString request)

  modify :: Maybe UTF8.ByteString -> AutocompleteModifier -> IO ()
  modify (Just encodedTagList) modifier =
    let tagList = read (UTF8.toString encodedTagList) :: [String]
    in forM_ tagList modifier
  modify Nothing _ = return ()
