module IntegrationSpec where
  import Test.Hspec
  import Test.Hspec.Wai

  import qualified Database.Redis as Redis

  import Network.Wai (Application)

  import Server (app)

  main :: IO ()
  main = hspec spec

  testApp :: IO Application
  testApp = do
    redis <- Redis.checkedConnect $ Redis.defaultConnectInfo
      { Redis.connectDatabase = 16 }
    _ <- Redis.runRedis redis (Redis.flushdb)
    return $ app redis

  spec :: Spec
  spec = with testApp $ do
    describe "inserting tags" $ do
      context "adding a new tag" $ do
        it "increments its score" $ do
          get "/search?q=tag" `shouldRespondWith` "[]"

          get "/modify?add=[\"tag\"]" `shouldRespondWith` 200

          get "/search?q=tag" `shouldRespondWith` "[[\"tag\",1]]"
              { matchStatus = 200
              , matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"] }
