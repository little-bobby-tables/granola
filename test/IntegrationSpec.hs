module IntegrationSpec where
  import Test.Hspec
  import Test.Hspec.Wai

  import Utils (redis, flushdb)

  import Server (app)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = before_ flushdb $ with (app <$> redis) $ do
    describe "inserting tags" $ do
      context "adding a new tag" $ do
        it "increments its score" $ do
          get "/search?q=tag" `shouldRespondWith` "[]"

          get "/modify?add=[\"tag\"]" `shouldRespondWith` 200

          get "/search?q=tag" `shouldRespondWith` "[[\"tag\",1]]"
              { matchStatus = 200
              , matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"] }
