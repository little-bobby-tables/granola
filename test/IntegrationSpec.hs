module IntegrationSpec where
  import Test.Hspec
  import Test.Hspec.Wai

  import Control.Monad (replicateM_)

  import Utils (redis, flushdb)

  import Server (app)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = before_ flushdb $ with (app <$> redis) $ do
    describe "indexing" $ do
      context "adding a new tag" $ do
        it "increments its score" $ do
          get "/search?q=tag" `shouldRespondWith` "[]"
          get "/index?add=[\"tag\"]" `shouldRespondWith` 200
          get "/search?q=tag" `shouldRespondWith` "[[\"tag\",1]]"
              { matchStatus = 200
              , matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"] }

      context "removing an existing tag" $ do
        it "decrements its score" $ do
          replicateM_ 2 $ get "/index?add=[\"tag\"]"
          get "/search?q=tag" `shouldRespondWith` "[[\"tag\",2]]"
          get "/index?remove=[\"tag\"]" `shouldRespondWith` 200
          get "/search?q=tag" `shouldRespondWith` "[[\"tag\",1]]"

        it "hides the tag once its score reaches 0" $ do
          replicateM_ 2 $ get "/index?add=[\"tag\"]"
          get "/search?q=tag" `shouldRespondWith` "[[\"tag\",2]]"
          replicateM_ 2 $ get "/index?remove=[\"tag\"]" `shouldRespondWith` 200
          get "/search?q=tag" `shouldRespondWith` "[]"
