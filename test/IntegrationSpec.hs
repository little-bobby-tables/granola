module IntegrationSpec where
  import Test.Hspec
  import Test.Hspec.Wai

  import Control.Monad (replicateM_)

  import Utils (redis, flushdb)

  import Strings (stringToUtf8)

  import Server (app)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = before_ flushdb $ with (app <$> redis) $ do
    describe "adding a new tag" $
      it "increments its score" $ do
        get "/search?q=tag" `shouldRespondWith` "[]"
        get "/index?add=[\"tag\"]" `shouldRespondWith` 200
        get "/search?q=tag" `shouldRespondWith` "[[\"tag\",1]]"

    describe "removing an existing tag" $ do
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

    describe "searching" $ do
      it "returns the results ordered by score" $ do
        replicateM_ 2 $ get "/index?add=[\"a tag\"]"
        replicateM_ 5 $ get "/index?add=[\"second tag\"]"
        replicateM_ 3 $ get "/index?add=[\"third tag\"]"
        get "/search?q=tag" `shouldRespondWith`
          "[[\"second tag\",5],[\"third tag\",3],[\"a tag\",2]]"
            { matchStatus = 200
            , matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"] }

      it "can search by multiple prefixes" $
        get "/index?add=[\"pearl\"\
          \,\"revengeful\"\
          \,\"terrifying renegade pearl\"\
          \,\"rebellious peridot\"\
          \,\"restrained sapphire\"\
          \,\"crying pear\"]" >>
        get "/search?q=re pe" `shouldRespondWith`
          "[[\"terrifying renegade pearl\",1]\
          \,[\"rebellious peridot\",1]]"

    describe "encoding" $
      it "properly indexes unicode tags" $
        get (stringToUtf8 "/index?add=[\"юникод\",\"юникод юникод\"]") >>
        get (stringToUtf8 "/search?q=юни") `shouldRespondWith`
          "[[\"юникод юникод\",1]\
          \,[\"юникод\",1]]"
