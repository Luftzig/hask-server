module Network.HaskServer.HaskServerSpec where

import SpecHelper
import Text.ParserCombinators.Parsec
import Text.Parsec.Error

spec :: Spec
spec = do
    describe "HaskServer request parsing" $ do
        describe "parse request line" $ do
            it "parses GET and URL" $ do
                parse parseMethod "" "GET /resource/path HTTP/1.1\r\nSomething else" `shouldBe` (Right (GET, "/resource/path"))
            it "parses POST" $ do
                parse parseMethod "" "POST /resource/path HTTP garbage\r\nSomething else" `shouldBe` (Right (POST, "/resource/path"))
        describe "parse headers" $ do
            it "reads zero headers" $ do
                parse parseHeaders "" "\r\n" `shouldBe` (Right [])
            it "reads 1 header" $ do
                parse parseHeaders "" "Accepts: */*\r\n\r\n" `shouldBe` (Right [("Accepts", "*/*")])
            it "reads multiple headers" $ do
                parse parseHeaders "" "Accepts: cow\r\nContent-Length: 30\r\n\r\n" `shouldBe` (Right [("Accepts", "cow"), ("Content-Length", "30")])

        describe "parse body" $ do
            describe "given length" $ do
                it "reads length content" $ do
                    parse (parseBody POST (Just "10")) "" "abcdef hgklemsn" `shouldBe` (Right $ Just "abcdef hgk")
                it "doesn't read content for GET method" $ do
                    parse (parseBody GET (Just "10")) "" "abcdef hgklemsn" `shouldBe` (Right $ Nothing)
            describe "not given length" $ do
                it "will read no more" $ do
                    parse (parseBody POST Nothing) "" "abcdef hasfkl" `shouldBe` (Right Nothing)

        describe "parse message" $ do
            it "parses a message without a body" $ \_ ->
                let res = parse parseRequest "" "GET /source/something HTTP 1.1\r\nAccept: */*\r\nCache-Control: no-cache\r\n\r\n"
                in res `shouldBe` (Right HTTPRequest { method = GET
                                                     , uri = "/source/something"
                                                     , reqHeaders = [("Accept", "*/*"), ("Cache-Control", "no-cache")]
                                                     , reqBody = Nothing
                                                     })
            it "parses a message with body" $ \_ ->
                let res = parse parseRequest "" "POST /source/something HTTP/1.1\r\nAccept: */*\r\nContent-Length: 10\r\n\r\nabcdefghijklmn"
                in res `shouldBe` (Right HTTPRequest { method = POST
                                                     , uri = "/source/something"
                                                     , reqHeaders = [("Accept", "*/*"), ("Content-Length", "10")]
                                                     , reqBody = Just "abcdefghij"
                                                     })
    describe "Response rendering" $ do
        describe "Render response line" $ do
            it "Displays proper response" $ do
                show StatusLine { version = HTTP11, code = 200, reason = "OK"} `shouldBe` "HTTP/1.1 200 OK\r\n"
        describe "Render headers" $ do
            it "displays zero headers" $ do
                showHeaders [] `shouldBe` "\r\n"
            it "displays one header" $ do
                showHeaders [("Some", "Thing")] `shouldBe` "Some: Thing\r\n\r\n"
            it "displays two headers" $ do
                showHeaders [("Some", "Thing"), ("Another", "thing")] `shouldBe` "Some: Thing\r\nAnother: thing\r\n\r\n"
        describe "Render body" $ do
            it "displays the empty body" $ do
                showBody Nothing `shouldBe` "\r\n"
            it "display body with content" $ do
                showBody (Just "Hello world!") `shouldBe` "Hello world!\r\n"
        describe "Render full response" $ do
            it "display correctly" $ do
                (show HTTPResponse  { status = StatusLine {version = HTTP11, code = 204, reason = "NO CONTENT"}
                                    , respHeaders = [("Content-Length", "5"), ("ETag", "123Blah")]
                                    , respBody = Just "12345"})
                                    `shouldBe` "HTTP/1.1 204 NO CONTENT\r\nContent-Length: 5\r\nETag: 123Blah\r\n\r\n12345\r\n"

main :: IO ()
main = hspec spec
