module Network.HaskServer where

import System.IO
import Network
import Data.List (intercalate)
import Control.Monad
import Control.Concurrent
import Control.Exception hiding (try)
import Text.ParserCombinators.Parsec
import Debug.Trace


port :: Int
port = 4567

maxHeaderLength :: Int
maxHeaderLength = 4096

data HTTPMethod = GET
                | HEAD
                | POST
                | PUT
                | DELETE
                | TRACE
                | OPTIONS
                | CONNECT
                | PATCH deriving (Show, Read, Eq)

type Header = (String, String)
type Headers = [Header]

data HTTPRequest = HTTPRequest { method :: HTTPMethod
                               , uri :: String
                               , reqHeaders :: Headers
                               , reqBody :: Maybe String
                               } deriving (Show)

instance Eq HTTPRequest where
    a == b =  method a == method b && uri a == uri b
           && reqHeaders a == reqHeaders b && reqBody a == reqBody b

data HTTPVersion =  HTTP11 | HTTP10
data StatusLine = StatusLine { version  :: HTTPVersion
                             , code     :: Int
                             , reason   :: String }
data HTTPResponse = HTTPResponse    { status        :: StatusLine
                                    , respHeaders   :: Headers
                                    , respBody      :: Maybe String
                                    }

instance Show HTTPVersion where
    show HTTP10 = "HTTP/1.0"
    show HTTP11 = "HTTP/1.1"

instance Show StatusLine where
    show s = intercalate " " [show $ version s, show $ code s, reason s] ++ "\r\n"

showHeader :: Header -> String
showHeader (k, v) = k ++ ": " ++ v ++ "\r\n"

showHeaders :: Headers -> String
showHeaders hs = (concat $ map showHeader hs) ++ "\r\n"

showBody :: Maybe String -> String
showBody (Just b) = b ++ "\r\n"
showBody Nothing  = "\r\n"

instance Show HTTPResponse where
    show r =  (show $ status r)
           ++ (showHeaders $ respHeaders r)
           ++ (showBody $ respBody r)

type RequestHandler = HTTPRequest -> HTTPResponse
server :: IO ()
server = withSocketsDo $ do
    sock <- listenOn $ PortNumber $ fromIntegral port
    loop sock

loop :: Socket -> IO ()
loop sock = do
    (h, host, port) <- accept sock
    forkIO (dispatch h echo)
    loop sock


echo :: RequestHandler
echo r = HTTPResponse   { status = StatusLine { version = HTTP11, code = 200, reason = "OK"}
                        , respHeaders  = origLength
                        , respBody  = reqBody r }
    where origLength = case (reqBody r) of
                       (Just s) -> [("Content-Length", show $ length s)]
                       Nothing  -> []


dispatch :: Handle -> RequestHandler -> IO ()
dispatch h f = do
    hSetBuffering h LineBuffering
    content <- hGetContents h
    request <- readContent content
    hPutStr h $ show $ f request
    hFlush h
    hClose h


readContent :: String -> IO HTTPRequest
readContent content =
    case (parsed content) of
        (Left e) -> error "Illegal message"
        (Right request) -> return request
    where parsed = parse parseRequest "Request"


parseRequest :: CharParser () HTTPRequest
parseRequest = do
    (m, u) <- parseMethod
    h <- parseHeaders
    b <- parseBody m $ lookup "Content-Length" h
    return HTTPRequest { method = m
                       , uri = u
                       , reqHeaders = h
                       , reqBody = b }


parseMethod :: CharParser () (HTTPMethod, String)
parseMethod = do
    m <- liftM (read :: String -> HTTPMethod) (manyTill anyChar (many1 space))
    u <- manyTill anyChar (try $ (many1 space) >> string "HTTP")
    manyTill anyChar eol
    return (m, u)


parseHeaders :: CharParser () [(String, String)]
parseHeaders = manyTill parseHeader eol
    where parseHeader = do
                name <- manyTill anyChar (char ':' >> spaces)
                value <- upTo maxHeaderLength notEol
                eol
                return (name, value)


parseBody :: HTTPMethod -> Maybe String -> CharParser () (Maybe String)
parseBody POST (Just len) = try (optionMaybe $ count (read len) anyChar)
parseBody PUT (Just len) = try (optionMaybe $ count (read len) anyChar)
parseBody _ _ = return Nothing


eol :: CharParser () String
eol = string "\r\n"


notEol :: CharParser () Char
notEol = noneOf "\r" <|> try (do c <- char '\r'
                                 notFollowedBy $ char '\n'
                                 return c)


upTo :: Int -> CharParser () a -> CharParser () [a]
upTo 0 _ = return []
upTo n p = option [] $ liftM2 (:) p (upTo (n - 1) p)


msg :: String
msg = "HTTP/1.1 204 OK\r\n\r\n"

