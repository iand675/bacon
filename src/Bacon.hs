{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as B
import Data.Conduit
import qualified Data.Map as M
import Data.List (foldl')
import qualified Data.Set as Set
import Data.Sequence ((<|), (|>), ViewL(..))
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Conduit
import Text.HTML.DOM
import Text.XML (Document)
import Text.XML.Cursor

baconUrl :: T.Text
baconUrl = "/wiki/Kevin_Bacon"

wikipediaBaseUrl :: Maybe Request
wikipediaBaseUrl = parseUrl "http://en.wikipedia.org"

isValidWikiUrl :: T.Text -> Bool
isValidWikiUrl t = T.isPrefixOf "/wiki/" t && not (T.any (== ':') t)

type RelativeWikiUrl = T.Text
type WikipediaM = ResourceT IO

fetchWikipediaPage :: Manager -> RelativeWikiUrl -> WikipediaM Document
fetchWikipediaPage m relUrl = do
  let (Just request) = wikipediaBaseUrl
  let req = request { path = T.encodeUtf8 relUrl }
  body <- fmap responseBody $ http req m
  body $$+- sinkDoc

getBodyContentLinks :: Document -> [RelativeWikiUrl]
getBodyContentLinks = filterLinks . fromDocument

filterLinks :: Cursor -> [RelativeWikiUrl]
filterLinks = child >=> child >=> child >=> attributeIs "id" "bodyContent" >=> descendant >=> element "a" >=> attribute "href"

getValidUrls :: Manager -> RelativeWikiUrl -> WikipediaM [RelativeWikiUrl]
getValidUrls m url = do
  page <- fetchWikipediaPage m url
  let urls = filter isValidWikiUrl $ getBodyContentLinks page
  return urls

data TraversalUrl = TraversalUrl
  { traversalUrl     :: RelativeWikiUrl
  , traversalHistory :: ![RelativeWikiUrl]
  } deriving (Show)

data TraversalQueue = TraversalQueue
  { knownUrls     :: !(Set.Set T.Text)
  , urlQueue      :: !(S.Seq TraversalUrl)
  } deriving (Show)

emptyTraversalQueue :: TraversalQueue
emptyTraversalQueue = TraversalQueue Set.empty S.empty

enqueue :: TraversalQueue -> TraversalUrl -> TraversalQueue
enqueue t url = if Set.member (traversalUrl url) (knownUrls t)
  then t
  else t { knownUrls = Set.insert (traversalUrl url) (knownUrls t), urlQueue = urlQueue t |> url }

dequeue :: TraversalQueue -> (TraversalQueue, Maybe TraversalUrl)
dequeue t = case S.viewl (urlQueue t) of
  EmptyL -> (t, Nothing)
  (l :< q) -> (t { urlQueue = q }, Just l)

traverse :: Manager -> TraversalQueue -> WikipediaM (Maybe [RelativeWikiUrl])
traverse m q = do
  let (q', dequeued) = dequeue q
  case dequeued of
    Nothing -> return Nothing
    Just t -> do
      let url = traversalUrl t
      let history = traversalHistory t
      liftIO $ putStrLn ("Examining " ++ T.unpack url)
      if url == baconUrl
        then return $ Just history
        else do
          urls <- getValidUrls m url
          traverse m $ foldl' enqueue q' $ map (\u -> TraversalUrl u (url : history)) urls

getBaconNumber :: Manager -> RelativeWikiUrl -> WikipediaM (Maybe [RelativeWikiUrl])
getBaconNumber m u = traverse m $ enqueue emptyTraversalQueue $ TraversalUrl u []

main :: IO ()
main = do
  putStrLn "Enter starting relative URL (e.g. /wiki/Tom_Hanks):"
  url <- getLine
  num <- withManager $ \m -> getBaconNumber m $ T.pack url
  case num of
    Nothing -> putStrLn "Can't connect this page to /wiki/Kevin_Bacon"
    Just history -> do
      putStrLn "Found Kevin Bacon. Route followed:"
      mapM_ (putStrLn . T.unpack) history
      putStrLn ("Total distance: " ++ show (length history))
