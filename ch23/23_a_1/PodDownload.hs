{-- snippet all --}
module PodDownload where
import PodTypes
import PodDB
import PodParser
import Network.HTTP.Conduit
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import System.IO
import Database.HDBC
import Data.Maybe
import Network.URI

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS

{- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success. -}
downloadURL :: String -> IO (Either String LBS.ByteString)
downloadURL url =
    do initReq <- parseRequest url
       let request = initReq { method = methodGet }
       manager <- newManager tlsManagerSettings
       resp <- httpLbs request manager
       let status = responseStatus resp
       -- httpLbs follows redirections (up to the limit specified in the
       -- request, default is 10). No need to handle redirections explicitly
       -- here.
       if statusIsSuccessful status
         then return $ Right $ responseBody resp
         else return $ Left $ T.unpack $ TE.decodeUtf8 $ statusMessage status

{- | Update the podcast in the database. -}
updatePodcastFromFeed :: IConnection conn => conn -> Podcast -> IO ()
updatePodcastFromFeed dbh pc =
    do resp <- downloadURL (castURL pc)
       case resp of
         Left x -> putStrLn x
         Right doc -> updateDB $ LT.unpack $ LTE.decodeUtf8 $ doc

    where updateDB doc = 
              do mapM_ (addEpisode dbh) episodes
                 commit dbh
              where feed = parse doc (castURL pc)
                    episodes = map (item2ep pc) (items feed)

{- | Downloads an episode, returning a String representing
the filename it was placed into, or Nothing on error. -}
getEpisode :: IConnection conn => conn -> Episode -> IO (Maybe String)
getEpisode dbh ep =
    do resp <- downloadURL (epURL ep)
       case resp of
         Left x -> do putStrLn x
                      return Nothing
         Right doc -> 
             do LBS.writeFile filename doc
                updateEpisode dbh (ep {epDone = True})
                commit dbh
                return (Just filename)
          -- This function ought to apply an extension based on the filetype
    where filename = "pod." ++ (show . castId . epCast $ ep) ++ "." ++ 
                     (show (epId ep)) ++ ".mp3"
{-- /snippet all --}
