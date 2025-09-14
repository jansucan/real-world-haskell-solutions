{-- snippet imports --}
module PodMain where

import PodDownload
import PodDB
import PodTypes
import Database.HDBC

{-- snippet workerFuncs --}
addUrl dbh url = 
    do addPodcast dbh pc
       commit dbh
    where pc = Podcast {castId = 0, castURL = url}

update :: IConnection conn => conn -> (String -> IO ()) -> IO ()
update dbh logf = 
    do pclist <- getPodcasts dbh
       mapM_ procPodcast pclist
       logf "Update complete."
    where procPodcast pc =
              do logf $ "Updating from " ++ (castURL pc)
                 updatePodcastFromFeed dbh pc

download dbh logf =
    do pclist <- getPodcasts dbh
       mapM_ procPodcast pclist
       logf "Download complete."
    where procPodcast pc =
              do logf $ "Considering " ++ (castURL pc)
                 episodelist <- getPodcastEpisodes dbh pc
                 let dleps = filter (\ep -> epDone ep == False)
                             episodelist
                 mapM_ procEpisode dleps
          procEpisode ep =
              do logf $ "Downloading " ++ (epURL ep)
                 getEpisode dbh ep
{-- /snippet workerFuncs --}
