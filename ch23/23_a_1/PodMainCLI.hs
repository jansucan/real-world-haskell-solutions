{-- From examples/examples/ch22/PodMain.hs and modified --}
module Main where

import PodMain
import PodDownload
import PodDB
import PodTypes
import System.Environment
import Database.HDBC
import Network.Socket(withSocketsDo)

main = withSocketsDo $ handleSqlError $
    do args <- getArgs
       dbh <- connect "pod.db"
       case args of
         ["add", url] -> addUrl dbh url
         ["update"] -> update dbh putStrLn
         ["download"] -> download dbh putStrLn
         ["fetch"] -> do update dbh putStrLn
                         download dbh putStrLn
         _ -> syntaxError
       disconnect dbh

syntaxError = putStrLn 
  "Usage: pod command [args]\n\
  \\n\
  \pod add url      Adds a new podcast with the given URL\n\
  \pod download     Downloads all pending episodes\n\
  \pod fetch        Updates, then downloads\n\
  \pod update       Downloads podcast feeds, looks for new episodes\n"
{-- End of code from examples --}
