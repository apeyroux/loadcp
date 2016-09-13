{-# LANGUAGE OverloadedStrings #-}

import Text.SSV
import Database.PostgreSQL.Simple
import Data.List.Extra
import Data.Monoid
import System.Environment

data Ville = Ville { villeInse :: String
                   , villeName :: String
                   , villeCP :: String
                   , villeAcheminement :: String
                   , villeLigne5 :: String } deriving (Show, Eq, Read)

entry2ville :: [String] -> Maybe Ville
entry2ville e = case e of
  [] -> Nothing
  [inse, nom, cp, acheminement, ligne5] -> Just (Ville (trim inse) (trim nom) (trim cp) (trim acheminement) (trim ligne5))
  _ -> Nothing


insert2db :: Connection -> Maybe Ville -> IO ()
insert2db c v = do
  case v of
    Just ville -> do
      putStrLn $ "Traitement de " <> villeName ville <> " ..."
      _ <- execute c "insert into villes (inse,name,cp,acheminement,ligne5) values (?,?,?,?,?)" [villeInse ville
                                                                        , villeName ville
                                                                        , villeCP ville
                                                                        , villeAcheminement ville
                                                                        , villeLigne5 ville]
      return ()
    Nothing -> return ()
             
main :: IO()
main = do
  args <- getArgs
  case args of
    [hostdb,cvspath] -> do
      c <- connect defaultConnectInfo { connectHost = hostdb, connectDatabase = "villes" }
      fcsv <- readFile cvspath
      mapM_ ((insert2db c) . entry2ville) $ readSSV format fcsv
      close c
    _ -> putStrLn usage
  where
    usage = "db host"
    format = csvFormat { ssvFormatSeparator = ';' }
