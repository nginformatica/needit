{-# LANGUAGE ScopedTypeVariables #-}
module NeedIt (readDeps, extractSource, clearEmptyLines) where
import System.IO
import Control.Exception

data Source = Source { package :: [String]
                     , deps :: [String] }
                     deriving Show

readDeps :: IO (Maybe [String])
readDeps = handle (\(e :: IOException) -> return Nothing)
    ((openFile "DEPENDENCIES" ReadMode) >>= hGetContents >>= return . Just . lines)

clearEmptyLines :: [String] -> [String]
clearEmptyLines = filter (not . null)

extractSource :: IO ()
extractSource = readDeps >>= \l -> case l of
    Just xs -> mapM_ (print) $ clearEmptyLines xs
    Nothing -> error "eita"

