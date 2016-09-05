{-# LANGUAGE ScopedTypeVariables #-}
module NeedIt (readDeps, extractSource, validCommands, prefix, collect') where
import System.IO
import Control.Exception (handle, IOException)
import Data.List (partition)
import Data.Char (isSpace)

data Source = Source { package :: [String]
                     , deps :: [String]
                     }
                     deriving Show

readDeps :: IO (Maybe [String])
readDeps = handle (\(e :: IOException) -> return Nothing)
    ((openFile "DEPENDENCIES" ReadMode) >>= hGetContents >>= return . Just . lines)

validCommands :: [String]
validCommands = ["PACKAGE", "NEEDS", ""]

prefix :: String -> String
prefix = takeWhile (not . isSpace)

invalidLine :: [String] -> Int
invalidLine = invalidLine' 1

invalidLine' :: Int -> [String] -> Int
invalidLine' n [] = -1
invalidLine' n (x:xs) | not $ elem (prefix x) validCommands = n
                      | otherwise                           = invalidLine' (n + 1) xs

getValue :: [String] -> [String]
getValue = map (tail . dropWhile (not . isSpace))

collect' :: [String] -> ([String], [String])
collect' xss = let (packages, rest) = partition (\x -> (prefix x) == "PACKAGE") xss
    in (getValue packages, getValue $ filter (\x -> (prefix x) == "NEEDS") rest)

extractSource :: IO (Either String Source)
extractSource = readDeps >>= \monad -> return $ case monad of
    Nothing -> Left "Failed to open DEPENDENCIES"
    Just xs -> case invalidLine xs of
        -1 -> let (package, deps) = collect' xs in case length package of
            0 -> Left "Package name not informed"
            1 -> Right $ Source { package = package, deps = deps }
            _ -> Left "More than one entry for package name"
        n  -> Left $ "Parsing error on line " ++ (show n) ++ ": " ++ (xs !! (n - 1))

