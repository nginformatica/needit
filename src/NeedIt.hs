{-# LANGUAGE ScopedTypeVariables #-}
module NeedIt (
    readDeps,
    extractSource,
    validCommands,
    prefix,
    collect',
    depLinks,
    printDeps,
    download,
    install,
    unzipFile) where
import System.IO
import Control.Exception (handle, IOException)
import Data.List (partition)
import Data.Char (isSpace)
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)
import Control.Concurrent.Async (mapConcurrently)
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as B
import Control.Applicative ((<$>))

data State = State { package :: [String]
                   , deps    :: [String]
                   , base    :: [String]
                   }
                   deriving Show

readDeps :: IO (Maybe [String])
readDeps = handle (\(e :: IOException) -> return Nothing)
    ((openFile "DEPENDENCIES" ReadMode) >>= hGetContents >>= return . Just . lines)

validCommands :: [String]
validCommands = ["PACKAGE", "NEEDS", "FROM", ""]

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

collect' :: [String] -> ([String], [String], [String])
collect' xss =
    let
        packages' = filterBy "PACKAGE"
        deps'     = filterBy "NEEDS"
        base'     = filterBy "FROM"
    in (packages', deps', base') where filterBy name = getValue $ filter (\x -> (prefix x) == name) xss

extractSource :: IO (Either String State)
extractSource = readDeps >>= \monad -> return $ case monad of
    Nothing -> Left "Failed to open DEPENDENCIES"
    Just xs -> case invalidLine xs of
        -1 -> case collect' xs of
            ([], _, _)      -> Left "Package name not informed"
            (_, _, [])      -> Left "Base name not informed"
            ((_:_:_), _, _) -> Left "More than one entry for package name"
            (_, _, (_:_:_)) -> Left "More than one origin informed"
            (p', d', b')    -> Right $ State { package = p', deps = d', base = b' }
        n  -> Left $ "Parsing error on line " ++ (show n) ++ ": " ++ (xs !! (n - 1))

depLinks :: State -> [(String, String)]
depLinks src = map compose (deps src)
    where
        baseUrl = head $ base src
        compose dep = (baseUrl ++ dep ++ "/archive/master.zip", getFileName dep)

printDeps :: IO ()
printDeps = extractSource >>= \monad -> case monad of
    Left  msg -> error msg
    Right src -> mapM_ (print . fst) (depLinks src)

getFileName :: String -> String
getFileName name = (reverse $ takeWhile (/= '/') (reverse name)) ++ ".zip"

--download :: (String, String) -> IO ()
download (url, name) = parseUrl url
    >>= \request -> newManager tlsManagerSettings
    >>= \manager -> runResourceT (http request manager
        >>= \response -> responseBody response C.$$+- sinkFile name)
    >>= \_ -> unzipFile name
    >>= \_ -> putStrLn $ "Installed " ++ name

asyncDownload :: [(String, String)] -> IO [()]
asyncDownload = mapConcurrently download

install :: IO [()]
install = extractSource >>= \monad -> case monad of
    Left  msg -> error msg
    Right src -> asyncDownload $ depLinks src

unzipFile :: String -> IO ()
unzipFile f = toArchive <$> B.readFile f >>= extractFilesFromArchive [OptVerbose]
