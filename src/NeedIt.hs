{-# LANGUAGE ScopedTypeVariables #-}
module NeedIt (
    catchSyntaxErrors,
    catchSemanticErrors,
    clearDeps,
    createDepsFolder,
    downloadDep,
    existDepsFile,
    existDepsFolder,
    getDepsFile,
    getModulesFolder,
    getPathFor,
    linesToTuples,
    listDependencies,
    readDeps,
    tuplesToDepContent,
    unzipFile,
    Configuration (..),
    DepContent (..),
    Repository (..)
) where
import System.IO
import Data.List (isInfixOf)
import Data.Char (isSpace)
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as B
import Control.Applicative ((<$>), liftA2)
import System.Directory (createDirectory, removeFile, doesFileExist, doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath (joinPath)
import qualified Data.Text as T
import Network.URI (parseURI)
import Data.Maybe (isJust)
import Debug.Trace

data Repository = Self | External String

data Configuration = Configuration { basepath :: FilePath, repository :: Repository }

data DepContent = DepContent { package :: String, source :: String, deps :: [String] }

instance Show Configuration where
    show cfg = foldl1 (++) ["Configuration:\n", "   · Path:       ", basepath cfg, "\n",
        "   · Repository: ", show $ repository cfg]

instance Show Repository where
    show Self         = "self"
    show (External r) = take ((length r) - 4) r

instance Show DepContent where
    show ct = "From " ++ (source ct) ++ ", package `" ++ (package ct) ++ "' needs\n" ++ depList
        where depList = foldl (++) "" (map mountLn $ deps ct)
              mountLn dep = "    · " ++ dep ++ "\n"

infixl 0 |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

clearDeps :: Configuration -> IO ()
clearDeps = removeDirectoryRecursive . getModulesFolder

createDepsFolder :: Configuration -> IO ()
createDepsFolder = createDirectory . getModulesFolder

existDepsFile :: Configuration -> IO Bool
existDepsFile = doesFileExist . getDepsFile

existDepsFolder :: Configuration -> IO Bool
existDepsFolder = doesDirectoryExist . getModulesFolder

getDepsFile :: Configuration -> FilePath
getDepsFile cfg = joinPath [basepath cfg, "DEPENDENCIES"]

getPathFor :: Configuration -> FilePath
getPathFor cfg = joinPath $ [basepath cfg] ++ case repository cfg of
    Self       -> []
    External r -> [r]

getModulesFolder :: Configuration -> FilePath
getModulesFolder cfg = joinPath [basepath cfg, "advpl_modules"]

readDeps :: Configuration -> IO [String]
readDeps cfg = getDepsFile cfg
    |> \filename -> openFile filename ReadMode
        >>= hGetContents
        >>= return . lines

validWords :: String -> Bool
validWords = (liftA2 (||) (== 2) (== 0)) . length . words

validDeclaration :: String -> Bool
validDeclaration = (liftA2 (||)
    (\c -> (length c) == 0)
    (\c -> (c !! 0) `elem` ["PACKAGE", "FROM", "NEEDS"])) . words

validNeeds :: String -> Bool
validNeeds x =
    (command /= "NEEDS") || -- Not a NEEDS directive
    ("/" `isInfixOf` x)
        where (command, value) = listToPair . words $ x

validSource :: String -> Bool
validSource x =
    (command /= "FROM") || -- Not a FROM directive
    (isJust $ parseURI value)
        where (command, value) = listToPair . words $ x

short :: String -> String
short str | length str < 10 = str
          | otherwise       = (++ "...") $ take 7 str

countBy :: (a -> Bool) -> [a] -> Int
countBy pred = length . filter pred

listToPair :: [String] -> (String, String)
listToPair (a:b:_) = (a, b)
listToPair _       = ("", "")

getFileName :: String -> String
getFileName name = (reverse $ takeWhile (/= '/') (reverse name)) ++ ".zip"

catchSyntaxErrors :: [String] -> Maybe (String, Int)
catchSyntaxErrors = catchErrors' 1
    where catchErrors' n [] = Nothing
          catchErrors' n (x:xs) | (not . validWords) x       = Just ("Wrong size declaration", n)
                                | (not . validDeclaration) x = Just ("Unknown declaration `" ++ (short x) ++ "'", n)
                                | (not . validNeeds) x       = Just ("Invalid repository passed for NEEDS", n)
                                | (not . validSource) x      = Just ("Invalid source base", n)
                                | otherwise                  = catchErrors' (n + 1) xs

catchSemanticErrors :: [(String, String)] -> Maybe String
catchSemanticErrors xss | nPackages == 0 = Just "No package name provided"
                        | nPackages  > 1 = Just "Too many package names provided"
                        | nSources  == 0 = Just "No source base provided"
                        | nSources   > 1 = Just "Too many source bases provided"
                        | otherwise      = Nothing
                        where countByCommand cmd = countBy (\pair -> (fst pair) == cmd) $ xss
                              nPackages = countByCommand "PACKAGE"
                              nSources  = countByCommand "FROM"

linesToTuples :: [String] -> Either (String, Int) [(String, String)]
linesToTuples xss = case catchSyntaxErrors xss of
    Nothing  -> Right (xss |> map words
                           |> filter (\l -> length l == 2)
                           |> map listToPair)
    Just err -> Left err

tuplesToDepContent :: [(String, String)] -> DepContent
tuplesToDepContent tuples = DepContent { package = (getByCommand "PACKAGE") !! 0
                                       , source  = (getByCommand "FROM") !! 0
                                       , deps    = (getByCommand "NEEDS") }
                            where getByCommand cmd = map snd $ filter (\pair -> (fst pair) == cmd) tuples

listDependencies :: DepContent -> [(String, String, String)]
listDependencies ct = map toPairWithLink $ deps ct
    where baseUrl = source ct
          toPairWithLink dep = (dep, baseUrl ++ dep ++ "/archive/master.zip", getFileName dep)

downloadDep :: (String, String, String) -> IO ()
downloadDep (repository, url, filename) = parseUrl url
    >>= \request -> newManager tlsManagerSettings
    >>= \manager -> runResourceT (http request manager
        >>= \response -> responseBody response C.$$+- sinkFile $ joinPath ["advpl_modules/", filename])

unzipFile :: FilePath -> IO ()
unzipFile f = toArchive <$> B.readFile f
    >>= extractFilesFromArchive [OptVerbose, OptRecursive, OptDestination "./advpl_modules"]

