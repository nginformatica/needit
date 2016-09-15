module Main where
import NeedIt
import System.Console.ANSI
import Control.Concurrent.Async (mapConcurrently)
import System.FilePath
import System.Directory

data Msg = Info String
         | Success String
         | Error String

cfg :: Configuration
cfg = Configuration { basepath = "./",
                      repository = Self }

nop :: IO ()
nop = return ()

printColored :: Color -> String -> IO ()
printColored c msg = setSGR [SetColor Foreground Vivid c]
    >> putStrLn (" · " ++ msg)
    >> setSGR [Reset]

withMessage :: IO a -> Msg -> IO ()
withMessage io (Info info)   = io >> printColored Blue info
withMessage io (Success msg) = io >> printColored Green msg
withMessage io (Error err)   = io >> printColored Red err

intro :: IO ()
intro = setSGR [SetColor Foreground Vivid Magenta]
    >> renderLogo
    >> setSGR [SetColor Foreground Vivid Cyan]
    >> putStrLn "NeedIt AdvPL v0.1"
    >> setSGR [Reset]

main = intro >> existDepsFolder cfg >>= \existance -> case existance of
        True  -> clearDeps cfg `withMessage` (Info "Cleaning advpl_modules...")
        False -> nop
    >> createDepsFolder cfg `withMessage` (Info "Creating dependencies folder...")
    >> existDepsFile cfg >>= \existance -> case existance of
        True  -> parseAndInstall cfg `withMessage` (Info "DONE")
        False -> clearDeps cfg
              >> nop `withMessage` (Error "DEPENDENCIES file not found")

parseAndInstall cfg = putStrLn (show cfg) >> readDeps cfg >>= \src -> case linesToTuples src of
    Left (msg, line) -> nop `withMessage` (Error $ msg ++ " on line " ++ (show line))
    Right tuples     -> case catchSemanticErrors tuples of
        Just err -> nop `withMessage` (Error err)
        Nothing  -> nop `withMessage` (Success "DEPENDENCIES file seems to be CORRECT")
            >> (putStr . show) depContent
            >> nop `withMessage` (Info "Fetching and installing packages concurrently...")
            >> (mapConcurrently download depList >> nop)
                where depContent = tuplesToDepContent tuples
                      depList = listDependencies depContent
                      download dep@(pkg, url, file) =
                        (downloadDep dep >> nop `withMessage` (Success $ "Downloaded " ++ pkg))
                        >> return Configuration { basepath = "./advpl_modules", repository = External file }
                        >>= \cfg -> do
                            let filepath = getPathFor cfg
                            unzipFile filepath
                            let basename = joinPath ["./", "advpl_modules/", tail (dropWhile (/= '/') pkg) ++ "-master"]
                            let newCfg = Configuration { basepath = basename, repository = repository cfg }
                            nop `withMessage` (Info $ "Looking for subrepositories for " ++ pkg)
                            hasSub <- existDepsFile newCfg
                            nop `withMessage` (Info $ "Has subrepositories? " ++ (show hasSub))
                            nop `withMessage` (Success $ "Installed " ++ pkg)
                            if hasSub then (parseAndInstall newCfg `withMessage` (Success $ "Installed subpackages for " ++ pkg))
                                      else nop
                            return ()
                        >> dropModule file
            >> nop `withMessage` (Success "Successfully installed all packages")

dropModule :: String -> IO ()
dropModule name = removeFile $ joinPath ["advpl_modules", name]

renderLogo :: IO ()
renderLogo = mapM_ putStrLn [ " _   _               _ ___ _   "
                            , "| \\ | | ___  ___  __| |_ _| |_ "
                            , "|  \\| |/ _ \\/ _ \\/ _` || || __|"
                            , "| |\\  |  __/  __/ (_| || || |_ "
                            , "|_| \\_|\\___|\\___|\\__,_|___|\\__|" ]
