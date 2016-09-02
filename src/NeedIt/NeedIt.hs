module NeedIt where
import System.IO
import Control.Monad

readDeps :: Maybe String
readDeps = let file = case openFile "DEPENDENCIES" ReadMode in
  case True of
    isAlreadyInUseError  file -> Nothing
    isDoesNotExistsError file -> Nothing
    isPermissionError    file -> Nothing
    otherwise                 -> file >>= hGetContents >>= Just . lines
