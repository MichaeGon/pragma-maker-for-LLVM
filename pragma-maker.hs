{-# OPTIONS -Wall -Werror #-}

import Data.List
import System.Directory
import System.Environment

main :: IO ()
main = getArgs >>= writePragmas

writePragmas :: [FilePath] -> IO ()
writePragmas [] = error "Error: No input"
writePragmas (_ : []) = error "Error: Distination filepath is needed"
writePragmas (x : y : _) = getDirectoryContents x >>= makeContents y . filter (isInfixOf ".lib")

makeContents :: FilePath -> [FilePath] -> IO ()
makeContents y = appendFile y . unlines . map (\x -> "#pragma comment( lib, \"" ++ x ++ "\" )")

