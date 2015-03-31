{-# OPTIONS -Wall -Werror #-}

import Data.List(isSuffixOf)
import System.Directory(getDirectoryContents)
import System.Environment(getArgs)

main :: IO ()
main = getArgs >>= writePragmas

writePragmas :: [FilePath] -> IO ()
writePragmas [] 	 = error "Error: No input"
writePragmas (_ : []) 	 = error "Error: Distination filepath is needed"
writePragmas (x : y : _) = getDirectoryContents x >>= appendFile y . makeContents

makeContents :: [FilePath] -> String
makeContents = unlines . foldr ff []
	where
	ff x acc
		| ".lib" `isSuffixOf` x = ("#pragma comment( lib, \"" ++ x ++ "\" )") : acc
		| otherwise 		= acc

