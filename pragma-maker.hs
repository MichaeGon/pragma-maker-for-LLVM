{-# OPTIONS -Wall -Werror #-}
import Control.Exception
import Control.Monad
import Data.List
import System.Directory
import System.IO

main :: IO ()
main = putStr "Distination filepath>" >> getLine >>= f
	where
	f x       = putStr ".lib path>" >> getLine >>= f' x
	f' x y    = putStr "include path>" >> getLine >>= f'' x y
	f'' x y z = readFile x >>= writeIncludes x z >> writePragmas x y

writePragmas :: FilePath -> FilePath -> IO ()
writePragmas x y = getDirectoryContents y >>= appendFile x . makeContents

writeIncludes :: FilePath -> FilePath -> String -> IO ()
writeIncludes x y zs = bracketOnError (openTempFile "." "temp") finalize editTemp
	where
	finalize (tempName, tempHandle) = hClose tempHandle >> removeFile tempName
	editTemp (tempName, tempHandle) = searchIncludes y
					>>= hPutStr tempHandle . (++ zs) . unlines . map addinc
					>> hClose tempHandle >> removeFile x >> renameFile tempName x
	addinc s = "#include <llvm/" ++ s ++ ">"


searchIncludes :: FilePath -> IO [FilePath]
searchIncludes  x = getDirectoryContents x >>= searchDir x

searchDir :: FilePath -> [FilePath] -> IO [FilePath]
searchDir _ []                         = return []
searchDir x (z : zs)
		| ".h" `isSuffixOf` z  = res >>= return . (z :)
		| '.' `elem` z         = res
		| otherwise            = indir +++ res
		where
		x' = x ++ "/" ++ z
		indir = searchIncludes x' >>= return . map ((z ++) . ('/' :))
		res   = searchDir x zs

infixr 5 +++
(+++) :: IO [a] -> IO [a] -> IO [a]
(+++) = liftM2 (++)

makeContents :: [FilePath] -> String
makeContents = unlines . ("" :) . foldr ff []
	where
	ff x acc
		| ".lib" `isSuffixOf` x = ("#pragma comment( lib, \"" ++ x ++ "\" )") : acc
		| otherwise 		= acc



