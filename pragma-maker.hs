{-# OPTIONS -Wall -Werror #-}
import Control.Applicative
import Control.Exception
--import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.IO

-- dist .lib include
main :: IO ()
main = do
	(dist : lib : inc : _) <- getArgs
	cnt <- readFile dist
	_ <- writeIncludes dist inc cnt
	writePragmas dist lib

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
		| ".h" `isSuffixOf` z  = (z :) <$> res
		| '.' `elem` z         = res
		| otherwise            = (++) <$> indir <*> res
		where
		x' = x ++ "/" ++ z
		indir = map ((z ++) . ('/' :)) <$> searchIncludes x'
		res   = searchDir x zs

{-
infixr 5 +++
(+++) :: (Monad m) => m [a] -> m [a] -> m [a]
(+++) = liftM2 (++)
-}

makeContents :: [FilePath] -> String
makeContents = unlines . ("" :) . foldr ff []
	where
	ff x acc
		| ".lib" `isSuffixOf` x = ("#pragma comment( lib, \"" ++ x ++ "\" )") : acc
		| otherwise 		= acc
