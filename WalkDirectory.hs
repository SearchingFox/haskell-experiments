import Control.Monad
import System.FilePath
import System.Directory

-- data Directory = Directory (FilePath, [FilePath], [FilePath])
--     deriving (Show)

contentDir :: FilePath -> IO (FilePath, [FilePath], [FilePath])
contentDir folder = do
    all         <- map (folder </>) <$> listDirectory folder
    files       <- reverse <$> filterM doesFileExist all
    directories <- reverse <$> filterM doesDirectoryExist all
    return (folder, directories, files)

walk :: FilePath -> IO [(FilePath, [FilePath], [FilePath])]
walk root = do
    here@(_, dirs, _) <- contentDir root
    subDirs <- mapM walk dirs
    return $ here : concat subDirs

main :: IO ()
main = print =<< walk "C:\\Users\\Ant\\Desktop\\read\\haskell-answers"