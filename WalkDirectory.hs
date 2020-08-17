import Control.Monad
import System.FilePath
import System.Directory

type Directory = (FilePath, [FilePath], [FilePath])

contentDir :: FilePath -> IO Directory
contentDir folder = do
    all         <- map (folder </>) <$> listDirectory folder
    files       <- reverse <$> filterM doesFileExist all
    directories <- reverse <$> filterM doesDirectoryExist all
    return (folder, directories, files)

walk :: FilePath -> IO [Directory]
walk root = do
    here@(_, dirs, _) <- contentDir root
    subDirs <- mapM walk dirs
    return $ here : concat subDirs

main :: IO ()
main = print =<< walk ""