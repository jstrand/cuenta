import System.Directory
import System.FilePath
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

-- IO functions

getEntries path = do
    paths <- getDirectoryContents path
    return $ map (\x -> combine path x) (noDots paths)

filesOnly = filterM doesFileExist
dirsOnly = filterM doesDirectoryExist

walkSubDirs :: [FilePath] -> IO [FilePath]
walkSubDirs dirs = fmap concat $ mapM walkDir dirs

walkDir :: FilePath -> IO [FilePath]
walkDir path = do
    entries <- getEntries path
    files <- filesOnly entries
    dirs <- dirsOnly entries
    filesBelow <- walkSubDirs dirs
    return $ files ++ filesBelow

fileLineCount :: FilePath -> IO Int
fileLineCount file = do
    contents <- readFile file
    return $ lineCount contents

filesLineCount :: [FilePath] -> IO [Int]
filesLineCount files = mapM fileLineCount files

-- Pure functions

newLineCount :: String -> Int
newLineCount = length . lines

lineCount :: String -> Int
-- The last line is also a line even if it's empty, hence +1
lineCount x = (newLineCount x) + 1

notaDot x = (x /= ".") && (x /= "..")
noDots = filter notaDot

extIsWanted wanted file = elem (takeExtension file) wanted

onlyWantedExt wanted = filter (extIsWanted wanted)

includeFiles = [".cs"]

type FileMap = Map FilePath FileTree
data FileTree = Dir Int FileMap | File Int
    deriving (Eq)

indent n = concat $ take n $ repeat "  "

neverStop lvl = False
maxlvl maxx lvl = lvl >= maxx

showOnePath lvl stop (path, tree) =
    indent lvl ++ path ++ showTree tree (lvl + 1) stop

showPaths lvl stop = map $ showOnePath lvl stop

showFileMap lvl stop fileMap = concat
    $ showPaths lvl stop
    $ Map.toList fileMap

showTree (Dir count children) lvl stop =
    " " ++ show count ++ "\n" ++
    (if stop lvl then "" else showFileMap lvl stop children)

showTree (File count) lvl stop = " " ++ show count ++ "\n"

instance Show FileTree where
    show (Dir count children) = showFileMap 0 neverStop children
    show (File count) = show count

showWholeTree dir = showTree dir 0 neverStop
showTreeToDepth dir depth = showTree dir 0 (maxlvl depth)

newDir = Dir 0 Map.empty

insertFile :: FilePath -> Int -> FileMap -> FileMap
insertFile name count fileMap = Map.insert name (File count) fileMap

insertAllLevels :: ([FilePath], Int) -> FileTree -> FileTree
insertAllLevels ([], _) tree = tree
insertAllLevels ((p:[]), count) (Dir dirCount subTree) = Dir (dirCount+count) (insertFile p count subTree)
insertAllLevels ((p:ps), count) (Dir dirCount subTree) =
    let nextLevel = Map.findWithDefault newDir p subTree
        updatedNextLevel = insertAllLevels (ps, count) nextLevel
    in
    Dir (dirCount + count) (Map.insert p updatedNextLevel subTree)

insertCount :: (FilePath, Int) -> FileTree -> FileTree
insertCount (path, lineCount) tree = insertAllLevels (splitPath path, lineCount) tree

makeFileTree :: [(FilePath, Int)] -> FileTree
makeFileTree = foldr insertCount newDir

-- The program

main :: IO ()
main = do
    allFiles <- walkDir "example"
    let files = onlyWantedExt includeFiles allFiles
    counts <- filesLineCount files
    let lineCounts = zip files counts
    putStr $ show $ makeFileTree lineCounts
