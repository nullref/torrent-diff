module Main (main) where

-- Hiding
import Prelude hiding (FilePath)

-- Types
import Data.Set (Set)
import Filesystem.Path (FilePath)

-- Normal
import ConcatMapM
  ( concatMapM
  )
import Data.AttoBencode
  ( BValue(..)
  )
import Data.AttoBencode.Parser
  ( bValue
  )
import Data.Attoparsec
  ( maybeResult
  , parse
  )
import Data.Maybe
  ( fromMaybe
  , mapMaybe
  )
import Data.Monoid
  ( (<>)
  )
import System.Console.GetOpt
  ( ArgDescr(NoArg)
  , ArgOrder(Permute)
  , OptDescr(Option)
  , getOpt
  , usageInfo
  )
import System.Directory
  ( getDirectoryContents
  )
import System.Environment
  ( getArgs
  )
import System.Exit
  ( exitFailure
  , exitSuccess
  )
import System.Posix.Files
  ( isDirectory
  , getFileStatus
  )

-- Qualified
import qualified Data.ByteString.Char8 as
  B
  ( pack
  )
import qualified Data.ByteString as
  B
  ( readFile
  )
import qualified Data.Map as
  Map
  ( fromSet
  , lookup
  , toList
  , union
  )
import qualified Data.Set as
  Set
  ( difference
  , fromList
  , intersection
  , toList
  )
import qualified Filesystem.Path.CurrentOS as
  Path
  ( concat
  , decode
  , decodeString
  , directory
  , encodeString
  , stripPrefix
  )

progName :: String
progName = "torrent-diff"

errorString :: String -> String
errorString err = progName ++ ": " ++ err

usageHeader :: String
usageHeader = "Usage: " ++ progName ++ " [OPTION]... OLDFILE NEWFILE"

data Mode
  = ModeAdded
  | ModeRemoved
  | ModeDiff

data Options
  = Options
  { optMode :: Mode
  , optMkSet :: ([FilePath] -> Set FilePath)
  }

defOptions :: Options
defOptions = Options ModeAdded mkSetDirectories

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['a'] []
    (NoArg (\ opt -> opt { optMode = ModeAdded }))
    "show added files (default)"
  , Option ['r'] []
    (NoArg (\ opt -> opt { optMode = ModeRemoved }))
    "show removed files"
  , Option ['p'] []
    (NoArg (\ opt -> opt { optMode = ModeDiff }))
    "show a diff"
  , Option ['d'] []
    (NoArg (\ opt -> opt { optMkSet = mkSetDirectories }))
    "operate on directories (default)"
  , Option ['f'] []
    (NoArg (\ opt -> opt { optMkSet = mkSetFiles }))
    "operate on files instead of directories"
  ]

-- | Extracts a list of file paths from a torrent file parsed into a BValue
--   structure.
getFiles :: BValue -> [FilePath]
getFiles be = fromMaybe [] $ do
  (BDict torrent) <- return be
  (BDict info) <- Map.lookup (B.pack "info") torrent
  (BList files) <- Map.lookup (B.pack "files") info
  return $ mapMaybe
    (\ (BDict file) -> do
      (BList segments) <- Map.lookup (B.pack "path") file
      let path = Path.concat $ mapMaybe
                   (\ (BString bs) -> return $ Path.decode bs)
                   segments
      return path)
    files

torrentFiles :: FilePath -> IO [FilePath]
torrentFiles path = do
  bytes <- B.readFile (Path.encodeString path)
  -- TODO: Catch exceptions.
  case maybeResult $ parse bValue bytes of
    Nothing -> error $ errorString "couldn't parse bencoded data"
    Just be -> return $ getFiles be

getDirectoryContentsFP :: FilePath -> IO [FilePath]
getDirectoryContentsFP filePath = map Path.decodeString `fmap` getDirectoryContents (Path.encodeString filePath)

isDirectoryFP :: FilePath -> IO Bool
isDirectoryFP filePath = isDirectory `fmap` getFileStatus (Path.encodeString filePath)

-- XXX: POSIX-specific.
isSpecialFP :: FilePath -> Bool
isSpecialFP filePath = filePath `elem` (map Path.decodeString [".", ".."])

dirFiles :: FilePath -> IO [FilePath]
dirFiles unfixedPath =
  let fixedPath = unfixedPath <> Path.decodeString ""
  in mapMaybe (Path.stripPrefix fixedPath) `fmap` dirFiles' fixedPath
  where
  dirFiles' path = do
    files <- filter (not . isSpecialFP) `fmap` getDirectoryContentsFP path
    concatMapM (\ file -> do
      let filePath = path <> file
      isDir <- isDirectoryFP filePath
      if isDir
        then dirFiles' filePath
        else return [filePath]) files

pathFiles :: ([FilePath] -> Set FilePath) -> FilePath -> IO (Set FilePath)
pathFiles mkSet filePath = do
  isDir <- isDirectoryFP filePath
  files <- if isDir then dirFiles filePath else torrentFiles filePath
  return $ mkSet files

mkSetFiles :: [FilePath] -> Set FilePath
mkSetFiles fps = Set.fromList fps

mkSetDirectories :: [FilePath] -> Set FilePath
mkSetDirectories fps = Set.fromList $ map Path.directory fps

main :: IO ()
main = do
  args <- getArgs

  case getOpt Permute options args of

    (optargs, [oldTorrent, newTorrent], []) -> do
      let opts = foldl (flip id) defOptions optargs

      oldDirs <- pathFiles (optMkSet opts) (Path.decodeString oldTorrent)
      newDirs <- pathFiles (optMkSet opts) (Path.decodeString newTorrent)

      let removed   = Set.difference oldDirs newDirs
          added     = Set.difference newDirs oldDirs
          unchanged = Set.intersection oldDirs newDirs
          changed   = Map.union (Map.fromSet (const "-") removed) (Map.fromSet (const "+") added)
          diff      = Map.union changed (Map.fromSet (const " ") unchanged)

      case (optMode opts) of
        ModeAdded -> do
          mapM_ (putStrLn . Path.encodeString) $ Set.toList added
        ModeRemoved -> do
          mapM_ (putStrLn . Path.encodeString) $ Set.toList removed
        ModeDiff -> do
          mapM_ putStrLn (map (\ (k, v) -> v ++ " " ++ (Path.encodeString k)) (Map.toList diff))

      exitSuccess

    (_, _, []) -> do
      putStrLn $ errorString "wrong number of files"
      putStr $ usageInfo usageHeader options
      exitFailure

    (_, _, errors) -> do
      mapM_ (putStr . errorString) errors
      putStr $ usageInfo usageHeader options
      exitFailure
