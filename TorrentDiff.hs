module Main (main) where

-- Hiding
import Prelude hiding (FilePath)

-- Types
import Data.Map (Map)
import Data.Set (Set)
import Filesystem.Path (FilePath)

-- Normal
import ConcatMapM
  ( concatMapM
  )
import Data.BEncode
  ( BEncode (..)
  , bRead
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
import qualified Data.ByteString as
  B
  ( concat
  )
import qualified Data.ByteString.Lazy as
  BL
  ( readFile
  , toChunks
  )
import qualified Data.Map as
  Map
  ( fromDistinctAscList
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

-- | Extracts a list of file paths from a torrent file parsed into a BEncode
--   structure.
getFiles :: BEncode -> [FilePath]
getFiles be = fromMaybe [] $ do
  (BDict torrent) <- return be
  (BDict info) <- Map.lookup "info" torrent
  (BList files) <- Map.lookup "files" info
  return $ mapMaybe
    (\ (BDict file) -> do
      (BList segments) <- Map.lookup "path" file
      let path = Path.concat $ mapMaybe
                   (\ (BString bs) -> return $ Path.decode $ B.concat $ BL.toChunks bs)
                   segments
      return path)
    files

torrentFiles :: FilePath -> IO [FilePath]
torrentFiles path = do
  bytes <- BL.readFile (Path.encodeString path)
  -- TODO: Catch exceptions.
  case bRead bytes of
    Nothing -> error $ errorString "couldn't parse bencoded data"
    Just be -> do
      let files = getFiles be
      return files

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

-- TODO: Remove this when containers-0.5 is used.
-- | Constructs a Map from a Set using the function to generate values
--   from the keys.
fromSet :: (k -> v) -> Set k -> Map k v
fromSet f s = Map.fromDistinctAscList $ map (\ k -> (k, f k)) $ Set.toList s

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
          changed   = Map.union (fromSet (const "-") removed) (fromSet (const "+") added)
          diff      = Map.union changed (fromSet (const " ") unchanged)

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
