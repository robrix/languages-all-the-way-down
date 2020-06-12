{-

NB: everything in this file is a bad idea!

-}
module Cache where

traverseCachedIO :: (Read b, Show b) => (a -> IO b) -> [a] -> IO [b]
traverseCachedIO process input = do
  cache <- readCacheIO
  results <- for (zip input (cache <> repeat Nothing)) $ \ (elem, entry) -> case entry of
    Just cached -> return $ Left cached
    Nothing     -> Right <$> process elem
  writeCacheIO $ map (either (const Nothing) Just) results
  return $ map (either id id) results


writeCacheIO :: Show a => [Maybe a] -> IO ()
writeCacheIO things = do
  infoIO $ "(writeCacheIO) starting write of " <> show nThings <> " things"

  exists <- doesDirectoryExist cacheDir
  unless exists $ do
    warnIO $ "(writeCacheIO) creating cache directory " <> cacheDir
    createDirectory cacheDir

  for_ (zip [1..] things) $ \ (i, thing) ->
    case thing of
      Just value -> do
        infoIO $ "(writeCacheIO) wriitng cache file " <> show (i :: Int)
        writeFile (cacheDir </> show i) (show value)
          `E.catch` \ e -> errIO $ "(writeCacheIO) could not write cache file " <> show i <> ": " <> E.displayException (e :: IOError)
      Nothing    -> return ()

  infoIO $ "(writeCacheIO) ending write of " <> show nThings <> " things"
  where
  nThings = length $ catMaybes things

readCacheIO :: Read a => IO [Maybe a]
readCacheIO = do
  exists <- doesDirectoryExist cacheDir
  if exists then do
    entries <- listDirectory cacheDir
    let files = sort $ filter isCacheFile entries
        nThings = length files
    infoIO $ "(readCacheIO) starting read of " <> show nThings <> " things"
    results <- for files $ \ entry -> do
      str <- readFile $ cacheDir </> entry
      case readEither str of
        Left err -> do
          errIO $ "(readCacheIO) could not read cache file " <> entry <> ": " <> err
          return Nothing
        Right a -> return $ Just a
    infoIO $ "(readCacheIO) ending read of " <> show nThings <> " things"
    return results
  else do
    warnIO $ "(readCacheIO) cache directory " <> cacheDir <> " does not exist"
    return []
  where
  isCacheFile path
    | c:_ <- path
    , c /= '.'  = True
    | otherwise = False

cacheDir :: FilePath
cacheDir = "cache"
