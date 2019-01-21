module HaskileSystem
( HaskileSystem.run
)
where

  import System.IO
  import Data.List (find)
  import Component
  import Helper

  run :: IO ()
  run = do
      let
        root = Directory "/" [ root
                             , root
                             ]
        in
        loop root root

  loop :: Component -> Component -> IO ()
  loop root wd = do
      inputTokens <- readInput $ name wd
      result <- handleInput inputTokens root wd
      case result of
          (Right dir) -> loop root dir
          (Left e) -> do
            putStrLn e
            loop root wd

  pwd :: Component -> Component -> String
  pwd _ (Directory "/" _) = ""
  pwd root wd =
      pwd root parentDir ++ "/" ++ name wd
      where parentDir = case cd root wd [".."] of
                          (Right dir) -> dir
  
  ls :: Component -> Component -> [String] -> String
  ls rootDir currentDir pathTokens =
      case cd rootDir currentDir pathTokens of
          (Right dir) -> Component.ls dir
          (Left e)    -> e

  cd :: Component -> Component -> [String] -> Either String Component
  cd root wd  []         = Right wd
  cd root wd  (dir:dirs) =
      case dir of
        "/" -> cd root root dirs
        otherwise ->
          let
            dir' =
                case dir of
                  "."       -> name $ components wd !! 0
                  ".."      -> name $ components wd !! 1
                  otherwise -> dir
            dir'' = find (\d -> (name d) == dir') $ components wd in
            case dir'' of
              (Nothing) -> Left $ dir ++ ": Not a valid Directory"
              (Just comp) ->
                case comp of
                  (File _ _) -> Left $ dir ++ ": Is a File."
                  otherwise  -> cd root comp dirs

  mkdir :: Component -> Component -> String -> Either String Component
  mkdir rootDir childDir newDirName =
      ncd
        where

          -- return a new rootDir directory which contains
          -- the newly created directory at the specified node
          innerMkdir :: Component -> Component -> String -> Either String Component
          innerMkdir f@(File fName fContents)   _        _           = Right f
          innerMkdir (Directory pName pContent) c@(Directory cName cContent) newDirName
            | not $ null (filter (\c -> name c == newDirName) cContent) =
                Left $ newDirName ++ ": Already exists in this Directory"
            | pName == cName = Right newDir' -- if we've reached the directory
                                       -- which contents we're changing by adding a new directory,
                                       -- we reconstruct it and add the new directory to it

            | otherwise      = Right $ Directory pName $ map (\c' -> newContent c') pContent
            where
                newContent c' =
                  case innerMkdir c' c newDirName of
                    (Right dir) -> dir

                -- reconstruct the rootDir and add the new directory
                newDir'  = Directory cName $ cContent ++ [newDir'']

                newDir'' = Directory newDirName [ newDir'' -- link to itself. Mimics the '.' dir.
                                                , newDir'  -- link to it's parent. Mimics the '..' dir.
                                                ]

          -- contains the path to the directory in which we created the new directory
          tokenizedPath = Helper.splitStrBy '/' $ pwd rootDir childDir

          -- New Child Directory
          -- This is the directory in the new tree
          -- which has the same path as childDir in the old tree
          ncd =
            case innerMkdir rootDir childDir newDirName of
              (Right newRoot) -> cd newRoot newRoot tokenizedPath
              (Left e) -> Left e

  rm:: Component -> Component -> String -> Either String Component
  rm rootDir childDir fileName =
    ncd
        where

          -- return a new rootDir directory 
          -- which does not contain
          -- the specified file
          innerRm :: Component -> Component -> String -> Either String Component
          innerRm f@(File fName fContents)   _        _           = Right f
          innerRm (Directory pName pContent) c@(Directory cName cContent) fileName
            | null (filter (\c -> name c == fileName) cContent) =
                Left $ fileName ++ ": No such File"
            | pName == cName = Right newDir' -- if we've reached the directory
                                             -- which contents we're changing by removing a file,
                                             -- we reconstruct it and remove the file

            | otherwise = Right $ Directory pName $ map (\c' -> newContent c') pContent
            where
                newContent c' =
                  case innerRm c' c fileName of
                    (Right dir) -> dir

                isNotFileWithName comp name' = 
                  case comp of
                    (File name' _) -> False
                    otherwise -> True

                -- reconstruct the rootDir and remove the specified file
                newDir'  = Directory cName $ filter (\c -> isNotFileWithName c fileName ) cContent

          -- contains the path to the directory in which we created the new directory
          tokenizedPath = Helper.splitStrBy '/' $ pwd rootDir childDir

          -- New Child Directory
          -- This is the directory in the new tree
          -- which has the same path as childDir in the old tree
          ncd =
            case innerRm rootDir childDir fileName of
              (Right newRoot) -> cd newRoot newRoot tokenizedPath
              (Left e) -> Left e

  handleInput :: [String] -> Component -> Component -> IO (Either String Component)
  handleInput inputTokens root wd = do
      case inputTokens of
        (command:args) ->
          let
            (args':_) = args 
            (s, ss)    = break (\d -> d /= '/') args'
            pathTokens =
                if not $ null args
                then case s of
                        "/"       -> s:(Helper.splitStrBy '/' ss)
                        otherwise -> Helper.splitStrBy '/' args'
                else []
            in
              case command of
                "cd" -> do
                  return $ cd root wd pathTokens
                "ls" -> do
                  putStrLn $ HaskileSystem.ls root wd pathTokens;
                  return $ Right wd
                "pwd" -> do
                  putStrLn $ pwd root wd
                  return $ Right wd
                "mkdir" -> do
                  return $ mkdir root wd args'
                "rm" -> do
                  return $ rm root wd args'
                otherwise -> do
                  return $ Right wd
        otherwise -> do
            return $ Right wd

  tokenizeInput :: String -> [String]
  tokenizeInput input = Helper.splitStrBy ' ' input

  readInput :: String -> IO [String]
  readInput wdName = do
      putStr $ wdName ++ "$ "
      hFlush stdout
      input <- getLine
      return $ tokenizeInput input