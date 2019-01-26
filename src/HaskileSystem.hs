module HaskileSystem
( HaskileSystem.run
)
where

  import System.IO
  import Data.List (find)
  import Data.Maybe (isNothing)
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
          (Right (root', wd')) -> loop root' wd'
          (Left e) -> do
            putStrLn e
            loop root wd

  pwd :: Component -> Component -> String
  pwd _ (Directory "/" _) = "/"
  pwd root wd =
      pwd root parentDir ++ name wd ++ "/"
      where parentDir = case cd root wd [".."] of
                          (Right dir) -> dir
  
  ls :: Component -> Component -> [String] -> String
  ls rootDir currentDir pathTokens =
      case cd rootDir currentDir pathTokens of
          (Right dir) -> Component.ls dir
          (Left e)    -> e

  cd :: Component -> Component -> [String] -> Either String Component
  cd root wd []         = Right wd
  cd root wd (dir:dirs) =
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

  alterTree :: Component -> Component -> String ->
               ([Component] -> Bool) -> String ->
               (Component -> Component -> String -> Component) ->
               Either String(Component, Component)
  alterTree rootDir objectiveDir componentName
            errorFunction errorMessage
            alteringFunction =
      ngd
      where
        -- return a new rootDir directory which contains
        -- the altered specified directory
        innerAlterTree :: Component -> Component -> Component -> String ->
                          ([Component] -> Bool) -> String ->
                          (Component -> Component -> String -> Component) ->
                          Either String Component

        innerAlterTree f@(File fName fContents) _ _ _ _ _ _ = Right f

        innerAlterTree currDir@(Directory cName cContent) parentDir objectiveDir@(Directory oName oContent) componentName
                       errorFunction errorMessage
                       alteringFunction
            | errorFunction cContent == True =
                  Left $ componentName ++ errorMessage
            | cName == oName = Right newObjectiveDir  -- if we've reached the directory
                                                      -- which contents we're changing,
                                                      -- we reconstruct it with the changes we have to make

            | otherwise = Right newCurrDir
            where
              currDirContentWithoutLinks = drop 2 cContent
              newCurrDirLinks = [newCurrDir, parentDir]
              newCurrDirContentWithoutLinks = map (\c' -> newContent c') currDirContentWithoutLinks
              newCurrDir = Directory cName $ newCurrDirLinks ++ newCurrDirContentWithoutLinks
              newContent c' =
                case innerAlterTree c' newCurrDir objectiveDir componentName
                                    errorFunction errorMessage
                                    alteringFunction of
                  (Right dir) -> dir

              newObjectiveDir = alteringFunction objectiveDir parentDir componentName

        -- contains the path to the directory we altered
        (root, dirs) = break (\c -> c /= '/') $ pwd rootDir objectiveDir
        tokenizedPath = root: Helper.splitStrBy '/' dirs

        -- New Objective Directory
        -- This is the directory in the new tree
        -- which has the same path as objectiveDir in the old tree
        ngd =
          case innerAlterTree rootDir rootDir objectiveDir componentName
                              errorFunction errorMessage
                              alteringFunction of
            (Right newRoot) ->
              let newWd = case cd newRoot newRoot tokenizedPath of
                                  (Right dir) -> dir
              in
                Right (newRoot, newWd)
            (Left e) -> Left e

  mkdir :: Component -> Component -> String -> Either String (Component, Component)
  mkdir rootDir objectiveDir newDirName =
      alterTree rootDir objectiveDir newDirName
                (errorFunction) errorMessage
                alteringFunction
      where
        errorFunction contents =
          not $ isNothing (find (\c -> name c == newDirName) contents)

        errorMessage = ": Already exists in this Directory"

        alteringFunction :: Component -> Component -> String -> Component
        alteringFunction (Directory oName oContent) parentDir newDirName =
            newObjectiveDir
            where
              objDirContentsWithLinks = drop 2 oContent
              newDir = Directory newDirName [newDir, newObjectiveDir]
              newObjectiveDir = Directory oName $ [newObjectiveDir, parentDir] ++ objDirContentsWithLinks ++ [newDir]

  mkfile :: Component -> Component -> String -> Either String (Component, Component)
  mkfile rootDir objectiveDir newFileName =
      alterTree rootDir objectiveDir newFileName
                (errorFunction) errorMessage
                alteringFunction
      where
        errorFunction contents =
          not $ isNothing (find (\c -> name c == newFileName) contents)

        errorMessage = ": Already exists in this Directory"

        alteringFunction :: Component -> Component -> String -> Component
        alteringFunction (Directory oName oContent) parentDir newFileName =
            newObjectiveDir
            where
              objDirContentsWithLinks = drop 2 oContent
              newFile = File newFileName []
              newObjectiveDir = Directory oName $ [newObjectiveDir, parentDir] ++ objDirContentsWithLinks ++ [newFile]

  rm' :: Component -> Component -> String -> Either String (Component, Component)
  rm' rootDir objectiveDir fileName =
      alterTree rootDir objectiveDir fileName
                errorFunction errorMessage
                alteringFunction

      where
        match :: Component -> String -> Bool
        match c fName = 
                case c of
                  (File fName _) -> True
                  otherwise -> False

        errorFunction contents =
          (name rootDir) == (name objectiveDir) && isNothing (find (\c -> match c fileName) contents)

        errorMessage = ": file does not exist"

        alteringFunction :: Component -> Component -> String -> Component
        alteringFunction (Directory oName oContent) parentDir newFileName =
            newObjectiveDir
            where
              objDirContentsWithLinks = drop 2 oContent
              filteredCurrDirContentWithoutLinks = filter (\c -> not (match c fileName)) objDirContentsWithLinks
              newObjectiveDir = Directory oName $ [newObjectiveDir, parentDir] ++ filteredCurrDirContentWithoutLinks

  rm :: Component -> Component -> [String] -> Either String (Component, Component)
  rm root wd [] = Right (root, wd)
  rm root wd (fp:fps) =
      result
      where
        (s, ss) = break (\d -> d /= '/') fp
        pathTokens =
            case s of
              "/"       -> s:(Helper.splitStrBy '/' ss)
              otherwise -> Helper.splitStrBy '/' fp
        (path, (fileName:_)) = splitAt ((length pathTokens) - 1) pathTokens
        result = case cd root wd path of
          (Right newWd) ->
            case rm' root newWd fileName of
                  (Right (root', wd')) ->
                    rm root' wd'' fps
                    where
                      (root'', dirs) = break (\c -> c /= '/') $ pwd root wd
                      tokenizedPath = root'': Helper.splitStrBy '/' dirs
                      wd'' = case cd root' root' tokenizedPath of
                                  (Right dir) -> dir
                  (Left e) -> Left e
          (Left e) -> Left e

  handleInput :: [String] -> Component -> Component -> IO (Either String (Component, Component))
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
                  return $ case cd root wd pathTokens of
                        (Right dir) -> Right (root, dir)
                        (Left e) -> Left e
                "ls" -> do
                  putStrLn $ HaskileSystem.ls root wd pathTokens;
                  return $ Right (root, wd)
                "pwd" -> do
                  putStrLn $ pwd root wd
                  return $ Right (root, wd)
                "mkdir" -> do
                  return $ mkdir root wd args'
                "mkfile" -> do
                  return $ mkfile root wd args'
                "rm" -> do
                  return $ rm root wd args
                otherwise -> do
                  putStrLn $ "Unknown command."
                  return $ Right(root, wd)
        otherwise -> do
            return $ Right (root, wd)

  tokenizeInput :: String -> [String]
  tokenizeInput input = Helper.splitStrBy ' ' input

  readInput :: String -> IO [String]
  readInput wdName = do
      putStr $ wdName ++ "$ "
      hFlush stdout
      input <- getLine
      return $ tokenizeInput input