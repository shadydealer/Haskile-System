module Component
( Component(..)
, ls
, writeFile'
) 
where

  data Component 
      = File{ name::String
            , fcontent::String
            }
      | Directory { name::String
                  , components::[Component]
                  } deriving (Show, Read, Eq)
 

  directoryDefault :: Component
  directoryDefault = Directory "untitled" []

  cp :: Component -> Component
  cp (File fromName fromCont)      =
      File fromName fromCont
  cp (Directory fromName fromCont) =
      Directory fromName $ map (\c -> cp c) fromCont

  --
  -- Returns a string containing:
  --  - the names of the components if the component is a Directory
  --  - name of the component if it's a File
  --
  ls :: Component -> String
  ls (Directory _ cont) = do
      foldl (\accum comp-> accum  ++ name comp ++ " ") "" cont
  ls (File name _) = do
      name

  writeFile' :: Component -> String -> Component
  writeFile' (Directory name _)      _   = error $ name ++ ": Is a directory"
  writeFile'  file                   ""  = file
  writeFile' (File fname fcontent) str  = File fname (fcontent ++ str)
