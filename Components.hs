module Components
(	Component(File)
, name
,	contents
,	mkdir
, ls
, readFile'
, writeFile'
) 
where

	data Component = 
		File { name::String }
		| Directory {	name::String
								,	contents::[Component]
								} deriving (Show)

	--
	-- Creates a directory by given name
	--
	mkdir::String -> Component
	mkdir dirName
		|	dirName == "" = directoryDefault
		| otherwise 		= directoryDefault { name = dirName }
		where directoryDefault = Directory 	{	name = "untitled"
																				, contents=[]
																				}

	--
	-- Lists directory contents
	--
	ls::Component -> [String]
	ls (Directory _ contents) =  map (\x -> name x) contents
	ls (File name) = [name]

	readFile'::Component -> IO String
	readFile' (Directory name _) 	= error $ name ++ " is a directory"
	readFile' (File name) 				= do
		fileData <- readFile name
		return fileData

	writeFile'::Component -> String -> IO ()
	writeFile' 	_ 								"" 	= return ()
	writeFile' (Directory name _) _		= error $ name ++ " is a directory"
	writeFile' (File name) 				str = do
		writeFile name str
		return ()
