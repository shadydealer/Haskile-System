module HaskileSystem
(	run
,
)
where

	import Helpers
	import Components
	
	data HaskileSystem =
		HaskileSystem 	{	wd::Component --WorkingDirectory
									 	}


	run::IO ()
	run =
		let hs = HaskileSystem.init in
			cd (wd hs) (name $ wd hs)

 	init::HaskileSystem
 	init = HaskileSystem 	{	wd = Components.mkdir "/"
 												}

 	cd::Component -> String -> IO ()
 	cd (File name) 	_		= error $ name ++ " not a directory"
 	cd 	wd					""	= cd wd "."
	cd	wd 					"."	= do
		input <- readInput $ name wd
		putStrLn $ input !! 0

 -- 	tokenizeInput::String -> [String]
	-- tokenizeInput input = Helpers.splitStrBy ' ' input

	readInput::String -> IO [String]
	readInput wdName = do
		putStrLn $ wdName ++ "$"
		input <- getLine
		-- tokenizeInput input
		return [input]