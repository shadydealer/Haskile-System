module Helpers
(	splitStrBy
 ,
)
where

	splitStrBy::Char-> String-> [String]
	splitStrBy delim str =
		let pred = (\c -> c == delim) in
			case dropWhile pred str of
				"" 		-> []
				str'	-> w: splitStrBy delim str''
					where (w, str'') = break pred str'
