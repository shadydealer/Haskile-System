module Helper
( splitStrBy
 ,
)
where

  splitStrBy :: Char -> String -> [String]
  splitStrBy delim str =
      let pred = (\c -> c == delim) in
        case dropWhile pred str of
          ""   -> []
          str' -> w: splitStrBy delim str''
            where (w, str'') = break pred str'

  secondToLast :: [a] -> Either String a
  secondToLast []       = Left "error: Empty list"
  secondToLast (x:_:[]) = Right x
  secondToLast (_:xs)   = secondToLast xs