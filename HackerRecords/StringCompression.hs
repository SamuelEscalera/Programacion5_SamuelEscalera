import Data.List

compressChar :: String -> String
compressChar [x] = [x]
compressChar xs = head xs : show (length xs)

compressString :: String -> String
compressString = concatMap compressChar.group