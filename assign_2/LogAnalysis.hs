--{-# OPTIONS_GHC -Wall #-}
--module LogAnalysis where
--    import Log

main = print(split "Ello 2 562 help help")

--parseMessage :: String -> LogMessage

split :: String -> [String]
split [] = []
split x
    | first x == [] = []
    | otherwise = first x : split(consume x)

consume :: String -> String
consume [] = []
consume (x:xs)
    | x == ' ' = xs
    | otherwise = consume xs

first :: String -> String
first [] = []
first (x:xs)
    | x == ' ' = []
    | otherwise = x : first xs