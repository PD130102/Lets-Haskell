import System.IO (hFlush, stdout)
import Data.Char (toLower)

main :: IO ()
main = do
    babybot

babybot :: IO ()
babybot = do
    putStr "CB> "
    hFlush stdout
    input <- getLine
    putStrLn input
    if isGoodbye input
        then return ()
        else babybot

isGoodbye :: String -> Bool
isGoodbye input = map toLower input == "bye"

stupidbotCreator :: String -> String -> String
stupidbotCreator motto _ = motto

dalek :: String -> String
dalek = stupidbotCreator "Exterminate!"


matcherbotCreator :: String -> (String -> String)
matcherbotCreator pattern = matcherbot
  where matcherbot sentence =
          case dropWhile (/= pattern) (words sentence) of
            [] -> "False"
            (_:xs) -> unwords xs