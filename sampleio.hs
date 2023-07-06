name2reply :: String -> String
name2reply name =
    "Pleased to meet you, "
    ++ name
    ++ ".\n"
    ++ "Your name contains "
    ++ show (length name)
    ++ " characters."

-- main :: IO ()
-- main = do
--     putStrLn "Greetings once again. What is your name?"
--     inpStr <- getLine
--     let outStr = name2reply inpStr
--     putStrLn outStr

--if else if example
main = do
    putStrLn "Greetings once again. What is your name?"
    inpStr <- getLine
    if inpStr == "Simon" then putStrLn "Hello, Simon, my old friend!" else if inpStr == "Paul" then putStrLn "Hello, Paul, my old friend!" else putStrLn "I don't know you."