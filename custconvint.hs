str2Int :: String -> Int -- custconvint.hs
str2Int x = read x :: Int

main = do
  putStrLn ("Enter number1:")
  input1 <- getLine
  let num1 = str2Int input1
  print (num1)

  putStrLn ("Enter number2:")
  input2 <- getLine
  let num2 = str2Int input2
  print (num2)
