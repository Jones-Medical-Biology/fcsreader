module MyLib (someFunc, runHCat) where

import qualified System.Environment as Env


someFunc :: IO ()
someFunc = putStrLn "someFunc"

runHCat :: IO ()
runHCat = handleArgs >>= displayMessage
  where
   displayMessage parsedArgument =
    case parsedArgument of
     Left errMessage ->
      putStrLn $ "Error: " <> errMessage
     Right filename ->
      readFile filename >>= putStrLn

handleArgs :: IO (Either String FilePath)
handleArgs =
  parseArgs <$> Env.getArgs
  where
   parseArgs argumentList =
    case argumentList of
     [fname] -> Right fname
     [] -> Left "Error: No arguments provided!"
     _ -> Left "garbage detected"


