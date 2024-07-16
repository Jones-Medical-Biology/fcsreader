module Misc (factors
            ,factors'
            ,primes
            ,primes'
            ,primesrev
            ,primesrev'
            ) where

import Prelude

factors num = 
  factors' num 2
  where
    factors' num fact
      | num == 1 = []
      | (num `rem` fact) == 0 = fact : factors' (num `div` fact) fact
      | otherwise = factors' num (fact + 1)

primes n =
  if (Prelude.tail . factors) n == []
v  then n : primes (n + 1)
  else primes (n + 1)

primes' n =
  let xs = factors n
      x  = (Prelude.head xs) : []
  in
    if x == xs
    then n : primes' (n + 1)
    else
      primes' (n + 1)

primesrev n =
  if n < 2
  then []
  else
    if (Prelude.tail . factors) n == []
    then n : primesrev (n - 1)
    else primesrev (n - 1)

primesrev' n =
  let xs = factors n
      x  = (Prelude.head xs) : []
  in
    if n < 2
    then []
    else    
      if x == xs
      then n : primesrev' (n - 1)
      else
        primesrev' (n - 1)

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
