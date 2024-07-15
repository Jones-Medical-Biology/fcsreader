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
