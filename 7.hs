
primes = 2 : filter (null . tail . numFactors) [3, 5..]

numFactors n = factors n primes
  where
    factors n (p:ps)
        | p * p > n      = [n]
        | n `mod` p == 0 = p : factors (n `div` p) (p:ps)
        | otherwise      = factors n ps

main :: IO ()
-- main = print $ length . take 1000000 $ primes
main = print $ primes !! 1000000
