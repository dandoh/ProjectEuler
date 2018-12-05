
primes = 2 : filter (null . tail . numFactors) [3, 5..]

numFactors n = factors n primes
  where
    factors n (p:ps)
        | p * p > n      = [n]
        | n `mod` p == 0 = p : factors (n `div` p) (p:ps)
        | otherwise      = factors n ps

sumPrimesBelow k = sum . takeWhile (<k) $ primes


main :: IO ()
main = print $ sumPrimesBelow 2000000
