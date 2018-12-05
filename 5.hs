
primes :: [Int]
primes = 2 : filter ((==1) . length . primeFactors) [3, 5..]

primeFactors :: Int -> [Int]
primeFactors n = factor n primes
  where
    factor n (p:ps)
        | p * p > n      = [n]
        | n `mod` p == 0 = p : factor n ps
        | otherwise      = factor n ps


smallestMultiple = foldl combine 1 . foldl1 (zipWith max) . map analyze $ [1..20]
  where
    combine acc (n, p) = acc * n ^ p

-- smallestMultiple = foldr1 lcm [1..20] ---> Faster solution

analyze num = zip ps . map (count num) $ ps
  where
    ps = takeWhile (<=20) primes
    count num p
        | num `mod` p == 0 = 1 + count (num `div` p) p
        | otherwise        = 0

main :: IO ()
main = print $ smallestMultiple
