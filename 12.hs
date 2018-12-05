import           Data.List (group)
primes = 2 : filter (null . tail . primeFactors) [3, 5..]

primeFactors n = factors n primes
  where
    factors n (p:ps)
        | p * p > n = [n]
        | n `mod` p == 0 = p : factors (n `div` p) (p:ps)
        | otherwise = factors n ps

-- numDivisors :: Int -> Int
-- numDivisors n = length . filter ((==0) . (n `mod`)) $ [1..n]
numDivisors = product . map ((+1) . length) . group . primeFactors

solve :: Int -> Int
solve k = x * (x + 1) `div` 2
  where
    x = head . dropWhile (not . satisfied) $ [1..]
    satisfied n
        | n `mod` 2 == 0 = numDivisors (n `div` 2) * numDivisors (n + 1) > k
        | otherwise      = numDivisors n * numDivisors ((n + 1) `div` 2) > k

main :: IO ()
main = print $ solve 500


