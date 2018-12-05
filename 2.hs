v4M = 4000000

-- sumEvenFib :: Int -> Int
-- sumEvenFib bound = go 0 1 1
--   where
--     go acc prv cur
--         | cur > bound      = acc
--         | cur `mod` 2 == 0 = go (acc + cur) cur (prv + cur)
--         | otherwise        = go acc cur (prv + cur)
sumEvenFib bound = sum . takeWhile (<= bound) . filter even $ fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO () 
main = print $ sumEvenFib v4M
