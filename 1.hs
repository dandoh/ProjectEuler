sumMultiples :: [Int] -> Int
sumMultiples = sum . filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0)

main :: IO ()
main = print $ sumMultiples [1..999]
