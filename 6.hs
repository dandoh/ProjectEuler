
sumSquareDifference = squareSum - sumSquare
  where
    sumSquare = sum . map (^2) $ [1..100]
    squareSum = (^2) . sum $ [1..100]

main :: IO ()
main = print $ sumSquareDifference
