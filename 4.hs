
largestPalindrome = 
    maximum [ z | x <- [100..999], y <- [x..999], let z = x * y, let s = show z, s == reverse s ]


main :: IO ()
main = print $ largestPalindrome

