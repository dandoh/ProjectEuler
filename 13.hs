
main :: IO ()
main = do
    content <- readFile "p13.txt"
    let xs = map read . lines $ content
    print . take 10 . show . sum $ xs

