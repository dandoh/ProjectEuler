
specialPyhagoreanTriplet = head [a * b * c | a <- [1..1000], b <- [a..1000], let c = 1000 - a - b, a^2 + b^2 == c^2]

main = print specialPyhagoreanTriplet
