{-
seperates the last digit which is the mod 10, put into a list, appended to the result of the divide which is the rest of the numbers.
-}
toDigits :: Int -> [Int]
toDigits x
 |x <= 0 = []
 |otherwise = toDigits (x `div` 10) ++ [x `mod` 10]
 
{-
goes through the list doubling every second digit, the helper function is used so i can apply the reversal of the list before the duplication and reverse it back after it's done.
-}
doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther xs = reverse (doubleEveryOther' (reverse xs))
  where
    doubleEveryOther' [] = []
    doubleEveryOther' [x] = [x]
    doubleEveryOther' (x:y:xs) = x : 2*y : doubleEveryOther' xs
{-
sums each digit after checking whether it's over 9 or not using guards.
-}
sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits xs = x + sumDigits(tail xs)
	where x | head xs > 9 = head xs - 9
			| otherwise = head xs
--uses all the functions and checks if it's divisible by 10.
validate :: Int -> Bool
validate no = sumDigits (doubleEveryOther (toDigits (no) )) `mod` 10 == 0

--altmap func as requested.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

-- doubleEveryoOther with altmap

doubleEveryotheraltMap :: (a -> b) -> (a -> b) -> [a] -> [b]
doubleEveryotheraltMap _ _ [] = []

doubleEveryotheraltMap f g (x:xs) = f x : doubleEveryOtherAltMap g f xs
