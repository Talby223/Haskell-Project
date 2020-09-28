{-# LANGUAGE DataKinds #-}
module DoubleUp where 


doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = x*2 + y*2   

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2   

doubleSmallNumber' :: (Num a, Ord a) => a -> a
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1  

-- Write psudo-code 
-- if divisible by 400 then true
-- if divisibly by 4 then true
-- if divisibly by 100 then false

-- Helper function where Integrals "x" and "n" are passed left to right, via CoMonad, into `rem` which is then evaluated into [n] and evaluated a second time using `EQ` == ,a Boolean, to determine if the remainder is equal to 0.
-- divBy :: Integral a => a -> [a] -> [a] though in our case it'll be y because this is the expression we're applying to our arguements. 
-- x being your input "year"
leapYear :: Integral y => y -> y -> Bool -- pass the Integral through the function. Don't double dribble. 
leapYear x y = x `rem` y == 0 

isLeapYear :: Integer -> Bool
isLeapYear year -- represented as x. "The tricky thing here is that a leap year in the Gregorian calendar occurs:"
    | divBy   4 = True -- "on every year that is thisly divisible by 4"
    | divBy 100 = False -- "except every year that is thisly divisible by 100 unless the year is also thisly divisible by 400 "
    | divBy 400 = True
    | otherwise = False
    where
    divBy y = year `leapYear` y -- This part is tricky. "year" is x, as is defined above, and y is our 4,100,400 which is defined as an Int that returns a Bool. If we use a new thisiable "z" it'll return not in scope, because its not defined.
    -- (x divBy y) | 1993 / 4 | 1993 `rem` 4 | remainder == 0 


checkLeapYear :: Int -> Bool
checkLeapYear y = divBy 400 || (divBy 4 && not (divBy 100)) -- Thanks SimC. Y (boolean) is equal to the result of x `mod` y. divBy 400 on the left (&) and divBy 4 on the right to produce True && True. Otherwise True && False. 
    where divBy x = mod y x == 0
 

dasLeap :: Int -> Bool
dasLeap y = divBy 400 || (divBy 4 && not (divBy 100)) -- Thanks SimC. Y (boolean) is equal to the result of x `mod` y. divBy 400 on the left (&) and divBy 4 on the right to produce True && True. Otherwise True && False. 
    where divBy x = rem y x == 0 
    
-- java

-- year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)

--  if( x % 400 == 0){
--  return true;
--  }
--  else if( x % 100 == 0){
--  return false;
--  }
--  else if( x % 4 ==0){
--  return true;
--  }
--  return false;

butHow :: Int -> IO ()
butHow this = do
   if even this 
      then putStrLn "Number is Even" 
   else putStrLn "Number is Odd"

-- Given an age in seconds, calculate how old someone would be on:
-- Mercury: orbital period 0.2408467 Earth years
-- Venus: orbital period 0.61519726 Earth years
-- Earth: orbital period 1.0 Earth years, 365.25 Earth days, or 31557600 seconds
-- Mars: orbital period 1.8808158 Earth years
-- Jupiter: orbital period 11.862615 Earth years
-- Saturn: orbital period 29.447498 Earth years
-- Uranus: orbital period 84.016846 Earth years
-- Neptune: orbital period 164.79132 Earth years

-- Hint: 
-- ageOn :: Planet -> Float -> Float


-- the goal is to type ageOn Mercury <input seconds> and have it return the age in years 

--      31557600 seconds is 1 year
--      86459 seconds is 1 day 

--      Calculate your age in days
-- Years              | Multiply by 365
-- Months             | Multiply by 30
-- Days               | Multiply by 1 
--      Result = exact age in days

--Mercury 88.0 days   | D/88 =
--Venus 225.0 days    | D/225 =
--Earth 365.25 days   | D/365 =
--Mars 687.0 days     | D/687 =
--Jupiter 11.8 years  | D/(11.8 x 365) =
--Saturn 29.4 years   | D/(29.4 x 365) =
--Uranus 84.0 years   | D/84 x 365) =
--Neptune 164.0 years | D/164 x 365) = 
   
--      Seconds -> convert to Days -> Calculate Age

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune 

orbitalPeriod :: Planet -> Float
orbitalPeriod planet =
  case planet of
    Mercury ->  0.2408467 
    Venus ->  0.61519726 
    Earth ->  1.0
    Mars ->  1.8808158 
    Jupiter ->  11.862615 
    Saturn ->  29.447498 
    Uranus ->  84.016846 
    Neptune ->  164.79132 
  


ageOn :: Planet -> Float -> Float
ageOn planet seconds =
     seconds / 31557600.0 / orbitalPeriod planet


-- The quick brown fox jumps over the lazy dog. Is a Pangram. I need to determin if a sentence is a pangram.

-- A pangram uses every letter once
-- check to see if a sentence uses each latter of the alphabet
-- i have no idea how to write psduocode 

sweetSpellTokens :: String -> String
sweetSpellTokens s = unwords $ map fixToken $ words s

fixToken :: String -> String
fixToken s = if s == "$s1" then "200" else s

main :: IO ()
main = do
  putStrLn $ sweetSpellTokens "Channel $s1 damage for 8 seconds"