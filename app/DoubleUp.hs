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

-- Helper function where Integrals "x" and "n" are passed left to right, via CoMonad, into `rem` which is then evaluated into [n] and evaluated a second time using `EQ` == ,a Boolean, to determine if the remainder is equal to 0.
-- divBy :: Integral a => a -> [a] -> [a] though in our case it'll be y because this is the expression we're applying to our arguements. 
-- x being your input "year"
leapYear :: Integral y => y -> y -> Bool -- pass the Integral through the function. Don't double dribble. 
leapYear x y = x `rem` y == 0 

isLeapYear :: Integer -> Bool
isLeapYear year -- represented as x. "The tricky thing here is that a leap year in the Gregorian calendar occurs:"
    | divBy   4 = True -- "on every year that is evenly divisible by 4"
    | divBy 100 = False --"except every year that is evenly divisible by 100 unless the year is also evenly divisible by 400 "
    | divBy 400 = True
    | otherwise = False
    where
    divBy y = year `leapYear` y -- This part is tricky. "year" is x, as is defined above, and y is our 4,100,400 which is defined as an Int that returns a Bool. If we use a new variable "z" it'll return not in scope, because its not defined.
    -- (x divBy y) | 1993 / 4 | 1993 `rem` 4 | remainder == 0 


checkLeapYear :: Int -> Bool
checkLeapYear y = divBy 400 || (divBy 4 && not (divBy 100)) -- Thanks SimC. Y (boolean) is equal to the result of x `mod` y. divBy 400 on the left (&) and divBy 4 on the right to produce True && True. Otherwise True && False. 
    where divBy x = mod y x == 0
 

dasLeap :: Int -> Bool
dasLeap y = divBy 400 || (divBy 4 && not (divBy 100)) -- Thanks SimC. Y (boolean) is equal to the result of x `mod` y. divBy 400 on the left (&) and divBy 4 on the right to produce True && True. Otherwise True && False. 
    where divBy x = rem y x == 0 
    
-- java

-- year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)

-- if( x % 400 == 0){
--  return true;
--  }
--  if( x % 100 == 0){
--  return false;
--  }
--  if( x % 4 ==0){
--  return true;
--  }
--  return false;
