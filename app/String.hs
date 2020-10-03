sweetSpellTokens :: String -> String
sweetSpellTokens s = unwords $ map fixToken $ words s

fixToken :: String -> String
fixToken s
    | elem s [ "${$s1}.1%","$113746s1%.", "${5*$s5*$s6/100}", "$123586m1", "$124273d.$?s322522[", "$132467s1", "$132463s1", "$119611o1", "$119611d.", "$o1"]  = "10" -- Small damage
    | elem s [ "$?s137025[${4*$107270s1*$<CAP>/$AP}][${4*$107270s1}]", "${5*$s5}"]  = "100" -- Big damage 
    | elem s [ "$d"]  = "1 second" -- Small Duration
    | elem s [ "$d.", "$119085d,", "$123586d."]  = "1.5 second" -- Fractional Duration
    | elem s [ "$?s137025[${$185099s1*$<CAP>/$AP}][$185099s1]", "$113656d"]  = "5" -- Rising Sun Kick damage
    | elem s [ "$115804d][]."]  = "10 seconds" -- Debuff duration
    | elem s [ "${$s1}.1%","$124273d.$?s322522["]  = "10" -- Mastery
    | elem s [ "$107270A1"]  = "5" -- Melee Range
    | elem s [ "$123586A1", "$132466a2"]  = "8" -- AoE Range
    | elem s [ "$119085m1%"]  = "30%" -- Percentages
    | elem s [ "$s5%"]  = "35%" -- Percentages
    | elem s [ "bigtiddygoths"]  = "50%" -- Percentages
    | elem s [ "$123586s2%"]  = "70%" -- Percentages
    | elem s [ "$xxx$"]  = "10" -- Chains / Bounces
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "${$s1}.1%","$113746s1%.", "$148135s1","$130654s1", "$s1", "$?s137025[${$s1*$<CAP>/$AP}][$s1]"]  = "10" -- Small damage
    | elem s [ "${$m1/-1000}"]  = "x" -- x value
    | elem s [ "$m2."]  = "n" -- n value
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "$xxx$"]  = "10" -- Placeholder
    | elem s [ "bigtiddygoth"]  = "10" -- Placeholder
    | elem s [ "Surging Mist,][]"]  = "Surging Mist" -- Placeholder
    | elem s [ "target.$?s117907["]  = "target" -- Placeholder
    | elem s [ "Mist$?s227344[,"]  = "Mist," -- Placeholder
    | elem s [ "yds.$?c3["]  = "yds" -- Placeholder Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "damage.$?a137384[", "damage.$?s261917[", "damage$?s128595[,"]  = "damage" -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "path.$?c1["]  = "path" -- Manual Fix to bad tokens. Only common abilities. 
    | otherwise = s

main :: IO ()
main = do
  putStrLn $ sweetSpellTokens "Heals the target for $o1 over $d. While channeling, Enveloping Mist$?s227344[, Surging Mist,][] and Vivify may be cast instantly on the target.$?s117907[\r\r"
