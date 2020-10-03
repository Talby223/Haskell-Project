sweetSpellTokens :: String -> String
sweetSpellTokens s = unwords $ map fixToken $ words s

fixToken :: String -> String
fixToken s 
    | elem s ["$s1","$s2"] = "1" 
    | elem s ["$s2"] = "x"
    | elem s ["bigtiddygoth"] = "x"
    | elem s ["$202090u"] = "3" -- Teaching of the Monistoary
    | elem s ["x"] = "x" 
    | elem s ["$113746s1%"] = "5" 
    | elem s ["bigtiddygoth"] = "x"
    | elem s ["${$s1}.1"] = "10" -- mink mastery
    | otherwise = s

main :: IO ()
main = do
  putStrLn $ sweetSpellTokens "Channel $s1 damage for $8 seconds, increasing damage by ${$s1}.1 and stacking up to $202090u "