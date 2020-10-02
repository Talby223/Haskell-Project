sweetSpellTokens :: String -> String
sweetSpellTokens s = unwords $ map fixToken $ words s

fixToken :: String -> String
fixToken s 
    | elem s ["$s1","$s2"] = "200" 
    | elem s [] = "150"
    | otherwise = "s"

main :: IO ()
main = do
  putStrLn $ sweetSpellTokens "Channel $s1 damage for $8 seconds"