sweetSpellTokens :: String -> String
sweetSpellTokens s = unwords $ map fixToken $ words s

fixToken :: String -> String
fixToken s = if s == "$" then "200" else s

main :: IO ()
main = do
  putStrLn $ sweetSpellTokens "Channel $s1 damage for $8 seconds"