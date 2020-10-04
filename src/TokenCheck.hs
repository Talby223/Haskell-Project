sweetSpellTokens :: String -> String
sweetSpellTokens s = unwords $ map fixToken $ words s

fixToken :: String -> String
fixToken s
    | elem s [ "${$s1}.1%","$113746s1%.", "${5*$s5*$s6/100}", "$123586m1", "$124273d.$?s322522[", "$132467s1", "$132463s1", "$119611o1", "$119611d.", "$o1", "$s1$?s274586[", "$s2", "${(1+$d/$t1)*$148187s1}", "$<newshield>", "$227291s1", "$343819s1", "$s3.", "$345727s1", "$345727s2", "$327264s1"]  = "10" -- Small damage
    | elem s [ "$?s137025[${4*$107270s1*$<CAP>/$AP}][${4*$107270s1}]", "${5*$s5}", "$m1", "$198533o1", "$125355s1", "$196733s1", "$311123s1"]  = "100" -- Big damage 
    | elem s [ "$d", "$120954d", "$117952d", " $338321d"]  = "1 second" -- Small Duration
    | elem s [ "$d.", "$119085d,", "$123586d.", "$198533d."]  = "1.5 second" -- Fractional Duration
    | elem s [ "$?s137025[${$185099s1*$<CAP>/$AP}][$185099s1]", "$113656d", "$196608o2", "$196608o1", "$312106u"]  = "5" -- Rising Sun Kick damage
    | elem s [ "$115804d][].", "$196608d."]  = "10 seconds" -- Debuff duration
    | elem s [ "${$s1}.1%","$124273d.$?s322522["]  = "10" -- Mastery
    | elem s [ "$107270A1", "$A1", "$325209o1", "$196733A1", "$196741u"]  = "5" -- Melee Range
    | elem s [ "$123586A1", "$132466a2", " $162530A1", "$345727a1"]  = "8" -- AoE Range
    | elem s [ "$?c1[${$117906bc1*$s1}]?c2[${$117907bc1*$s1}][${$115636bc1*$s1}]%."]  = "10%" -- Placeholder
    | elem s [ "$311054s1", "${$s8/1000}"]  = "1%" -- Percentages
    | elem s [ "$m2%"]  = "20%" -- Percentages
    | elem s [ "$197916s1%."]  = "25%" -- Placeholder
    | elem s [ "$119085m1%"]  = "30%" -- Percentages
    | elem s [ "$s5%"]  = "35%" -- Percentages
    | elem s [ "${100+$m1}%"]  = "45%" -- SEF
    | elem s [ "$m3%", "$196733m2%"]  = "50%" -- Percentages
    | elem s [ "$123586s2%"]  = "70%" -- Percentages
    | elem s [ "${$s1}.1%","$113746s1%.", "$148135s1","$130654s1", "$s1", "$?s137025[${$s1*$<CAP>/$AP}][$s1]"]  = "10" -- Small damage
    | elem s [ "${$m1/-1000}", "$s2%", "$s1%,][]", "$A1", "$s1,", "$s3,", "$163178A1", "$115804d.", "$116706d", "$?a197895[$u", "$s4%", "$343737o1", "$343737d.", "$311123s2"]  = "x" -- x value
    | elem s [ "$m2.", "$343818s3", "${$m1+1}", "$311054d,"]  = "n" -- n value
    | elem s [ "$<health>%,$?s322960[", "$s2%.", "$116189m3%", "${$s1}.1%", "$s1%", "$202090u.", "$107270A1", "$m2%][].", "$s3%", "$322740s1%", "$343820s1%,", "$338321m1%", "$s1%.", "$325209s3%.", " $312106s1%", "|cFFFFFFFF${$s1}.1%|r"]  = "x%" -- x percent
    | elem s [ "$215479d.", "$d,", "$d.", "$124280d.", "$325209d,", "$d$?a231605[,", "$?s115069[$s1 sec][$d].", "$196733d.", "$312106d,"]  = "x seconds" -- x seconds
    | elem s [ "${$s1+1}"]  = "2" -- charges
    | elem s [ "$m1%", "$m2", "][]$?a137023"]  = "" -- experiment
    | elem s [ "$h%"]  = "unknown" -- experiment
    | elem s [ "${3*$158221s1}", "${$162530s1*13}"]  = "a ton of" -- Placeholder
    | elem s [ "${$100784m3/1000}.1", "$196741s1%", "${$m2/1000}", "${$s2/1000}.1"]  = "1" -- Placeholder
    | elem s [ "enemies$?s322740[", "enemies][]"]  = "enemies" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "$?a137025[Rising"]  = "Rising" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "Stagger.$?s322510["]  = "Stagger" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "target$?a154436[,"]  = "target" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "spells][spell]"]  = "spells" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "instead.][]"]  = "instead" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "attacks.$?s343731["]  = "attacks" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "it.$?s322719["]  = "it" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "you$?s328670["]  = "you" --Manual Fix to bad tokens. Only common abilities.
    | elem s [ "speed.][.]$?s115315["]  = "speed" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "movement][]"]  = "movement" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "action$?s328682["]  = "action" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "Surging Mist,][]"]  = "Surging Mist" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "target.$?s117907["]  = "target" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "Mist$?s227344[,"]  = "Mist," -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "yds.$?c3["]  = "yds" -- Manual Fix to bad tokens. Only common abilities.
    | elem s [ "damage.$?a137384[", "damage.$?s261917[", "damage$?s128595[,", "$<damage>%.", "damage$?s343744[", "damage,][]", "damage$?s117906[", "damage].", "damage.]"]  = "damage" -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "path.$?c1["]  = "path" -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "sec.][]$?a137023"]  = "sec" -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "[Keg"]  = "Keg" -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "times.][]$?a137024[Essence"]  = "times. Essence" -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "$?a137024[Up"]  = "Up" -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "allies]?a137023|?a137025[Up"]  = "allies or Up" -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "$?a137023[are"]  = "are" -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "Fire]?a137024[are"]  = "Fire or are" -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "bolt][suffer"]  = "bolt or suffer" -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "$?a137023[are"]  = "are" -- Manual Fix to bad tokens. Only common abilities. 
    | elem s [ "|cFFFFFFFF${$s1}.1%|r.][When"]  = "x %. When" -- Manual Fix to bad tokens. Only common abilities. 
    | otherwise = s

main :: IO ()
main = do
  putStrLn $ sweetSpellTokens "$?s152173[Serenity's damage bonus is increased by |cFFFFFFFF${$s1}.1%|r.][When your Storm, Earth, and Fire spirits fixate on the same target, they deal |cFFFFFFFF${$s1}.1%|r increased damage.]"
