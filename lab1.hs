import Data.Char
toDecimal :: Int -> String -> String
toDecimal base snumber = show (helper base snumber)
    where
        helper 1 str = length str - 1
        helper base [x] = if digitToInt' x < base then digitToInt' x else error "Going abroad base"
        helper base (x:snumber) = if (digitToInt' x < base) then base * digitToInt' x + helper base snumber
                                       else error "Going abroad base"
        digitToInt' a = if ((ord a >= 97)&&(ord a <=122)) then ord a - 87 
        else if ((ord a >= 65)&&(ord a <=90)) then ord a - 29 
        else if ((ord a >= 48)&&(ord a <=57)) then ord a - 48 else error "No such digit"
{-        convert '0' = 0
        convert '1' = 1
        convert '2' = 2
        convert '3' = 3
        convert '4' = 4
        convert '5' = 5
        convert '6' = 6
        convert '7' = 7
        convert '8' = 8
        convert '9' = 9
        convert 'a' = 10
        convert 'b' = 11
        convert 'c' = 12
        convert 'd' = 13
        convert 'e' = 14
        convert 'f' = 15
        convert 'g' = 16
        convert 'h' = 17
        convert 'i' = 18
        convert 'j' = 19
        convert _ = error "No such digit"-}

fromDecimal :: Int -> String -> String
fromDecimal toBase snumber = convertTo toBase (decNum snumber)
    where
        --decNum :: String -> Int
        decNum [x] = if (digitToInt' x < 10) then digitToInt' x else error "Going abroad base"
        decNum (x:snumber) = if (digitToInt' x < 10) then 10^length(snumber) * digitToInt' x + decNum snumber
                                       else error "Going abroad base"
        digitToInt' a = if ((ord a >= 48)&&(ord a <=57)) then ord a - 48 else error "No such digit"
        --convertTo :: Int -> Int -> String
        convertTo toBase 0 = ""
        convertTo toBase numb = convertTo toBase (numb `div` toBase) ++ [intToDigit (numb `mod` toBase)]
        intToDigit b = if (b >= 0) && (b <= 9) then chr (b + 48) 
        else if ((b>=10) && (b<=35)) then chr (b+87) 
        else if ((b>=36) && (b<=61)) then chr (b+29) 
        else error "No such digit"


convertFromTo :: Int -> Int -> String -> String
convertFromTo fromBase toBase snumber = convertTo' toBase (decNum' fromBase snumber)
    where
        --decNum' :: Int -> String -> Int
        decNum' fromBase [x] = if (digitToInt' x < fromBase) then digitToInt' x else error "Going abroad base"
        decNum' fromBase (x:snumber) = if (digitToInt' x < fromBase) then fromBase^length(snumber) * digitToInt' x + decNum' fromBase snumber
                                       else error "Going abroad base"
        digitToInt' a = if ((ord a >= 97)&&(ord a <=122)) then ord a - 87 
        else if ((ord a >= 65)&&(ord a <=90)) then ord a - 29 
        else if ((ord a >= 48)&&(ord a <=57)) then ord a - 48 else error "No such digit"
        --convertTo' :: Int -> Int -> String
        convertTo' toBase 0 = ""
        convertTo' toBase numb = convertTo' toBase (numb `div` toBase) ++ [intToDigit (numb `mod` toBase)]
        intToDigit b = if (b >= 0) && (b <= 9) then chr (b + 48) 
        else if ((b>=10) && (b<=35)) then chr (b+87) 
        else if ((b>=36) && (b<=61)) then chr (b+29) 
        else error "No such digit"

