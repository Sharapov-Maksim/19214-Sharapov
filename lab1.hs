import Data.Char
toDecimal :: Int -> String -> String
toDecimal base snumber = show (helper base snumber)
    where
        helper 1 str = length1 str - 1
        helper base [x] = if digitToInt' x < base then digitToInt' x else error "Going abroad base"
        helper base (x:snumber) = if (digitToInt' x < base) then base * digitToInt' x + helper base snumber
                                       else error "Going abroad base"
        digitToInt' a = if ((ord a >= 97)&&(ord a <=122)) then ord a - 87 
        else if ((ord a >= 65)&&(ord a <=90)) then ord a - 29 
        else if ((ord a >= 48)&&(ord a <=57)) then ord a - 48 else error "No such digit"
        length1 ys = len 0 ys
        len acc [] = acc
        len acc (x:xs) = if (x == '1') then len (acc+1) xs else error "Going abroad base"


fromDecimal :: Int -> String -> String
fromDecimal toBase snumber = convertTo toBase (decNum snumber)
    where
        --decNum :: String -> Int
        decNum [x] = if (digitToInt' x < 10) then digitToInt' x else error "Going abroad base"
        decNum (x:snumber) = if (digitToInt' x < 10) then 10^length(snumber) * digitToInt' x + decNum snumber
                                       else error "Going abroad base"
        digitToInt' a = if ((ord a >= 48)&&(ord a <=57)) then ord a - 48 else error "No such digit"
        --convertTo :: Int -> Int -> String
        convertTo 1 numb = print1 numb
        convertTo toBase 0 = ""
        convertTo toBase numb = convertTo toBase (numb `div` toBase) ++ [intToDigit (numb `mod` toBase)]
        intToDigit b = if (b >= 0) && (b <= 9) then chr (b + 48) 
        else if ((b>=10) && (b<=35)) then chr (b+87) 
        else if ((b>=36) && (b<=61)) then chr (b+29) 
        else error "No such digit"
        print1 0 = "1"
        print1 n = print1 (n-1) ++ "1"


convertFromTo :: Int -> Int -> String -> String
convertFromTo fromBase toBase snumber = convertTo' toBase (decNum' fromBase snumber)
    where
        --decNum' :: Int -> String -> Int
        decNum' 1 snumber = length1 snumber
        decNum' fromBase [x] = if (digitToInt' x < fromBase) then digitToInt' x else error "Going abroad base"
        decNum' fromBase (x:snumber) = if (digitToInt' x < fromBase) then fromBase^length(snumber) * digitToInt' x + decNum' fromBase snumber
                                       else error "Going abroad base"
        digitToInt' a = if ((ord a >= 97)&&(ord a <=122)) then ord a - 87 
        else if ((ord a >= 65)&&(ord a <=90)) then ord a - 29 
        else if ((ord a >= 48)&&(ord a <=57)) then ord a - 48 else error "No such digit"
        --convertTo' :: Int -> Int -> String
        convertTo' 1 numb = print1 numb
        convertTo' toBase 0 = ""
        convertTo' toBase numb = convertTo' toBase (numb `div` toBase) ++ [intToDigit (numb `mod` toBase)]
        intToDigit b = if (b >= 0) && (b <= 9) then chr (b + 48) 
        else if ((b>=10) && (b<=35)) then chr (b+87) 
        else if ((b>=36) && (b<=61)) then chr (b+29) 
        else error "No such digit"
        length1 ys = len (-1) ys
        len acc [] = acc
        len acc (x:xs) = if (x == '1') then len (acc+1) xs else error "Going abroad base"
        print1 0 = "1"
        print1 n = print1 (n-1) ++ "1"
