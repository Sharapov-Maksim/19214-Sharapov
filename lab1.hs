import Data.Char
toDecimal :: Int -> String -> String
toDecimal base snumber = show (helper base snumber)
    where
        helper 1 str = length1 str - 1
        helper base snumber = if (base <=62)&&(base>0) then foldl (\acc x -> base*acc + digitToInt' x) 0 snumber
                                                       else error "Bad base input"
        digitToInt' a = if ((a >= 'a')&&(a <='z')) then ord a - 87 
        else if ((a >= 'A')&&(a <='Z')) then ord a - 29 
        else if ((a >= '0')&&(a <='9')) then ord a - 48 else error "No such digit"
        length1 ys = len 0 ys
        len acc [] = acc
        len acc (x:xs) = if (x == '1') then len (acc+1) xs else error "Going abroad base"


fromDecimal :: Int -> String -> String
fromDecimal 1 numb = replicate ((read numb :: Int)+1) '1'
fromDecimal toBase snumber = convertTo toBase (read snumber)
    where
        convertTo toBase 0 = ""
        convertTo toBase numb = if (toBase>0) && (toBase<=62) then convertTo toBase (numb `div` toBase) ++ [intToDigit (numb `mod` toBase)]
                                                              else error "Bad base input"
        intToDigit b = if (b >= 0) && (b <= 9) then chr (b + 48) 
        else if ((b>=10) && (b<=35)) then chr (b+87) 
        else if ((b>=36) && (b<=61)) then chr (b+29) 
        else error "No such digit"


convertFromTo :: Int -> Int -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)
