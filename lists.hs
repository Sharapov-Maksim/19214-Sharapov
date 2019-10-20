get::[a] -> Integer -> a
get [] _ = error "no such element"
get (x:xs) 0 = x
get (x:xs) n = get xs (n-1)


head':: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x


last':: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (x:xs) = last' xs


tail':: [a] -> [a]
tail' [] = error "empty list"
tail' (x:xs) = xs


init':: [a] -> [a]
init' [] = error "empty list"
init' [x] = []
init' (x:xs) = x:init' xs


reverse':: [a] -> [a]
reverse' ys = rev [] ys
    where
        rev acc [] = acc
        rev acc (x:xs) = rev (x:acc) xs


length':: [a] -> Integer
length' ys = len 0 ys
    where
        len acc [] = acc
        len acc (x:xs) = len (acc+1) xs



append' :: [a] -> a -> [a]
append' [] x = [x]
append' (y:xs) x = y : append' xs x


concat' :: [a] -> [a] -> [a]
concat' [] ys = ys
concat' (x:xs) ys = x : concat' xs ys




drop' :: Integer -> [a] -> [a]
drop' _ [] = [] -- or error
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs


take' :: Integer -> [a] -> [a]
take' _ [] = [] -- or error
take' 0 xs = []
take' n (x:xs) = x : take' (n-1) xs


splitAt' :: Integer -> [a] -> ([a],[a])
splitAt' n xs = (take' n xs, drop' n xs)


null' :: [a] -> Bool
null' [] = True
null' xs = False


elem' ::Eq a => [a] -> a -> Bool
elem' [] _ = False
elem' (x:xs) y = if x == y then True else elem' xs y


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' test (x:xs) = if test x  then x:filter' test xs
                                        else filter' test xs


map' :: (a->b) -> [a]->[b]
map' _ [] = []
map' f (x:xs) = (f x) : map' f xs


zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' [x] [] = []
zip' [] [y] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
