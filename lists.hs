get::[a] -> Int -> a
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
reverse' [] = []
reverse' xs = last' xs : reverse'(init' xs)


length':: [a] -> Int
length' [] = 0
length' (x:xs) =1 + length' xs


append :: [a] -> a -> [a]
append xs x = reverse'(x:(reverse' xs))


concat' :: [a] -> [a] -> [a]
concat' xs [] = xs
concat' xs (y:ys) = concat' (append xs y) ys 


drop' :: Int -> [a] -> [a]
drop' _ [] = [] -- or error
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs


take' :: Int -> [a] -> [a]
take' _ [] = [] -- or error
take' 0 xs = []
take' n (x:xs) = x : take' (n-1) xs


splitAt' :: Int -> [a] -> ([a],[a])
--splitAt' _ [] = error
splitAt' n xs = (take n xs, drop n xs)


null' :: [a] -> Bool
null' [] = True
null' xs = False


elem' ::Eq a => [a] -> a -> Bool
elem' [] _ = False
elem' (x:xs) y = if x == y then True else elem' xs y


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' test (x:xs) = if test x == True then x:filter' test xs
                                        else filter' test xs


map' :: (a->b) -> [a]->[b]
map' _ [] = []
map' f (x:xs) = (f x) : map' f xs


zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' [x] [] = []
zip' [] [y] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys










