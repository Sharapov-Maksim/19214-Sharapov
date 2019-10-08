map_l :: (a->b) -> [a]->[b]
map_l f xs = foldl (\ys x -> ys ++ [f x]) [] xs

map_r :: (a->b) -> [a]->[b]
map_r f xs = foldr (\x ys -> f x:ys) [] xs