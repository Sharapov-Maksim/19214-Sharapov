import Data.Char
import System.IO

hash :: String -> Capacity -> Integer
hash key maxSize = (foldl (\acc v -> (acc + v)) 0 (map (\ v -> toInteger(ord v)) key)) `mod` maxSize

type Capacity = Integer
type Size = Integer
type LoadFactor = Double
data Hashtable k v = ToHashtable [[(k,v)]] Capacity Size deriving (Show)

--instance (Show k, Show v) => Show (Hashtable k v) where
--    show (ToHashtable table cap size) = (foldl (\acc elem ->acc ++ show elem ++ "\n") "" table) ++ "\n"

defaultHashTable:: Integer -> Hashtable k v
defaultHashTable cap = (ToHashtable (replicate (fromInteger(cap)) []) cap 0)

loadFactor :: Hashtable k v -> LoadFactor
loadFactor (ToHashtable _ cap size) = (fromInteger size / fromInteger cap)


insert :: (Show k, Eq k) =>  Hashtable k v -> k -> v -> Hashtable k v
insert (ToHashtable table cap size) key value = if loadFactor (ToHashtable table cap size) > 0.9 then 
                                    insert (rehash (ToHashtable table size cap)) key value else
                                    (ToHashtable (left ++ mid ++ (drop 1 right)) cap (size+1)) where
                                            mid = [(table !! fromInteger(hash (show key) cap)) ++ [(key, value)]]
                                            (left, right) = splitAt (fromInteger(hash (show key) cap)) table

rehash ::(Show k, Eq k) => Hashtable k v -> Hashtable k v
rehash (ToHashtable table cap _) = foldl (\acc (k,v) -> insert acc k v) (defaultHashTable (cap * 3)) (concat table)


fromList::(Show k, Eq k)=> [(k,v)]->Hashtable k v
fromList table = foldl (\acc (k,v) -> (insert acc k v)) (defaultHashTable 15) table

clear::Hashtable k v -> Hashtable k v
clear table = defaultHashTable 15

erase::(Show k, Eq k)=>Hashtable k v->k->Hashtable k v
erase (ToHashtable table cap size) key = (ToHashtable (left ++ mid ++ (drop 1 right)) cap (size+1)) where
                                            mid = [[]]
                                            (left, right) = splitAt (fromInteger(hash (show key) cap)) table

contains::(Show k, Eq k)=>Hashtable k v -> k -> Bool
contains (ToHashtable table cap _) key = elem key (foldl (\acc (k,v) -> acc ++ k) [] (table !! fromInteger(hash (show key) cap)))



main = do
    handle <- openFile "input.txt" ReadMode
    hh <- hGetContents handle
    let content = [(head (words str), last (words str)) |str <- lines hh]
    print (fromList content)
    hClose handle






test = ToHashtable [[],[],[("Doom","3")],[("Ferrum","SO4"),("Eoom","65"),("Doon","221")],[],[("Lolik","Privet")],[],[("Mass","Effect")],[],[],[],[],[("Privet","Kreved")],[],[]] 15 7