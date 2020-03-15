import Prelude as P
import Data.Char
import Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA1 as SHA1 
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Char8 as BSC
------------------------------ Concurency
import Control.Concurrent
import Control.Monad
import Control.DeepSeq


bruteForce :: ByteString -> [[Char]] -> String
bruteForce x [] = ""
bruteForce x (word:ws)  | encode(SHA1.hash (BSC.pack word)) == x = word
                        | otherwise = bruteForce x ws


workerLoop :: MVar [String] -> MVar [ [Char] ] -> ByteString -> IO ()
workerLoop taskQueue resultQueue hashFind = do
  maybeTask <- modifyMVar taskQueue
                 (\q -> return $ case q of
                                   [] -> ([], Nothing)
                                   xs -> (P.drop 30 xs, Just (P.take 30 xs)))
  case maybeTask of
    Nothing -> return ()
    Just task -> do
      let rslt = bruteForce hashFind task
      --let tmp = P.map (\x -> (x, encode(SHA1.hash (BSC.pack x)) == hashFind)) task
      --let rslt = if encode(SHA1.hash (BSC.pack task)) == hashFind then task else []
      --let rslt = P.head $ P.filter (\(val, isfind) -> isfind) tmp
      --let res = if snd rslt then fst rslt else []
      rslt `deepseq` modifyMVar_ resultQueue (\q -> return (rslt:q))
      workerLoop taskQueue resultQueue hashFind

mainLoop :: MVar [ [Char] ] -> Int -> IO ()
mainLoop _ 0 = return ()
mainLoop resultQueue taskNumber = do
  results <- modifyMVar resultQueue (\q -> return ([], q))
  let rslt = P.filter (\x -> x /= "") results
  case rslt of
    [] -> do
      threadDelay 100000 -- 100 ms
      mainLoop resultQueue taskNumber
    _ -> do
      P.putStrLn $ P.head rslt
      return()
      --mainLoop resultQueue (taskNumber - P.length results)



main :: IO()
main = do
         --x <- P.getLine
         let x = "7b21848ac9af35be0ddb2d6b9fc3851934db8420"
         let taskNumber = (P.length pull) ^ 5    -- общее кол-во вариантов пароля
         let hashFind = BSC.pack x
         --let res = bruteForce (BSC.pack x) allPasses
         --P.putStrLn (show res)
         workerNumber <- getNumCapabilities
         taskQueue <- newMVar allPasses
         resultQueue <- newMVar []
         workerNumber `replicateM_` forkIO (workerLoop taskQueue resultQueue hashFind)
         mainLoop resultQueue taskNumber
         return()
        

allPasses = [a++b++c++d++e | a <- pull,b <- pull,c <- pull,d <- pull,e <- pull]
pull::[String]
pull = "" : (P.map show [0..9]) ++ (charsToStrings letters) ++ (charsToStrings(P.map toUpper letters))
letters = "abcdefghijklmnopqrstuvwxyz"
charsToStrings :: [Char] -> [String]
charsToStrings [x] = [[x]]
charsToStrings (x:xs) = [[x]] ++ (charsToStrings xs)




--encode $ SHA1.hash (BSC.pack "1111")   
--da39a3ee5e6b4b0d3255bfef95601890afd80709        - ""
--17ba0791499db908433b80f37c5fbc89b870084b        - "11"
--6216f8a75fd5bb3d5f22b6f9958cdede3fc086c2        - "111"
--011c945f30ce2cbafc452f39840f025693339c42        - "1111"
--7b21848ac9af35be0ddb2d6b9fc3851934db8420        - "11111"      //  Total   time   98.688s  ( 15.344s elapsed)
--f888fa8a61ba9a53a45f040a4bbb8b2fc1f64444        - "ZZZZZ"      // Total   time  3273.281s  (519.332s elapsed)
