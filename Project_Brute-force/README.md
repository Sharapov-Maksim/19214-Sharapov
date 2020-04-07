# **Пакеты для установки**
    cabal install <...>
* base16-bytestring
* cryptohash
* bytestring
# **Как компилировать и запускать?**
    ghc forkIO_brute-force.hs -threaded -O2
    forkIO_brute-force.exe +RTS -s -N*
флаг -s для статистики о времени исполнения;
флаги +RTS -N* для запуска на нескольких ядрах (вместо '*' нужно поставить кол-во ядер);
-N запускает на всех доступных ядрах

# **Примеры работы**
* *Хэш*: 7b21848ac9af35be0ddb2d6b9fc3851934db8420
  * *Пароль*: "11111"        
  * Total   time   35.922s  (  9.495s elapsed) - 4 ядра
* *Хэш*: f888fa8a61ba9a53a45f040a4bbb8b2fc1f64444
  * *Пароль*: "ZZZZZ"                                                             
  * Total   time  2587.281s  (350.580s elapsed) - 8 ядер
  * Total   time  1219.672s  (324.559s elapsed) - 4 ядра
