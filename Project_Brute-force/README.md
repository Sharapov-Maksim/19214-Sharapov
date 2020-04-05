# **Пакеты для установки**
    cabal install <...>
* base16-bytestring
* cryptohash
* bytestring
# **Как компилировать и запускать?**
    ghc forkIO_brute-force.hs -threaded -O2
    forkIO_brute-force.exe +RTS -s -N*
флаги +RTS -s для статистики о времени исполнения
флаг -N* для запуска на нескольких ядрах (вместо '*' нужно поставить кол-во ядер)
-N запускает на всех доступных ядрах
