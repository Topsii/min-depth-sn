## Minimum Depth Sorting Networks

My [bachelor thesis](https://github.com/Topsii/min-depth-sn/blob/main/theses/bachelor_thesis.pdf) and my [master thesis](https://github.com/Topsii/min-depth-sn/blob/main/theses/master_thesis.pdf) are concerned with settling the depth optimization problem for sorting networks on < 32 elements.

This repository contains experimental code that implements various approaches proposed in my theses.

The code depends on an incremental Boolean SAT solver that implements the [IPASIR interface](https://github.com/biotomas/ipasir/blob/master/ipasir.h). This interface has been used in SAT competitions since 2015 and is implemented by many solvers such as [JamSAT](https://github.com/fkutzner/JamSAT) or [CryptoMiniSat](https://github.com/msoos/cryptominisat).
