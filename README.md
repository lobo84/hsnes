hsnes
=====

A Nintendo Entertainment System emulator written in functionally pure Haskell.

Prerequisites
---
```
$ cabal install options
$ cabal install ansi-terminal
$ cabal install stream
```

Building
--------
```
$ ghc Main.hs
```

Execution
---------
```
$ ./Main
```
This will load and execute nestest.nes and compare log output with nestest.log.


Milestones
----------

**2014-10-31** - picoLAN @ tobias-johanssson
  * Completed all CPU op codes tested in nestest.nes

**2014-05-01** - West Coast Beer LAN
  * Initial PPU work and sprite viewer

**2014-02-02** - FOSDEM Red Wine and code
  * tobias-johansson and mlofjard joined the project

**2013-07-18 20:54** - Initial commit by project founder lobo84.
