hsnes
=====

A Nintendo Entertainment System emulator written in functionally pure Haskell.

Prerequisites
---
* GHC - for compiling the source
* SDL - for the current PPU code (not needed to run against nestest.nes)

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

**2014-12-31** - picoLAN @ tobias-johanssson

**2014-05-01** - West Coast Beer LAN

**2014-02-02** - FOSDEM Red Wine and code

**2013-07-18 20:54** - Initial commit by project founder lobo84.
