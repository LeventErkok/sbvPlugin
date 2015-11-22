## SBVPlugin: SBV Plugin for GHC

[![Hackage version](http://img.shields.io/hackage/v/sbvPlugin.svg?label=Hackage)](http://hackage.haskell.org/package/sbvPlugin)
    [![Build Status](http://img.shields.io/travis/LeventErkok/sbvPlugin.svg?label=Build)](http://travis-ci.org/LeventErkok/sbvPlugin)

### Example

```haskell
{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module Test where

import Data.SBV.Plugin

{-# ANN test theorem #-}
test :: Integer -> Integer -> Bool
test x y = x + y >= x - y
```

*Note the GHC option on the very first line. Either decorate your file with
this option, or pass `-fplugin=Data.SBV.Plugin` as an argument to GHC, either on the command line
or via cabal. Same trick also works for GHCi.*

If we compile this program with the SBVPlugin enabled, SBV will automatically attempt to prove
this theorem, and will produce a counter-example at compile time:

```
$ ghc -c Test.hs

[SBV] Test.hs:7:1-4 Proving "test", using Z3.
[Z3] Falsifiable. Counter-example:
  x = 0 :: Integer
  y = -1 :: Integer
[SBV] Failed. (Use option 'WarnIfFails' to continue.)
```

Note that the compilation will be aborted, since the theorem doesn't hold. As shown in the hint, GHC
can be instructed to continue in that case, using an annotation of the form:

```haskell
{-# ANN test theorem {options = [WarnIfFails]} #-}
```

### Using SBVPlugin from GHCi
The plugin should work from GHCi with no changes.  Note that when run from GHCi, the plugin will
behave as if the `WarnIfFails` argument is given on all annotations, so that failures do not stop
the load process.
