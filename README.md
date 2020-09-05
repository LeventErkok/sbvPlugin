## SBVPlugin: SBV Plugin for GHC

On Hackage: http://hackage.haskell.org/package/sbvPlugin

[![Build Status](http://img.shields.io/travis/LeventErkok/sbvPlugin.svg?label=Build)](http://travis-ci.org/LeventErkok/sbvPlugin)

### Example

```haskell
{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module Test where

import Data.SBV.Plugin

test :: Proved (Integer -> Integer -> Bool)
test x y = x + y >= x - y
```

*Note the GHC option on the very first line. Either add this to your file, or pass `-fplugin=Data.SBV.Plugin` as an
argument to GHC, either on the command line or via cabal. Same trick also works for GHCi.*

The `Proved` type simply wraps over the type of the predicate you are trying to prove, typically a function
returning a `Bool` value. It tells the plugin to treat the input as a theorem that needs to be proved.
When compiled, we get:

```
$ ghc -c Test.hs

[SBV] Test.hs:8:1-4 Proving "test", using Z3.
[Z3] Falsifiable. Counter-example:
  x =  0 :: Integer
  y = -1 :: Integer
[SBV] Failed. (Use option 'IgnoreFailure' to continue.)
```

Note that the compilation will be aborted, since the theorem doesn't hold. If you load this file in GHCi, it will simply
fail and drop you back to the GHCi prompt.

### Annotation style
While the `Proved` type should suffice for simple uses, the plugin takes a number of arguments to modify
options and pick underlying solvers. In this case, an explicit annotation can be provided:

```haskell
{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module Test where

import Data.SBV.Plugin

{-# ANN test theorem {options = [IgnoreFailure]} #-}
test :: Integer -> Integer -> Bool
test x y = x == y -- clearly not True!
```

The above, for instance, tells the plugin to ignore failed proofs (`IgnoreFailure`). This is useful when you
have a failing theorem that you are still working on, to make sure GHC continues compilation instead of stopping
compilation and erroring out at that point.

### Available options

The plugin currently understands the following options. Multiple options can be given at the same time 
by comma separating them.

```haskell
data SBVOption =
   IgnoreFailure  -- ^ Continue even if proof fails
 | Skip String    -- ^ Skip the proof. Can be handy for properties that we currently do not want to focus on.
 | Verbose        -- ^ Produce verbose output, good for debugging
 | Debug          -- ^ Produce really verbose output, use only when things go really wrong!
 | QuickCheck     -- ^ Perform quickCheck
 | Uninterpret    -- ^ Uninterpret this binding for proof purposes
 | Names [String] -- ^ Use these names for the arguments; need not be exhaustive
 | ListSize Int   -- ^ If a list-input is found, use this length. If not specified, we will complain!
 | Z3             -- ^ Use Z3
 | Yices          -- ^ Use Yices
 | Boolector      -- ^ Use Boolector
 | CVC4           -- ^ Use CVC4
 | MathSAT        -- ^ Use MathSAT
 | ABC            -- ^ Use ABC
 | AnySolver      -- ^ Run all installed solvers in parallel, and report the result from the first to finish
```

### Using SBVPlugin from GHCi
The plugin should work from GHCi with no changes.  Note that when run from GHCi, the plugin will
behave as if the `IgnoreFailure` argument is given on all annotations, so that failures do not stop
the load process.

### Thanks
The following people reported bugs, provided comments/feedback, or contributed to the development of SBVPlugin in
various ways: Nickolas Fotopoulos and Stephan Renatus.
