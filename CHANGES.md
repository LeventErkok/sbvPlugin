* Hackage: <http://hackage.haskell.org/package/sbvPlugin>
* GitHub:  <http://github.com/LeventErkok/sbvPlugin>

* Latest Hackage released version: 9.6.1, 2023-04-14

### Version 9.6.1, 2023-04-14
  * Changes required to compile with GHC 9.6.1
  * Bump up sbv dependence to >= 10.1

### Version 9.4.4, 2023-01-16
  * Changes required to compile with GHC 9.4.4
  * Bump up sbv dependence to >= 9.2

### Version 9.2.2, 2022-04-27
  * Changes required to compile with GHC 9.2.2
  * Bump up sbv dependence to >= 9.0

### Version 9.0.1, 2021-03-22
  * Changes required to compile with GHC 9.0.1
  * SBVPlugin version now matches the version of GHC we compiled it with.
    It might work with newer versions of GHC, though not tested/guaranteed.
  * Bump up sbv dependence to >= 8.13

### Version 0.12, 2020-09-05
  * Changes required to compile with GHC 8.10.2
  * Bump up sbv dependence to >= 8.8

### Version 0.11, 2019-01-14

  * Changes required to compile with GHC 8.6.3
  * Bump up sbv dependence to >= 8.0
  * Clean-up/improve test cases

### Version 0.10, 2017-07-29

  * Changes required to compile with GHC 8.2.1/8.2.2.
  * Bump up sbv dependence to >= 7.4

### Version 0.9, 2017-07-19

  * Sync-up with recent modifications to SBV. No user visible changes.
  * Bump up sbv dependence to >= 7.0

### Version 0.8, 2017-01-12

  * Fix broken links, thanks to Stephan Renatus for the patch.
  * Add the 'Proved' type, which allows for easily tagging a type for proof,
    without the need for an explicit annotation. Thanks to Nickolas Fotopoulos
    for the patch.
  * Bump up sbv dependence to > 5.14
  
### Version 0.7, 2016-06-06

  * Compile with GHC-8.0. Plugin at least requires GHC-8.0.1 and SBV 5.12
  * Fix a few dead links

### Version 0.6, 2016-01-01

  * Support for list expressions of the form [x .. y] and
    [x, y .. z]; so long as the x, y, and z are all concrete.
  * Simplify some of the expressions in BitTricks using
    the new list-construction support.
  * Added more proofs to the BitTricks example

### Version 0.5, 2015-12-26
 
  * Allow higher-order (i.e., function) arguments to theorems.
  * Rework uninterpreted functions, generalize types
  * Simplify cabal file; no need to ship gold-files for tests
  * Add merge-sort example "Data/SBV/Plugin/Examples/MergeSort.hs"
  * Add bit-tricks example "Data/SBV/Plugin/Examples/BitTricks.hs"

### Version 0.4, 2015-12-24

  * Support for case-alternatives producing lists/tuples
    and functions. In the list case, we require that both
    alternatives produce equal-length lists, as otherwise
    there is no way to merge the two results.
  * More test cases.

### Version 0.3, 2015-12-21
  
  * Added the micro-controller example, adapted from
    the original SBV variant by Anthony Cowley:
    <http://acowley.github.io/NYHUG/FunctionalRoboticist.pdf>
  * Add the "skip" option for the plugin itself. Handy when
    compiling the plugin itself!

### Version 0.2, 2015-12-21

  * Further fleshing of internals
  * Support for case-expressions
  * Support for uninterpreted types/functions
  * Lots of test cases, refactoring.

### Version 0.1, 2015-12-06

  * Basic functionality. Initial design exploration.
  * The plugin functional on base values, but there
    are a lot of rough edges around the details.
    Please report any issues you might find!
