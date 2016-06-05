* Hackage: <http://hackage.haskell.org/package/sbvPlugin>
* GitHub:  <http://github.com/LeventErkok/sbvPlugin>

* Latest Hackage released version: 0.6, 2015-01-01

### Version 0.7, Not yet relesed

  * Compile with GHC-8.0. Plugin at least requires GHC-8.0.1.
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
