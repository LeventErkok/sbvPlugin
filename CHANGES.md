* Hackage: <http://hackage.haskell.org/package/sbvPlugin>
* GitHub:  <http://github.com/LeventErkok/sbvPlugin>

* Latest Hackage released version: 0.4, 2015-12-24

### Version 0.5, Not yet released

  * Simplify cabal file; no need to ship gold-files
    for tests

### Version 0.4, 2015-12-24

  * Support for case-alternatives producing lists/tuples
    and functions. In the list case, we require that both
    alternatives produce equal-length lists, as otherwise
    there is no way to merge the two results.
  * More test cases.

### Version 0.3, 2015-12-21
  
  * Added the micro-controller example, adapted from
    the original SBV variant by Anthony Cowley
    (http://acowley.github.io/NYHUG/FunctionalRoboticist.pdf)
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
