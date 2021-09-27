# HaskellNumericsTestsFixes
Whilst trying to understand numbers in Haskell(*), I discovered some issues:
- With real numbers in Windows, e.g.: `asinh 0` gives `-0.0` (should be `0.0`); `asinh 1e300` gives `NaN` (sb ~ `691.5`).
  These are due to defects in the underlying mingw-w64 library used by Haskell in Windows
- With complex number, esp branch cuts, e.g.: `sqrt $ (-4) :+ (-0)` gives `0.0 :+ 2.0` (sb `0.0 :+ (-2.0)`).
  These are due to defects in the Haskell functions.

This repo is work-in-progress, and has code to test and fix these problems.
I'm trying to develop a more complete set of tests that can become part of the Haskell CI test suite.
I intend to integrate the fixes back into core Haskell. (With some help!)

It also has illustrations of the complex functions [as they are](https://davjam.github.io//HaskellNumericsTestsFixes/TrigDiags/Curr.html)
and [as they should be](https://davjam.github.io//HaskellNumericsTestsFixes/TrigDiags/Fixed.html) when fixed.
These are based on the diagrams in "Common Lisp The Language (2nd Edition)" by Guy Steele Jr, wih some modifications:
- The diagrams in the book are based on floating point numbers that don't support negative zeros. Haskell does, so shows separate lines for +0.0 (in solid) and -0.0 (dashed).
- The (initially) straight lines are coloured.

The diagrams are explained well in the book, but briefly:
Each diagram shows a decorated complex plane with axes that have markers at +/- 1, 2, 3 & 4 and slightly smaller markers at pi/2 and pi.
The "id" graph shows an initial decoration of lines and rings.
The other graphs show how these same decorations are mapped under the different functions.

An illustration of the incorrect branch cuts with `sqrt`, for example, become quite visible:
- With the current functions, the branch cut is (correctly) along the negative real axis, but (incorrectly) between imaginary -0 and imaginary < -0.
  Hence the solid and dashed orange lines end up on the positive imaginary axis, leaving an "open edge" on left of quadrant IV.
- With the fixed function, the branch cut is between imaginary 0 and imaginary -0.
  Hence the dashed orange line is mapped to the negative imaginary axis neatly closes off the left edge of quadrant IV.

Similar "open edges" can be seen in the other functions, and these all get closed with the new code.
An additional issue is visible in e.g. `asin`, where the mapped lines along the negative real axis wobble between 0 and -0.
Again, this is fixed in the new functions (with the solid and dashed lines being straight).
(However, I'm not sure they're yet quite in the right places, so more work is needed to validate/correct.)

The main files in the repo are:
- MyFloatC.c: the suggested fixes for mingw-w64 real number issues.
- MyFloat.hs: a reimplementation of the above in Haskell code.
- RealFloatTests.hs: tests for the real number functions.
- MyComplex.hs: the suggested fixes for Haskell complex number issues.
- ComplexTests.hs: tests for the complex functions.
- TrigDiags.hs: a reimplementation of the code from the Common Lisp book that generates the diagrams. (It depends on the blaze-html and blaze-svg packages).

(*): As part of trying to understand, I thought I'd write a wiki page on them
(the draft is [here](https://en.wikibooks.org/wiki/User:Davjam2/Numbers),
and it still needs quite a lot of work).


