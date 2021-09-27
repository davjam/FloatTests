/*
These are the suggested fixes to
https://github.com/mirror/mingw-w64/blob/master/mingw-w64-crt/math/x86/asinh.c and atanh.c.
I've also included a copy of https://github.com/mirror/mingw-w64/blob/master/mingw-w64-crt/math/x86/fastmath.h
to check it compiles.

The same changes are needed for float, double, long double (anything else) versions?
*/

#include <math.h>
#include <float.h>
#include <errno.h>
#include "fastmath.h"

double asinh(double x)
{
  /* mathematically:
     asinh(x) = log (x + sqrt (x * x + 1.0))
  */

  double z;
  if (!isfinite (x))
    return x;
  z = fabs (x);

  /* Avoid setting FPU underflow exception flag in x * x. */
#if 0
  if ( z < 0x1p-32)
    return x;
#endif

  const double asinhCutover = pow(2,DBL_MAX_EXP/2); // 1.3407807929943e+154

  if (z >= asinhCutover)
  /* above this, z*z+1 == z*z, so we can simplify
     (and avoid z*z being infinity).
      asinh(z) = log (z + sqrt (z * z + 1.0))
               = log (z + sqrt (z * z      ))
               = log (2 * z)
               = log 2 + log z

      Choosing asinhCutover is a little tricky.
      We'd like something that's based on the nature of
      the numeric type (DBL_MAX_EXP, etc).
      If x = asinhCutover, then we need:
         (1) x*x == x*x + 1
         (2) x*x < Infinity
         (3) log (2*x) = log 2 + log x.
      For float:
         9.490626562425156e7 is the smallest value that
         achieves (1), but it fails (3). (It only just fails,
         but enough to make the function erroneously non-monotonic).
         1.3407807929942596e154 is the largest value
         that achieves (2).
    */
    z = __fast_log(2) + __fast_log(z);
  else
  /* Use log1p to avoid cancellation with small x. Put
     x * x in denom, so overflow is harmless. 
     asinh(z) = log   (z + sqrt (z * z + 1.0))
              = log1p (z + sqrt (z * z + 1.0) - 1.0)
              = log1p (z + (sqrt (z * z + 1.0) - 1.0)
                         * (sqrt (z * z + 1.0) + 1.0)
                         / (sqrt (z * z + 1.0) + 1.0))
              = log1p (z + ((z * z + 1.0) - 1.0)
                         / (sqrt (z * z + 1.0) + 1.0))
              = log1p (z + z * z / (sqrt (z * z + 1.0) + 1.0))
     */
    z = __fast_log1p (z + z * z / (__fast_sqrt (z * z + 1.0) + 1.0));

  return copysign(z, x);
}

double atanh(double x)
{
  double z;
  if (isnan (x))
    return x;
  z = fabs (x);
  if (z == 1.0)
    {
      errno  = ERANGE;
      return (x > 0 ? INFINITY : -INFINITY);
    }
  if (z > 1.0)
    {
      errno = EDOM;
      return nan("");
    }
  /* Rearrange formula to avoid precision loss for small x.
  atanh(x) = 0.5 * log ((1.0 + x)/(1.0 - x))
	   = 0.5 * log1p ((1.0 + x)/(1.0 - x) - 1.0)
           = 0.5 * log1p ((1.0 + x - 1.0 + x) /(1.0 - x)) 
           = 0.5 * log1p ((2.0 * x ) / (1.0 - x))  */
  z = 0.5 * __fast_log1p ((z + z) / (1.0 - z));
  return copysign(z, x);
}
