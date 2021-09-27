/*
These are the suggested fixes to
https://github.com/mirror/mingw-w64/blob/master/mingw-w64-crt/math/x86/asinh.c and atanh.c.
I've also included a copy of https://github.com/mirror/mingw-w64/blob/master/mingw-w64-crt/math/x86/fastmath.h
to check it compiles.
*/

#include <math.h>
#include <errno.h>
#include "fastmath.h"

double asinh(double x)
{
  double z;
  if (!isfinite (x))
    return x;
  z = fabs (x);

  /* Avoid setting FPU underflow exception flag in x * x. */
#if 0
  if ( z < 0x1p-32)
    return x;
#endif

  /* Use log1p to avoid cancellation with small x. Put
     x * x in denom, so overflow is harmless. 
     asinh(x) = log   (x + sqrt (x * x + 1.0))
              = log1p (x + sqrt (x * x + 1.0) - 1.0)
              = log1p (x + (sqrt (x * x + 1.0) - 1.0) * (sqrt (x * x + 1.0) + 1.0) / (sqrt (x * x + 1.0) + 1.0))
              = log1p (x + ((x * x + 1.0) - 1.0) / (sqrt (x * x + 1.0) + 1.0))
              = log1p (x + x * x / (sqrt (x * x + 1.0) + 1.0))  */

  if (z >= 9.490626562425156e7)  // FIXME: Check this doesn't give discontinuity. Replace by formula? (e.g. sqrt(floatSuccLim)).
    /* above this, z*z+1 == z*z, so we can simplify (and avoid z*z being infinity).
            log1p (z + sqrt (z * z + 1.0) - 1.0)
        =   log1p (z + sqrt (z * z      ) - 1.0)
        =   log1p (z +       z            - 1.0)
        =   log   (z +       z                 )
        =   log   (2 *       z                 )
        =   log    2 + log   z
    */
    z = __fast_log(2) + __fast_log(z);
  else
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
