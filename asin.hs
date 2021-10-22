
import MyComplex
import MyFloat as F


asinOld z@(x:+y)  =  y':+(-x')
                      where  (x':+y') = log (((-y):+x) + sqrt (1 - z*z))


asinDef z = -iTimes (log (sqrt (-z*z +: 1) + iTimes z))

asinK z@(x:+y) =  atan(x/r) :+ s
  where r = realPart (sqrt(-z+:1)*sqrt(z+:1))
        --NB, conjugate(sqrt w)) /= sqrt(conjugate w) when w doesn't support -0.0: (e.g. (-1):+0).
        --(Kahan's formula sqrt(z-1)* is ambiguous, but like this works correctly for IEEE754 and not floats).
        s = F.asinh $ imagPart (conjugate(sqrt(-z+:1))*sqrt(z+:1))
