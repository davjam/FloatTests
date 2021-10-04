#include <stdio.h>
#include <string.h>

//Include this to show the errors in the exiting functions:
// #include <math.h>

//Include these to test the fixes:
#include "asinh.c"
#include "asinhf.c"
#include "asinhl.c"
#include "atanh.c"
#include "atanhf.c"
#include "atanhl.c"

void printErr (char *sMsg, int ndps, double x, double y) {
  char xStr[50], yStr[50];
  sprintf(xStr, "%.*e", ndps, x);
  sprintf(yStr, "%.*e", ndps, y);
  if (strcmp(xStr,yStr) != 0)
    printf("%s: %.16e doesn't match %.16e\n",sMsg,x,y);
}

void test_asinh() {
  double x;
  x = 0.0;  //use a variable to prevent constant folding (which uses a better version of asinh).
  printErr("asinh", 8, asinh( x),  x);
  printErr("asinh", 8, asinh(-x), -x);

  for (x = -700.0; x <= 700.0; x += 10) {
    printErr("asinh(sinh(x))", 12, asinh(sinh(x)),x);
  }
  for (x = -1; x <= 1; x += 0.01) {
    printErr("asinh(sinh(x))", 12, asinh(sinh(x)),x);
  }
  for (x = -1e-300; x <= 1e-300; x += 1e-302) {
    printErr("asinh(sinh(x))", 12, asinh(sinh(x)),x);
  }
}

void test_asinhf() {
  float xf;
  xf = 0.0;
  printErr("asinhf", 8, (double)asinhf( xf),  xf);
  printErr("asinhf", 8, (double)asinhf(-xf), -xf);

  for (xf = -80.0; xf <= 80.0; xf += 1) {
    printErr("asinhf(sinhf(xf))", 9, (double)asinhf(sinhf(xf)), (double)xf);
  }
  for (xf = -1; xf <= 1; xf += 0.01) {
    //strangely float seems less accurate in the range +/- [0.5 - 1] (neither sinh nor asinh).
    printErr("asinhf(sinhf(xf))", 5, (double)asinhf(sinhf(xf)), (double)xf);
  }
  for (xf = -1e-30; xf <= 1e-30; xf += 1e-32) {
    printErr("asinhf(sinhf(xf))", 9, (double)asinhf(sinhf(xf)), (double)xf);
  }
}

void test_asinhl() {
  long double xl;
  xl = 0.0L;
  printErr("asinhl", 8, (double)asinhl( xl),  xl);
  printErr("asinhl", 8, (double)asinhl(-xl), -xl);

  for (xl = -11000.0L; xl <= 11000.0L; xl += 100.0L) {
    //sinh(-10000L) is too big for a double, but asinh(sinh(-10000L)) is fine.
    printErr("asinhl(sinhl(xl))", 15, (double)asinhl(sinhl(xl)), (double)xl);
  }
  for (xl = -1; xl <= 1; xl += 0.01) {
    printErr("asinhl(sinhl(xl))", 15, (double)asinhl(sinhl(xl)), (double)xl);
  }
  for (xl = -1e-4900L; xl <= 1e-4900L; xl += 1e-4902L) {
    printErr("asinhl(sinhl(xl))", 15, (double)asinhl(sinhl(xl)), (double)xl);
  }
}

void test_atanh() {
  double x;
  x = 0.0;
  printErr("atanh", 8, atanh( x),  x);
  printErr("atanh", 8, atanh(-x), -x);

  /* only test tanh in range within [-pi, pi] to avoid infinities */
  for (x = -1; x <= 1; x += 0.01) {
    printErr("atanh(tanh(x))", 12, atanh(tanh(x)),x);
  }
}

void test_atanhf() {
  float xf;
  xf = 0.0;
  printErr("atanhf", 8, (double)atanhf( xf),  xf);
  printErr("atanhf", 8, (double)atanhf(-xf), -xf);

  for (xf = -1; xf <= 1; xf += 0.01) {
    printErr("atanhf(tanhf(xf))", 5, (double)atanhf(tanhf(xf)), (double)xf);
  }
}

void test_atanhl() {
  long double xl;
  xl = 0.0L;
  printErr("atanhl", 8, (double)atanhl( xl),  xl);
  printErr("atanhl", 8, (double)atanhl(-xl), -xl);

  for (xl = -1; xl <= 1; xl += 0.01) {
    printErr("atanhl(tanhl(xl))", 14, (double)atanhl(tanhl(xl)), (double)xl);
  }
}

void main () {
  test_asinh();
  test_asinhf();
  test_asinhl();
  test_atanh();
  test_atanhf();
  test_atanhl();
  printf("Completed tests.\n");
}

