// math.h
// UnderC Pocket C++ Library, Steve Donovan 2001-2003
// UC already has as builtins:
// sin cos tan atan2 pow exp log sqrt atof
#ifndef __MATH_H
#define __MATH_H
#ifdef __win32__
#lib msvcrt40.dll
#else
#lib libm.so
#endif
extern "C" {
/* From Appendix B4 of K&R2 */
// double sin(double);
//double cos(double);
//double tan(double);
  double asin(double);
  double acos(double);
  double atan(double);
//double atan2(double, double);
  double sinh(double);
  double cosh(double);
  double tanh(double);
//double exp(double);
//double log(double);
  double log10(double);
//double pow(double, double);
//double sqrt(double);
  double ceil(double);
  double floor(double);
#ifndef __FreeBSD__
 /* these are in libc */
  double fabs(double);
  double ldexp(double, int);
  double frexp(double, int *);
#endif
  //* double mod(double, double *); *not in Linux?*
  double fmod(double, double);
}
#lib
#ifdef __FreeBSD__
#lib libc.so
extern "C" {
  double fabs(double);
  double ldexp(double, int);
  double frexp(double, int *);
}
#lib
#endif
#endif
