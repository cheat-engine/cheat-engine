// UnderC Development Project, 2001
#ifndef __TIME_H
#define __TIME_H

// no. of seconds since midnight Jan 1, 1970
#ifndef _TIME_T_DEFINED
typedef long time_t;             /* time value */
typedef long clock_t;
typedef unsigned long _fsize_t;
#define CLOCKS_PER_SEC 1000000
#define _TIME_T_DEFINED          /* avoid multiple def's of time_t */
#endif

// depends on C mode! This hack relies on the fact that C mode defines delete as a macro
#ifndef delete
# define _STRUCT
#else
# define _STRUCT struct
#endif

struct tm {
        int tm_sec;     /* seconds after the minute - [0,59] */
        int tm_min;     /* minutes after the hour - [0,59] */
        int tm_hour;    /* hours since midnight - [0,23] */
        int tm_mday;    /* day of the month - [1,31] */
        int tm_mon;     /* months since January - [0,11] */
        int tm_year;    /* years since 1900 */
        int tm_wday;    /* days since Sunday - [0,6] */
        int tm_yday;    /* days since January 1 - [0,365] */
        int tm_isdst;   /* daylight savings time flag */
  };

#include <_shared_lib.h>
extern "C" {
 time_t     clock();
 time_t     time(time_t *);
 char *     ctime(time_t *);
 _STRUCT tm* localtime(time_t *);
 _STRUCT tm* gmtime(time_t *);
 char *asctime( const _STRUCT tm *timeptr );
 int strftime( char *strDest, int maxsize, const char *format, const _STRUCT tm *timeptr );
}
#lib

#endif
