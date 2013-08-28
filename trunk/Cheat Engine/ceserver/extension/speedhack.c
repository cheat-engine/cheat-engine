/*
 * speedhack.c
 *
 *  Created on: Aug 28, 2013
 *      Author: eric
 */

#include <sys/time.h>
#include <stddef.h>
#include <inttypes.h>
#include <math.h>

#include "speedhack.h"

typedef int (*GETTIMEOFDAY) (struct timeval *tv, struct timezone *tz);

GETTIMEOFDAY real_gettimeofday;

struct timeval initial_offset_tod_tv;
struct timeval initial_time_tod_tv;

float speedmultiplier;


int new_gettimeofday(struct timeval *tv, struct timezone *tz)
{
  int r;

  struct timeval currenttv;
  struct timeval temptv;

  //get the current real time
  r=real_gettimeofday(&currenttv, tz);

  //Subtract the real time with the initial time(real) and multiply that with the speed and add it to the initial time offset (fake)


  //assume that currenttv is ALWAYS bigger than initial_time (or at least equal)
  temptv.tv_sec=currenttv.tv_sec-initial_time_tod_tv.tv_sec;
  temptv.tv_usec=currenttv.tv_usec-initial_time_tod_tv.tv_usec;

  if (temptv.tv_usec<0)
  {
    temptv.tv_usec+=1000000;
    temptv.tv_sec--;
  }


  //printf("delta : sec=%ld  usec=%ld\n", temptv.tv_sec, temptv.tv_usec);

  //temptv now contains the relative time passed since the speedhack got enabled/set a new speed

  //because of the high values multiply in two steps instead of one like on windows where 64-bit is the biggest time value
  //(and not all systems seem to support uint128)

  //first multiply tv_usec with the speed

  int64_t newusec=floor((double)temptv.tv_usec*speedmultiplier);
  int64_t newsec=floor((double)temptv.tv_sec*speedmultiplier);

  //add the initial offset
  newsec+=initial_offset_tod_tv.tv_sec;
  newusec+=initial_offset_tod_tv.tv_usec;

  //sanitize the results (no negative usec, and add usec values of 1000000 to sec)
  newsec+=newusec / 1000000;  //if newsec is negative this will decrease it with the number of seconds it represents
  newusec=newusec % 1000000;

  if (newusec<0)
  {
    newusec+=1000000;
    newsec--;
  }

  if (tv)
  {
    tv->tv_sec=newsec;
    tv->tv_usec=newusec;
  }

  return r;
}

void speedhack_initializeSpeed(float speed)
{

  printf("speedhack_initializeSpeed(%f)\n", speed);
  gettimeofday(&initial_offset_tod_tv, NULL);

  if (real_gettimeofday)
    real_gettimeofday(&initial_time_tod_tv, NULL);
  else
    gettimeofday(&initial_time_tod_tv, NULL);


  speedmultiplier=speed;
}
