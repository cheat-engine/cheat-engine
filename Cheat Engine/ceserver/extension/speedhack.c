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
#include <stdio.h>
#include <time.h>

#include "speedhack.h"

typedef int (*GETTIMEOFDAY) (struct timeval *tv, struct timezone *tz);
typedef int (*CLOCK_GETTIME) (clockid_t clk_id, struct timespec *tp);

GETTIMEOFDAY real_gettimeofday;
CLOCK_GETTIME real_clock_gettime;



float speedmultiplier;

struct {
  int result;
  struct timespec initialoffset;
  struct timespec initialtime;
} initialclock[10];

struct timeval initial_offset_tod_tv;
struct timeval initial_time_tod_tv;

int new_clock_gettime(clockid_t clk_id, struct timespec *tp)
{
  int r=-1;
  struct timespec currenttp;



  r=real_clock_gettime(clk_id, &currenttp);

  //printf("clock_gettime\n");


  if ((clk_id<=9) && (initialclock[clk_id].result==0))
  {
    struct timespec temptp;

    temptp.tv_sec=currenttp.tv_sec-initialclock[clk_id].initialtime.tv_sec;
    temptp.tv_nsec=currenttp.tv_nsec-initialclock[clk_id].initialtime.tv_nsec;

    if (temptp.tv_nsec<0)
    {
      temptp.tv_nsec+=1000000000;
      temptp.tv_sec--;
    }

    //printf("delta : sec=%ld  nsec=%ld\n", temptp.tv_sec, temptp.tv_nsec);

    double newsec_double = (double)temptp.tv_sec*speedmultiplier;

    int64_t newnsec=floor((double)temptp.tv_nsec*speedmultiplier);
    int64_t newsec=floor(newsec_double);

    newnsec+=floor((newsec_double - floor(newsec_double)) * 1000000000.0f);

    //printf("newsec=%ld  newnsec=%ld\n", newsec, newnsec);

    //add the initial offset
    newsec+=initialclock[clk_id].initialoffset.tv_sec;
    newnsec+=initialclock[clk_id].initialoffset.tv_nsec;


    //sanitize the results (no negative usec, and add usec values of 1000000 to sec)
    newsec+=newnsec / 1000000000;  //if newsec is negative this will decrease it with the number of seconds it represents
    newnsec=newnsec % 1000000000;


    if (newnsec<0)
    {
      newnsec+=1000000000;
      newsec--;
    }

    if (tp)
    {
      tp->tv_sec=newsec;
      tp->tv_nsec=newnsec;
    }


  }
  else
  {
    if (tp)
    {
      *tp=currenttp;
    }
  }

  return r;
}

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

  double newsec_double = (double)temptv.tv_sec*speedmultiplier;

  int64_t newusec=floor((double)temptv.tv_usec*speedmultiplier);
  int64_t newsec=floor(newsec_double);

  //convert the decimal part of sec to usec

  newusec+=floor((newsec_double - floor(newsec_double)) * 1000000.0f);



  //printf("newsec=%ld  newusec=%ld\n", newsec, newusec);

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

int speedhack_initializeSpeed(float speed)
{

 // printf("speedhack_initializeSpeed(%f)\n", speed);
  gettimeofday(&initial_offset_tod_tv, NULL);

  if (real_gettimeofday)
    real_gettimeofday(&initial_time_tod_tv, NULL);
  else
    gettimeofday(&initial_time_tod_tv, NULL);


  int i;
  for (i=0; i<=9; i++)
  {
    clock_gettime(i, &initialclock[i].initialoffset);

    if (real_clock_gettime)
      initialclock[i].result=real_clock_gettime(i, &initialclock[i].initialtime);
    else
      initialclock[i].result=clock_gettime(i, &initialclock[i].initialtime);

  }



  speedmultiplier=speed;

  return 1;
}
