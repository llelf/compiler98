#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>
#include <signal.h>
#include <time.h>

#include "comp.h"
#include "runtime.h"

double profileInterval;
volatile NodePtr profileHpLimit;
volatile int timeSample;
int pactive;

#if !defined(__arm)
static int milliseconds = 0;
static int nextsampletime = 0;

/*
 * Called every RATE us...
 */

#ifdef TPROF
void timertickignore(int i)
{
#ifdef __CYGWIN32__
  signal(SIGALRM,timertickignore);	/*PH*/
#else
  signal(SIGVTALRM,timertickignore);	/*PH*/
#endif
  return;	/*PH - don't do anything on a tick*/
}
#endif

void timertick(int i)
{

  int sampleflag;
#ifdef TPROF
#ifdef __CYGWIN32__
  signal(SIGALRM,timertickignore);	/*PH*/
#else
  signal(SIGVTALRM,timertickignore);	/*PH*/
#endif
#endif
  if (timeSample == ACTIVE_TIME) { /* clock is frozen during sampling */
    WHEN_SCC(sccptr->time ++;)
    milliseconds += RATE/1000;          /* another tick (ms) */
    sampleflag = (milliseconds >= nextsampletime);
    if (sampleflag) {
/*    fprintf(stderr,"timertick %6d %6d\n",milliseconds,nextsampletime); */
#ifdef TPROF
      if (tprof)	/*PH*/
        tprofRecordTick((CodePtr*)Ip);    
      else
#endif
        profileHpLimit = 0;
      nextsampletime += 1000*profileInterval;
    }
  }
#ifdef __CYGWIN32__
  /* Cannot use virtual timers on win32, have to use real */
  signal(SIGALRM, timertick);
#else
  signal(SIGVTALRM, timertick);
#endif
  return;
}

void setuptimer(void)
{
    struct itimerval inttimer;

    inttimer.it_value.tv_sec = 0;
    inttimer.it_value.tv_usec = RATE;
    inttimer.it_interval = inttimer.it_value;
#ifdef __CYGWIN32__
    /* Cannot use virtual timers on win32, have to use real */
    signal(SIGALRM, timertick);
    setitimer(ITIMER_REAL, &inttimer, (struct itimerval *)0);
#else
    signal(SIGVTALRM, timertick);
    setitimer(ITIMER_VIRTUAL, &inttimer, (struct itimerval *)0);
#endif
    milliseconds = 0;
    nextsampletime = 1000*profileInterval;
}
void stoptimer(void)
{
    struct itimerval inttimer;

    inttimer.it_value.tv_sec = 0;
    inttimer.it_value.tv_usec = 0;
    inttimer.it_interval = inttimer.it_value;
#ifdef __CYGWIN32__
    setitimer(ITIMER_REAL, &inttimer, (struct itimerval *)0);
#else
    setitimer(ITIMER_VIRTUAL, &inttimer, (struct itimerval *)0);
#endif
}
#endif



#ifdef __arm
char *strdup(char *str)
{
  char *strc;
  int i = strlen(str)+1;
  if(0==(strc = (char *)malloc(i))) {
    fprintf(stderr,"No space to duplicate \"%s\"\n",str);
    exit(-1);
  }
  strcpy(strc,str);
  return strc;
}
#endif
