#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <setjmp.h>
#include "comp.h"

#include "node.h"
#include "mutlib.h"
/* #include "runtime.h" -- already included in node.h */

#ifdef DBGTRANS
extern Node C0_DPrelude_46SR[];
extern Node C0_DPrelude_46Root[];
extern Node FN_DPrelude_46ap1[];
extern Node FN_Prelude_46_95startDbg[];
#endif

#define ARGSIZE 200	/* Obsolete?  Used to limit number of cmdline args. */

/* flags */

#ifdef DBGTRANS
int traceIp,traceSp,traceHp,traceFlag;
int traceDepth = 1;
int traceGcStat = 0;
FILE *traceGcFd = NULL;
FILE *HatFile;
#endif
#if INSCOUNT
int insCount;
#endif

int bellGc;
int gcStatics;
int dumpStack;
timer gcTime,totalTime,runTime;

int traceStat = 0;
int traceShow = 0;
int traceNoR = 0;
int traceNoSat = 0;
int traceNoSatF = 0;
int traceQuit = 0;
int tracePruneSATs = 0;
int traceAdaptablePruning = 0;
int traceK = -1;
int traceBreak = 0;

/****/

#define NO_UNIT 0
#define SIZE_UNIT 1
#define TIME_UNIT 2

#define assign(var,exp) (var>exp ? var : (var=exp))

double numArg(int unit, char *s)
{
  double prefix = 1;
  double i = 0;
  while(isdigit(*s)) {
    /* i = i*10 + *s++ - '0'; */
    assign(i,i*10 + *s - '0');
    s++;
  }
  if(*s=='.') {
    double pos = 0.1;
    while(isdigit(*++s)) {
      i = i + (double)(*s - '0')*pos;
      pos /= 10.0;
    }
  }

  switch(*s) {
  case 'k': 
  case 'K': prefix *= 1000;     s++; break;
  case 'm': prefix *= -1000000; s++; break;  /* ugly hack to distinguis milli from Mega if followed by s */
  case 'M': prefix *=  1000000; s++; break;
  }

  if(TIME_UNIT & unit && *s == 's') { 
    s++;
    if(prefix < 0 ) prefix = 0.001;
    i = -i*prefix;
  } else if (SIZE_UNIT & unit) {
    if(prefix<0) prefix = -prefix;
    switch(*s) {
    case 'b': case 'B': s++; i = assign(i,i*prefix)/sizeof(Node); break;
    case 'w': case 'W': s++; assign(i,i*prefix); break;
    default: i = assign(i,i*prefix)/sizeof(Node); break;
    }
  }
      
  if(*s) 
    fprintf(stderr,"Ignoring extra character(s) '%s' at end of number\n",s);
  
  return i;
}

#ifdef PROFILE
void getMaxSet(char *maxs)
{
  extern int maxSet;
  if(*maxs)
    maxSet = atoi(maxs);
  else
    maxSet = 1;
}
#endif

int exit_code;

#ifdef __CYGWIN32__
jmp_buf exit_mutator;
#else
sigjmp_buf exit_mutator;
#endif



char **Argv;
int Argc;

void haskellInit (int argc, char **argv)
{
  int i;

#if 0
  if(argc>=ARGSIZE) {
    fprintf(stderr,"Sorry temporary limit of max %d arguments\n",ARGSIZE);
    exit(-1);
  }
#endif
  
/*Argv = (char **)malloc(ARGSIZE*sizeof(char *));*/
  Argv = (char **)malloc(argc*sizeof(char *));
  if(!Argv) {
    fprintf(stderr,"Out of memory when reserving space for %d arguments\n",argc);
    exit(-1);
  }

  Argv[0] = argv[0];
  for(Argc = i = 1; i < argc; i++) {
    if (!strcmp(argv[i],"+RTS")) {
      i++;
      for(; i < argc; i++) {
	if(!strcmp(argv[i],"-RTS")) {
	  break;
	} else if(argv[i][0] != '-') {
	  fprintf(stderr,"Warning: unknown runtime argument %s ignored.\n",argv[i]);
	} else {
	  switch(argv[i][1]) {
	  case 's':
	    gcStatics ++; break;
	  case 'B':
	    bellGc ++; break;
	  case 'H':
	    if(argv[i][2]) hpSize = (Int)numArg(SIZE_UNIT,&argv[i][2]); break;
	  case 'V':
	    if(argv[i][2]) spSize = (Int)numArg(SIZE_UNIT,&argv[i][2]); break;

	  case 'd':
#ifdef DBGTRANS
	    if (strcmp(&argv[i][2], "s") == 0) {
		if (i < argc) {
		    char *p;
		    extern void suspectModule(char *, char *, int);
		    i++;
		    if ((p = strchr(argv[i], '.')) != NULL) {
		        p[0] = '\0';
		        suspectModule(argv[i], &p[1], 0);
		    } else {
		        suspectModule(argv[i], NULL, 0);
		    }
		}
	    } else if (strcmp(&argv[i][2], "t") == 0) {
		if (i < argc) {
		    char *p;
		    extern void trustModule(char *, char *, int);
		    i++;
		    if ((p = strchr(argv[i], '.')) != NULL) {
		        p[0] = '\0';
		        trustModule(argv[i], &p[1], 0);
		    } else {
		        trustModule(argv[i], NULL, 0);
		    }
		}
	    } else if (strcmp(&argv[i][2], "sr") == 0) {
		if (i < argc) {
		    extern void suspectModule(char *, char *, int);
		    i++;
		    suspectModule(argv[i], NULL, 1);
		}
	    } else if (strcmp(&argv[i][2], "tr") == 0) {
		if (i < argc) {
		    extern void trustModule(char *, char *, int);
		    i++;
		    trustModule(argv[i], NULL, 1);
		}
	    } else if (strcmp(&argv[i][2], "show") == 0)
		traceShow ++;
	    else if (strcmp(&argv[i][2], "nr") == 0)
		traceNoR++;
	    else if (strcmp(&argv[i][2], "nsatf") == 0)
		traceNoSatF++;
	    else if (strcmp(&argv[i][2], "nsat") == 0)
		traceNoSat++;
	    else if (strcmp(&argv[i][2], "q") == 0)
		traceQuit++;
	    else if (strcmp(&argv[i][2], "ps") == 0)
		tracePruneSATs++;
	    else if (argv[i][2] == 'b') {
                if (argv[i][3]) {
                    traceBreak = (Int)numArg(NO_UNIT,&argv[i][3]);
                    fprintf(stderr, "traceBreak = %d\n", traceBreak);
                }
	    } else if (strcmp(&argv[i][2], "stat") == 0)
		traceStat++;
	    else if (argv[i][2] == 'K')
		traceAdaptablePruning++;
	    else if (argv[i][2] == 'k') {
		if (argv[i][3]) 
		    traceK = (Int)numArg(NO_UNIT,&argv[i][3]);
		else
		    traceK = -1;
	    } else if (argv[i][2] == 'g') {
               if (argv[i][3]) 
                   traceGcStat = (Int)numArg(NO_UNIT,&argv[i][3]);
               if (++i >= argc) {
                   fprintf(stderr, "-dg requires a filename argument\n");
                   exit(-1);
               }
               if (strcmp(argv[i], "-") == 0)
                   traceGcFd = stdout;
               else if ((traceGcFd = fopen(argv[i], "w")) == NULL) {
                   fprintf(stderr, "-dg: Cannot open log file for writing: %s\n", argv[i]);
                   exit(-1);
               }
               /* After zero reductions the live heap size is ... zero ;-) */
               fprintf(traceGcFd, "0 0\n");
	    }
#else
            fprintf(stderr, "Program has not been compiled for tracing - ignoring -d[option]\n");
#endif
	    break;
#if defined(PROFILE) || defined(TPROF)
	  case 'i':
	    if(argv[i][2]) profileInterval = numArg(SIZE_UNIT|TIME_UNIT,&argv[i][2]);
	    else { fprintf(stderr,
			   "-i must be followed by number of words or time in seconds between profile sample!\n");
		   exit(-1);
		 }
	    /*	     fprintf(stderr,"profileInterval = %g\n",profileInterval); */
	    if(profileInterval < 0) {
	      profileInterval = -profileInterval;
	      timeSample = 1;
	    } else
	      timeSample = 0;
	    break;
#endif
#if PROFILE
	  case 'p':
	    if(!profile)  {profile = PROFILE_PRODUCER; WHEN_DYNAMIC(getMaxSet(&argv[i][2]);)}
	    else if(argv[i][2]) addRestrictions(&argv[i][2],PROFILE_PRODUCER);
	    break;
	  case 'c':
	    if(!profile)  {profile = PROFILE_CONSTRUCTOR; WHEN_DYNAMIC(getMaxSet(&argv[i][2]);)}
	    else if(argv[i][2]) addRestrictions(&argv[i][2],PROFILE_CONSTRUCTOR);
	    break;
	  case 'm':
	    if(!profile)   {profile = PROFILE_MODULE; WHEN_DYNAMIC(getMaxSet(&argv[i][2]);)}
	    else if(argv[i][2]) addRestrictions(&argv[i][2],PROFILE_MODULE);
	    break;
	  case 'r':
	    if(!profile) {profile = PROFILE_RETAINER; getMaxSet(&argv[i][2]);}
	    else if(argv[i][2]) addRestrictions(&argv[i][2],PROFILE_RETAINER);
	    break;
	  case 'b':
	    if(!profile){profile = PROFILE_BIOGRAPHY; getMaxSet(&argv[i][2]);}
	    else addRestrictions(&argv[i][2],PROFILE_BIOGRAPHY);
	    break;
	  case 'l':
	    if(!profile){profile = PROFILE_LIFETIME; getMaxSet(&argv[i][2]);}
	    else addRestrictions(&argv[i][2],PROFILE_LIFETIME);
	    break;
	  case '1':
	    if(!profile) {
	      fprintf(stderr,"First run\n");
	      profile = PROFILE_FIRST;
	    } else {
	      fprintf(stderr,"The '-f' can only be used on it's own\n");
	    }
	  case '2':
	    second_run = 1;
	    break;
	    
	    
	  case '@':
	    countAp = 1; break;
	    
	  case 'u':
	    if(!profile){
	      fprintf(stderr,"It only possible to use -u as modifier of other profile.");
	      exit(-1);
	    } else {
	      PrintUse = 1;
	    }
	    break;
#endif
          case 't':          /*PH*/
#ifdef TPROF
#ifdef PROFILE
            if (!profile) { /* -t  List will be sorted by module,ticks,enters */
              tprof = 1;    /* -tt List will be sorted by ticks,enters        */
            }               /* -te List will be sorted by enters,ticks        */
#else                       /* -tp List will include enter percentages        */
            tprof = 1;      /* -tep by tick,enters including enter %  etc...  */
#endif                                  /* Args following -t in quotes give   */
            if (argv[i][2]) {           /* Module names that the user wishes  */
              tprofInclude(argv[i]+2);  /* to collapse/expand eg -t"-Ph +IO"  */
                                        /* will collapse Ph and expand IO     */
            }                           /* "+all" and "-all" are valid        */
            break;                      /* default: "-Prelude -System -IO"    */
#else
            fprintf(stderr, "Program has not been compiled for time profiling - ignoring -t[option]\n");
            break;
#endif

#if INSCOUNT
	  case 'I':
	    insCount++;
            break;
#endif

	  case 'X':   /* Only useful if linked with -X */
	    xlib_debug ++;
	    break;
#ifdef DBGTRANS
	  case 'D':
	    if(argv[i][2]) dumpStack = (Int)numArg(NO_UNIT,&argv[i][2]);
	    else dumpStack = 1;
            break;
	  case 'T':
	    if(argv[i][2] != 0) {
	      int ii = 2;
	      while(argv[i][ii] != 0) {
		switch(argv[i][ii]) {
		case 'r' : traceFlag |= TRACE_RETURN; break;
		case 'e' : traceFlag |= TRACE_EVAL; break;
		case 'b' : traceIp ++; break;
		case 's' : traceSp ++; break;
		case 'h' : traceHp ++; break;
		case 'a' : traceFlag |= DUMP_ADDR; break;
		case 'i' : traceFlag |= DUMP_IND; break;
		case 't' : traceFlag |= DUMP_TOP; break;
		default:
		  if(isdigit(argv[i][ii])) {
		    traceDepth = 0;
		    while(isdigit(argv[i][ii])) {
		      traceDepth = traceDepth*10 + argv[i][ii++] - '0';
		    }
		    ii--;
		  } else {
		    fprintf(stderr,"Warning unrecognized trace flag %c ignored.\n",argv[i][ii]);
		  }
		}
		ii++;
	      }
	    } else {
	       traceIp++; traceSp++; traceHp++;
	       traceFlag = DUMP_ADDR | DUMP_IND | DUMP_TOP;
	     } break;
#endif
	  default:
	    fprintf(stderr,"Warning unrecognized run-time flag %s ignored.\n",argv[i]);
	  }
	}
      }
    } else {
      Argv[Argc] = argv[i];
      Argc++;
    } 
  }

#ifdef DBGTRANS
  {
    fpos_t p;
    char filename[256];
    extern void dumpNewModInfo(FILE*, void*);
    extern void *MODULE_Main, NMOD_Prelude;
    strcpy(filename,argv[0]);
    strcat(filename,".hat");
    HatFile = fopen(filename,"w");
    fgetpos(HatFile,&p);
    fprintf(HatFile,"Hat v01");		/* initialise file */
    fputc(0,HatFile);
    fwrite(&p,sizeof(fpos_t),1,HatFile);
    fwrite(&p,sizeof(fpos_t),1,HatFile);
  /*dumpNewModInfo(HatFile,MODULE_Main); */
  /*dumpNewModInfo(HatFile,&NMOD_Prelude); */
    fflush(HatFile);
  }
#endif

  initForeignObjs();
  initGc(hpSize,&Hp,spSize,&Sp);
  stableInit();  /*MW*/

  timerClear(&totalTime);
  timerClear(&runTime);
  timerClear(&gcTime);
  timerStart(&totalTime);

#ifdef PROFILE
  if(profile) profile_start(argc,argv);
#endif
#ifdef TPROF
  if(tprof) tprofStart(argc,argv);	/*PH*/
#endif

  timerStart(&runTime);

  Fp = Sp;	/* initialise the Frame pointer */
} /* end of haskellInit */





int haskellEnd (int argc, char **argv) {
  timerStop(&runTime);
#ifdef TPROF
  if(tprof) tprofStop();	/*PH*/
#endif
#ifdef PROFILE
  if(profile) profile_stop(Hp);
#endif

  /* runDeferredGCs();		/* need to run finalisers (?) */

  timerStop(&totalTime);
  fflush(stdout);
  fflush(stderr);
  finishGc(Hp,bellGc > 2);
  if(bellGc > 2) {
    double tt = (double)totalTime.l/(double)HZ;
    if(totalTime.h)
      fprintf(stderr,"32 bit timer not enough! Total time wrapped around %d times.\n",totalTime.h);
    fprintf(stderr,"Total time = %7.2f\n",tt);
    if(gcTime.h)
      fprintf(stderr,"32 bit timer not enough! Gc time wrapped around %d times.\n",gcTime.h);
    fprintf(stderr,"Gc time    = %7.2f\n",(double)gcTime.l/(double)HZ);
    if(runTime.h)
      fprintf(stderr,"32 bit timer not enough! Run time wrapped around %d times.\n",runTime.h);
    fprintf(stderr,"Run time   = %7.2f\n",(double)runTime.l/(double)HZ);
  }

#ifdef PROFILE
  if(profile) profile_again(argc,argv);
#endif

#if INSCOUNT
  if(insCount) {
    printIns();
  }
#endif

  if(gcStatics) {
    finishGc(Hp,1);
  }

#ifdef DBGTRANS
  fclose(HatFile);
#endif

  return exit_code;
}

