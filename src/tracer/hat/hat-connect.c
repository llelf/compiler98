#include "browsercomms.h"

#define	MAX_LINE_SIZE	1024
#define	MAX_SYM_SIZE	128
#ifndef PRELUDE_PATH
#define PRELUDE_PATH PRELSRCDIR
#endif

char *paths[] = {0, 0, 0}; /* Filled in by init routine */
char *prelude_path = NULL;

void dumpOutput(FILE* sock) {
  char str[16];
  sprintf(str, "%d\n", outputsize);
  ToBrowser(sock, str);
  dumpOutputToBrowser(sock);
  ToBrowser(sock, "\n");
}

int
main (int argc, char** argv)
{
    FILE *sock;
    int i;
    char str[16], *s;
    FileOffset fo;

    initialise(argc,argv);

    paths[0] = getcwd(NULL, 160);
    paths[1] = getenv("TRACE_SOURCEPATH");
    s = getenv("TRACE_PRELUDEPATH");
    prelude_path = s != NULL ? s : PRELUDE_PATH;

    ncInit();

    if (!(sock = waitForBrowserConnection())) {
      fprintf(stderr, "Couldn't connect to redex trail browser\n");
      exit(1);
    }
    fprintf(stderr, "Haskell redex trail browser connected\n");

    if (remoteStartNode) {
      ToBrowser(sock,"Remote\n");

      // dump output to browser
      dumpOutput(sock);
      // output dumped

      dumpTraceToBrowser(sock,remoteStartNode);
      loop(sock, remoteStartNode);
    } else
    if (errorRoot&&!ignoreErrors) {
      ToBrowser(sock, "Error\n");
      
      // modification: still show program's output, even if an error occurred
      dumpOutput(sock);
      // end of modification (also see OutputPanel.java)

      dumpTraceToBrowser(sock, errorRoot);
      loop(sock, errorRoot);
    } else {
      ToBrowser(sock, "Output\n");
      dumpOutput(sock);
      loop(sock, 0);
    }

    fclose(sock);
    finalise();
}

