#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NO  -1
#define YES 0

int
local_bind (short port)
{
  int sd;
  if ((sd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    fprintf(stderr,"System call 'socket()' failed\n");
    exit(1);
  } else
    return bind_address(sd,port);
}

int
main (void)
{
  int port=6710, tries=15;
  int i, s;
#if defined(__linux) || defined(__NetBSD__)
  struct sockaddr addr;
#else
  struct sockaddr_in addr;
#endif
  struct hostent *host;

  i=0;
  while (i<tries && listen(s = local_bind(port+i), 5))
    i++;
  if (i==tries) {
    fprintf(stderr, "Couldn't establish listen socket between %d and %d\n",
			port, port+i-1);
    exit(1);
  }
  fprintf(stderr, "Established socket port %d is free\n", port+i);
  close(s);
  fprintf(stdout, "%d\n", port+i);
  exit(0);
}
