#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>

/* A sample client program for bidirectional communication with the server.
 */

void quit(char *msg)
{
  perror(msg);
  exit(1);
}

void talk(int s)
{
  int n;
  char buf[512];

  switch(fork()) {
    case -1:
      quit("fork");
      break;
    case 0:
      while(1) {
        n=read(0,buf,sizeof(buf));
        if(n<=0) break; /* end of file? */
        write(s,buf,n);
      }
      break;
    default:
      while(1) {
        n=read(s,buf,sizeof(buf));
        if(n<=0) break; /* end of file? */
        write(0,buf,n);
      }
      break;
  }
}

void main(int argc,char **argv)
{
  int port;
  int s;

  if(argc==3) {
    port=atoi(argv[2]);
    s=in_connect_ne(argv[1],port,SOCK_STREAM);
    talk(s);
  }
  else fputs("Usage: client host portnr\n",stderr);
}
