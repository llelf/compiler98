#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
/*#include <sys/un.h>*/
#include <netinet/in.h>
#include <errno.h>

#define MAXCLIENTS 20

void quit(char *msg)
{
  perror(msg);
  exit(1);
}

void start_demo(int s)
{
  struct sockaddr_in addr;
  fd_set rdset,clientset,empty;
  int j,i,n,len,clients;
  int maxfd;
  int cfd[MAXCLIENTS];
  char buf[512];

  maxfd=getdtablesize();
  FD_ZERO(&clientset);
  clients=0;
  while(1) {
    rdset=clientset;
    FD_SET(s,&rdset);
    puts("Waiting for some input...");
    n=select(maxfd,&rdset,NULL,NULL,NULL);
    if(n<0) quit("select");
    puts("Got something...");
    if(n==0) {
      /* timeout */;
    }
    else {
      if(FD_ISSET(s,&rdset)) {
        /* new connection available */
        puts("New connection");
        len=sizeof(addr);
        cfd[clients]=accept(s,&addr,&len);
        if(cfd[clients]<0) quit("accept");
        printf("Accepted client #%d on fd %d\n",clients,cfd[clients]);
	FD_SET(cfd[clients],&clientset);
        clients++;
      }
      for(i=0;i<clients;i++) {
	if(FD_ISSET(cfd[i],&rdset)) {
	  /* data available to read from client i */
	  printf("Reading from client #%d\n",i);
	  n=read(cfd[i],buf,sizeof(buf));
	  if(n<0) quit("read");
	  if(n>0) {
	    /* got data from a client */
	    write(1,buf,n); /* show it on stdout */
	    for(j=0;j<clients;j++) {
	      if(j!=i) write(cfd[j],buf,n); /* send it to other clients */
	    }
	  }
	  else {
	    /* n==0: end of file, close connection? */
	    puts("Got 0 bytes. Closing connection..");
	    close(cfd[i]);
	    FD_CLR(cfd[i],&clientset);
	    clients--;
	    if(i<clients) {
	      printf("Moving client #%d to #%d.\n",clients,i);
	      cfd[i]=cfd[clients];
	      i--; /* musn't miss last client in loop */
	    }
	  }
	}
      }
    }
  }
}

void main(int argc,char **argv)
{
  int port;
  int s;

  if(argc==2) {
    port=atoi(argv[1]);
    s=in_bind_ne(port,SOCK_STREAM);
    if(listen(s,5)) quit("listen");
    start_demo(s);
  }
  else fputs("Usage: server portnr\n",stderr);
}
