#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <time.h>
#include <ctype.h>
#include <sys/param.h>
#include <string.h>
#include <signal.h>

int max_clients;
int nclients = 0;

FILE *pidfile;

void quit(char *msg)
{
  perror(msg);
  exit(1);
}

void 
start_demo(int s, char *argv[])
{
    int sockfd;
    struct sockaddr addr;
    struct sockaddr_in addr_in;
    int addr_len, addr_in_len;
    struct hostent *host;
    char fd_str[32];
    FILE *log;
    long tod;
    struct tm *tmb;
    char *timestr, *str;
    log = fopen("/users/cs/sparud/tracer/demo/log.demo_server", "a");

    /* Run in the background */
    switch (fork()) {
	case -1:
	    quit("fork");
	    break;
	case 0: /* Child */
	    break;
	default:
	    exit(0);
    }
      
    fprintf(pidfile, "%d\n", getpid());
    fclose(pidfile);

    tod = 0;
    time(&tod);
    timestr = asctime(localtime(&tod));
    str = strchr(timestr, '\n');
    *str = '\0';
    fprintf(log, "%s  Server for %s restarted.\n", 
	    timestr, argv[0]);
    fflush(log);
    
    while (1) {

	/*fprintf(stderr, "Waiting for a connector...\n");*/
        addr_len=sizeof(addr);
	if ((sockfd = accept(s, &addr, &addr_len)) == -1) {
	    quit("Couldn't accept");
	}
	/* Only accept max_clients simultaneous connections */
	if (nclients == max_clients) {
	    close(sockfd);
	    continue;
	}
	nclients++;
	addr_in_len = sizeof(addr_in);
	if (getpeername(sockfd, (struct sockaddr *)&addr_in, &addr_in_len) < 0) {
	    quit("Couldn't get peer name");
	}
        host = gethostbyaddr((char *)&addr_in.sin_addr.s_addr, sizeof(struct in_addr), AF_INET);
	if (host == NULL) {
	    fprintf(stderr, "Bad host\n");
	}
	tod = 0;
	time(&tod);
	/*tmb = localtime(&tod);*/
	timestr = asctime(localtime(&tod));
	str = strchr(timestr, '\n');
	*str = '\0';
	fprintf(log, "%s  %s(%d): %s\n", 
		timestr, argv[0], nclients, host->h_name);
	fflush(log);
	switch (fork())
	{
	case -1:
	    quit("fork");
	    break;
	case 0: /* Child */
	    sprintf(&fd_str[0], "TRACE_FD=%d", sockfd);
	    putenv(fd_str);
	    chdir(argv[1]);
	    execv(argv[2], &argv[2]);
	    quit("Couldn't exec demo program!");
	    break;
	default:
	    /*fprintf(stderr, "Closing parent version of socket fd.\n");*/
	    close(sockfd);
	    break;
	}
    }
}

void
wait_for_children() {
   int child, status;
   while ((child = waitpid((pid_t) -1, &status, WNOHANG)) > 0) {
       nclients--;
     /*fprintf(stderr, "Connection terminated.\n");*/
   }
}

int 
main(int argc, char **argv)
{
  int port;
  int s;
  char pidfilename[16];

#if !defined(__linux__)
  signal(SIGCHLD, wait_for_children);
#endif
  if(argc > 3) {
    max_clients = atoi(argv[1]);
    port = atoi(argv[2]);
    s = in_bind_ne(port, SOCK_STREAM);
    if (listen(s, 5)) 
	quit("listen");
    sprintf(&pidfilename[0], "%d.pid", port);
    pidfile = fopen(pidfilename, "w");
    start_demo(s, &argv[3]);
  } else 
      fputs("Usage: demo_server max_clients portnr description directory program args\n", stderr);
  exit(0);
}
