#include <stdio.h>
#include <stdlib.h>

#define S "This is spaceman spiff!\n"

int
main(int argc,char **argv)
{
    char *sock_fds;
    int sock_fd;
    FILE *out;
    char line[128];
    int len;

    sock_fds = getenv("TRACE_FD");
    
    fprintf(stderr, "Demo program started, fd=%s\n", sock_fds);
    sock_fd = atoi(sock_fds);
    write(sock_fd, S, sizeof(S));
    len = read(sock_fd, &line[0], sizeof(line));
    line[len] = '\0';

    fprintf(stderr, "got %s\n", line);
}
