#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
/*#include <sys/un.h>*/
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <ctype.h>
#include <sys/param.h>

#ifdef MAXHOSTNAMELEN
#define HOST_NAME_LEN		MAXHOSTNAMELEN
#else
#define HOST_NAME_LEN		32
#endif

#ifndef FALSE
#define FALSE			0
#define TRUE			1
#endif

#define CLOSE( sd, t )\
    t = errno;\
    (void)close(sd);\
    errno = t

#define SYSCALL_ERROR		-1
#define RESOLVER_ERROR		-2
#define OPTION_ERROR		-3

/*
 * How many times to try gethostbyname
 */
#define MAX_TRIES				3

/*
 * This function prints a description of the resolver error
 * and terminates the process
 */
static void resolver_error(char *call, char *host)
{
    fprintf(stderr, "%s: resolver error. ", call);

#ifdef HAS_H_ERRNO
    switch (errno)
    {
    case HOST_NOT_FOUND:
	fprintf(stderr, "Host not found. ");
	break;
    case TRY_AGAIN:
	fprintf(stderr, "Got a TRY_AGAIN after %d times. ", MAX_TRIES);
	break;
    case NO_RECOVERY:
	fprintf(stderr, "Non-recoverable error. ");
	break;
    case NO_DATA:
	fprintf(stderr,
		"Valid name, but no data record of requested type. ");
	break;
#if NO_ADDRESS != NO_DATA
    case NO_ADDRESS:
	fprintf(stderr, "No address. ");
	break;
#endif
    default:
	fprintf(stderr, "UNKNOWN h_errno VALUE (h_errno=%d). ", errno);
	break;
    }
#endif 	/* HAS_H_ERRNO */
    fprintf(stderr, "Host = %s\n", ( host ) ? host : "LOCAL");
    /* exit 1; */
}


/*
 * bind_address binds the address <INADDR_ANY,port> to a socket.
 * Return value:
 *		socket			: if successful
 *		SYSCALL_ERROR	: if bind fails
 *		RESOLVER_ERROR	: if it can't get the address of the local host
 */
int bind_address(int sd, short port)
{
    struct sockaddr_in sin;
    int errno_save;

    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = INADDR_ANY;
    sin.sin_port = htons(port);

    if (bind(sd, (struct sockaddr *)&sin, sizeof(sin)) == -1) {
	CLOSE(sd, errno_save);
	return SYSCALL_ERROR;
    }
    return sd;
}

int in_bind(short port, int type)
{
    int sd;

    if ((sd = socket( AF_INET, type, 0)) == -1)
	return SYSCALL_ERROR;
    return(bind_address(sd, port));
}
	
#define TYPE_NAME_LEN					10

/*
 * Returns a string with the socket type (e.g. "SOCK_STREAM" for SOCK_STREAM)
 */
static char *socket_type_name(int type)
{
    static char type_name[TYPE_NAME_LEN];

    if (type == SOCK_STREAM)
	return "SOCK_STREAM";
    else if (type == SOCK_DGRAM)
	return "SOCK_DGRAM";
    else {
	(void) sprintf(type_name, "%d", type);
	return type_name;
    }
}

#define MAXTRIES 5
int in_bind_ne(short port, int type)
{
    char num[10];
    char *type_name;
    int tries = 0, sd;

    while ((tries++ < MAXTRIES) && ((sd = in_bind(port, type)) < 0))
	sleep(1);

    if (sd < 0)
	switch (sd)
	{
	case SYSCALL_ERROR:
	    fprintf(stderr, "in_bind_ne: bind", "port = %d, type = %s\n", 
		 port, socket_type_name(type));
	    /* NOTREACHED */
	case RESOLVER_ERROR:
	    resolver_error("in_bind_ne", NULL);
	    /* NOTREACHED */
	default:
	    fprintf(stderr,
		    "in_bind_ne: in_bind RETURNED BAD VALUE: %d\n", sd);
	    exit(1);
	}
    return sd;
}

/*
 * Returns TRUE if the host argument has the form:
 *		<num>.<num>
 *		<num>.<num>.<num>
 *		<num>.<num>.<num>.<num>
 */
static int is_number_address(char *host)
{
    register int dot_count = 0;
    register char *p;

    if (host == NULL)
	return(FALSE);
	
    for (p = host; *p; p++)
	if (isascii(*p) && isdigit(*p))		/* skip digits */
	    continue;
	else if (*p == '.') {			/* count dots */
	    dot_count++;
	    if (dot_count > 3)
		return( FALSE );
	} else					/* reject everything else */
	    return(FALSE);
	return dot_count > 0;
}

/*
 * Form an Internet address.
 * If host is NULL, the current host is assumed (but this does not
 * guarantee the use of the loopback address)
 *
 * Return value:
 *		0					: if successful
 *		RESOLVER_ERROR	: if an error occurs
 *
 * In case of error, errno is set to the value of h_errno, if the OS
 * supports h_errno; if not, the value of errno is undefined.
 */
int in_address(char *host, short port, struct sockaddr_in *address )
{
    char host_name[HOST_NAME_LEN] ;
    char *the_host;
    struct hostent *hp;

    bzero((char *)address, sizeof(*address));

    /*
     * Determine the host name
     */
    if (host == NULL) {
	(void)gethostname(host_name, HOST_NAME_LEN);
	the_host = host_name;
    } else
	the_host = host;
	
    /*
     * Get host address
     */
    if (is_number_address(host))
	address->sin_addr.s_addr = inet_addr(host);
    else {
#ifndef HAS_H_ERRNO
	if ((hp = gethostbyname(the_host)) == NULL)
	    return RESOLVER_ERROR;
#else
	int tries = 0;
	while ((hp = gethostbyname(the_host)) == NULL &&
	       h_errno == TRY_AGAIN &&
	       tries < MAX_TRIES)
	    tries++;
	if (hp == NULL) {
	    errno = h_errno;
	    return RESOLVER_ERROR;
	}
#endif	/* HAS_H_ERRNO */
	bcopy(hp->h_addr, (char *)&address->sin_addr, hp->h_length);
    }

    address->sin_family = AF_INET;
    address->sin_port = htons(port);

    return 0;
}

/*
 * connect_to_host tries to connect socket sd to the address <host,port>
 * Return value:
 *		socket				: if the operation is successful
 *		RESOLVER_ERROR		: if there is a resolver error
 *		SYSCALL_ERROR		: if connect(2) fails
 */
static int connect_to_host(int sd, char *host, short port)
{
    struct sockaddr_in sin;
    int errno_save;

    if (in_address(host, port, &sin) == RESOLVER_ERROR) {
	CLOSE(sd, errno_save);
	return RESOLVER_ERROR;
    }

    if (connect(sd, (struct sockaddr *)&sin, sizeof(sin)) == -1) {
	CLOSE(sd, errno_save);
	return SYSCALL_ERROR;
    }
    return sd;
}

/*
 * Connect to an internet address. The address is specified by the
 * host, port arguments. Type is the type of the socket which
 * defines the type of the protocol to be used.
 * If host is NULL, then the local host is implied.
 *
 * Return value:
 *		a file descriptor	:  if the operation is successful
 *		SYSCALL_ERROR 		: if there is a system call error
 *		RESOLVER_ERROR 	: if there is a resolver error
 * In case of error errno contains a description of the error
 */
int in_connect(char *host, short port, int type)
{
    int sd;

    if ((sd = socket(AF_INET, type, 0)) == -1)
	return SYSCALL_ERROR;	
    return connect_to_host(sd, host, port);
}

in_connect_ne(char *host, short port, int type)
{
    int sd = in_connect(host, port, type);

    if (sd < 0)
	switch (sd)
	{
	case SYSCALL_ERROR:
	    fprintf(stderr, "in_connect_ne: connect", "host = %s, port = %d, type = %s\n",
		 host, port, socket_type_name(type));
	    break;
	case RESOLVER_ERROR:
	    resolver_error("in_connect_ne", host);
	    break;
	default:
	    fprintf(stderr,
		    "in_connect_ne: in_connect RETURNED BAD VALUE: %d\n", sd);
	    /* exit( 1 ) ; */
	}
    return sd;
}
