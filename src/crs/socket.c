/*  socket.c  -- socket interface					*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Library Public License as published
    by the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <stdio.h>
#include <sys/types.h>
#include <errno.h>

#include <netinet/in.h>
#include <netdb.h> 
#include <sys/socket.h>
#include <string.h>

#include <sys/ioctl.h>

extern int errno;

/***********************************************************************
 * Client side
 **********************************************************************/

/* 
 * Attempts to connect to server, given host and port. Returns file 
 * descriptor (network socket) or 0 if connection fails.
 */
int connect_to_server(char *host, int port)
{
  struct sockaddr_in inaddr;	/* INET socket address. */
  struct sockaddr *addr;	/* address to connect to */
  struct hostent *host_ptr;
  int addrlen;			/* length of address */
  extern char *getenv();
  extern struct hostent *gethostbyname();
  int fd;			/* Network socket */

  /* Get the statistics on the specified host. */
  if ((inaddr.sin_addr.s_addr = inet_addr(host)) == -1) {
    if ((host_ptr = gethostbyname(host)) == NULL) {
      /* No such host! */
      errno = EINVAL;
      return(0);
    }
    /* Check the address type for an internet host. */
    if (host_ptr->h_addrtype != AF_INET) {
      /* Not an Internet host! */
      errno = EPROTOTYPE;
      return(0);
    }
    /* Set up the socket data. */
    inaddr.sin_family = host_ptr->h_addrtype;
    memcpy((char *)&inaddr.sin_addr, (char *)host_ptr->h_addr,
	   sizeof(inaddr.sin_addr));
  } 
  else
    inaddr.sin_family = AF_INET;

  addr = (struct sockaddr *) &inaddr;
  addrlen = sizeof (struct sockaddr_in);
  inaddr.sin_port = port;
  inaddr.sin_port = htons(inaddr.sin_port);
  /*
   * Open the network connection.
   */
  if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0)
    return(0);			/* errno set by system call. */

#ifdef TCP_NODELAY
  /* make sure to turn off TCP coalescence */
  { int mi;
    setsockopt (fd, IPPROTO_TCP, TCP_NODELAY, &mi, sizeof (int));
  }
#endif
#ifdef THREADS
  start_critical_section();
#endif
  if (connect(fd, addr, addrlen) == -1) {
    (void) close (fd);
#ifdef THREADS
    end_critical_section();
#endif
    return(0);		/* errno set by system call. */
  }
  /*
   * Return the id if the connection succeeded.
   */
  return(fd);
}


/***********************************************************************
 * Server side
 **********************************************************************/
/*
 * Creates a server port. Returns file 
 * descriptor (network socket) or 0 if connection fails.
 */

int
create_server_port(int port)
{
  struct sockaddr_in inaddr;	/* INET socket address. */
  struct sockaddr *addr;	/* address to connect to */
  int addrlen;			/* length of address */
  int request, conn;		/* Network socket */

  /*
   * Open the network connection.
   */
  if ((request = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    return(0);			/* errno set by system call. */
  }

#ifdef SO_REUSEADDR
    /* Necesary to restart the server without a reboot */
    {
	int one = 1;
	setsockopt(request, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(int));
    }
#endif /* SO_REUSEADDR */
#ifdef TCP_NODELAY
  /* make sure to turn off TCP coalescence */
  { int mi;
    setsockopt(request, IPPROTO_TCP, TCP_NODELAY, &mi, sizeof (int));
  }
#endif

  /* Set up the socket data. */
  memset((char *)&inaddr, 0, sizeof(inaddr));
  inaddr.sin_family = AF_INET;
  inaddr.sin_port = htons(port);
  inaddr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(request, (struct sockaddr *)&inaddr, sizeof (inaddr)))
    FEerror("Binding TCP socket", 0);
  if (listen(request, 1))
    FEerror("TCP listening", 0);
#ifdef THREADS
  /* Don't make this file-descriptor non-blocking
   * just block on it before we attempt to accept from it
   * Think _hard_ about moving this out of here, into somewhere sane
   * and creating an 'accepting' stream type, which is bound to a port
   * on reading returns streams
   */
  {
    FILE *fp;			/* need to use FILE *'s rather than fd... *sigh* */
    if ((fp = fdopen(request, "r")) == (FILE *)0)
      printf("fdopen didn't work on accept fd!\n"); fflush(stdout);
    fcntl(request, F_SETFL, O_NONBLOCK);
    clearerr(fp);

  loop:	errno = 0;
    if ((conn = accept(request, (struct sockaddr *)NULL, (int *)NULL)) < 0)
    if (errno) {
      lwpblockon(active, fp, PD_INPUT);
      clearerr(fp);
      goto loop;
    } else {
      fclose(fp);
      FEerror("Accepting requests", 0);
    }
    fclose(fp);
  }
#else
  if ((conn = accept(request, (struct sockaddr *)NULL, (int *)NULL)) < 0)
    FEerror("Accepting requests", 0);
#endif	/* THREADS */

  return(conn);
}
