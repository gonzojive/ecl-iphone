/* tcp.c  -- stream interface to TCP					*/

/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Library Public License as published
    by the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecls.h"

cl_object
make_stream(cl_object host, int fd, enum smmode smm)
{
   cl_object stream;
   char *mode;			/* file open mode */
   FILE *fp;			/* file pointer */

   switch(smm) {
    case smm_input:
      mode = "r";
      break;
    case smm_output:
      mode = "w";
      break;
    default:
      FEerror("make_stream: wrong mode", 0);
   }
   fp = fdopen(fd, mode);

   stream = alloc_object(t_stream);
   stream->stream.mode = (short)smm;
   stream->stream.file = fp;
   stream->stream.object0 = Sbase_char;
   stream->stream.object1 = host; /* not really used */
   stream->stream.int0 = stream->stream.int1 = 0;
#if !defined(GBC_BOEHM)
   fp->_IO_buf_base = NULL; /* BASEFF */; 
   setbuf(fp, stream->stream.buffer = alloc_atomic(BUFSIZ)); 
#endif
   return(stream);
}

/* 
   Lopen_client_stream --

   To test this function, try:
   (setq s (si:open-client-stream "host" 13))
   (read-line s)
   "Wed Jun 22 19:44:36 METDST 1994"
*/
@(defun open_client_stream (host port)
   int fd;			/* file descriptor */
   cl_object streamIn, streamOut;
@
   if (type_of(host) != t_string)
     FEwrong_type_argument(Sstring, host);

   if (!FIXNUMP(port))
     FEwrong_type_argument(TSpositive_number, port);

   /* FIXME! Why? */
   if (host->string.fillp > BUFSIZ - 1)
     FEerror("~S is a too long file name.", 1, host);

#ifdef THREADS
   start_critical_section();
#endif THREADS
   fd = connect_to_server(host->string.self, fix(port)); 
#ifdef THREADS
   end_critical_section();
#endif THREADS

   if (fd == 0)
     @(return Cnil)

   streamIn = make_stream(host, fd, smm_input);
   streamOut = make_stream(host, fd, smm_output);

   @(return make_two_way_stream(streamIn, streamOut))
@)

@(defun open_server_stream (port)
   int fd;			/* file descriptor */
   cl_object streamIn, streamOut;
   cl_object output;
@
   if (!FIXNUMP(port))
     FEwrong_type_argument(TSpositive_number, port);

#ifdef THREADS
   start_critical_section();
#endif THREADS
   fd = create_server_port(fix(port));
#ifdef THREADS
   end_critical_section();
#endif THREADS

   if (fd == 0)
     output = Cnil;
   else {
     streamIn = make_stream(Cnil, fd, smm_input);
     streamOut = make_stream(Cnil, fd, smm_output);
     output = make_two_way_stream(streamIn, streamOut);
   }
   @(return output)
@)


