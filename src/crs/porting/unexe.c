int initflag = 0;

#include <stdio.h>

char *data_end;
extern char *sbrk();
extern unexec();
char stdin_buf[BUFSIZ], stdout_buf[BUFSIZ];

main(argc, argv)
int argc; char ** argv;
{ char *data_start;
  if (!initflag) {
    initflag = 1;
    printf("brk(0): %d\n", sbrk(0));
    printf("data begin: %d\n", &initflag);
    data_start = sbrk(2000);
    brk(data_start + 500);
    data_end = sbrk(0);
    printf("data_start: %d, data_end: %d, sbrk: %d\n",
	    data_start, data_end, sbrk(0));
    unexec("/tmp/foo", argv[0]);
  } else {
    printf ("end: %d, sbrk(0): %d, sbrk(0): %d\n", data_end, sbrk(0), sbrk(0));
  }
}
