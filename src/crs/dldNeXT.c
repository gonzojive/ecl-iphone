/*  dldNeXT.c -- Dynamic loader for NeXT				*/
/*
    Copyright (c) 1994, Giuseppe Attardi

    This file is part of ECoLisp, an Embeddable Common Lisp.
    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecls.h"
#include <rld.h>
#include <fcntl.h>

#ifndef THREADS
static int code_size;
static char *code_start;
#endif

static unsigned long
alloc_code_block(unsigned long size, unsigned long headers_size) {
  code_size = size;
  code_start = (char *)alloc_contblock(size);
  return (unsigned long)code_start;
}

static void
load_mach_o(char               *filename)
{
    FILE               *fp;
    struct mach_header  header;
    char               *hdrbuf;
    struct load_command *load_command;
    struct segment_command *segment_command;
    struct section     *section;
    int                 len, cmd, seg;

    if ((fp = fopen(filename, "r")) == NULL)
	FEerror("Can't read Mach-O object file", 0);
    len = fread((char *)&header, sizeof(struct mach_header), 1, fp);
    if (len == 1 && header.magic == MH_MAGIC) {
	hdrbuf = (char *)malloc(header.sizeofcmds);
	len = fread(hdrbuf, header.sizeofcmds, 1, fp);
	if (len != 1)
	    FEerror("failure reading Mach-O load commands", 0);
	load_command = (struct load_command *) hdrbuf;
	for (cmd = 0; cmd < header.ncmds; ++cmd) {
	    if (load_command->cmd == LC_SEGMENT) {
		segment_command = (struct segment_command *) load_command;
		section = (struct section *) ((char *)(segment_command + 1));
		for (seg = 0; seg < segment_command->nsects; ++seg, ++section) {
		    if (section->size != 0 && section->offset != 0) {
#ifdef DEBUG
			fprintf(stderr, "section: %s, addr: 0x%08x, size: %d\n",
			   section->sectname, section->addr, section->size);
			fflush(stderr);
#endif
			fseek(fp, section->offset, 0);
			fread((char *)section->addr, section->size, 1, fp);
		    }
		}
	    }
	    load_command = (struct load_command *)
	      ((char *)load_command + load_command->cmdsize);
	}
	free(hdrbuf);
    }
    (void)fclose(fp);
}

/*
 *----------------------------------------------------------------------
 *
 * dld --
 *     dynamically load a file into memory.
 *
 * Results:
 *	none.
 *
 * Side effects:
 *	codeblock: containing address where the code has been loaded, and
 *		   its size
 *
 *----------------------------------------------------------------------
 */
dld(char *faslfile, struct codeblock *Cblock)
{
  struct mach_header *hdr;
  char *files[2] = {faslfile, 0};
  char tempfile[40];

  rld_address_func(alloc_code_block);
  sprintf(tempfile, "/tmp/fasltemp%d", getpid());
  if (!rld_load(NULL, &hdr, files, tempfile))
    FEerror(";;; rld_load() failed", 0);
  load_mach_o(tempfile);
  unlink(tempfile);
  Cblock->cd_size = code_size;
  Cblock->cd_start = code_start;
}

/*
 *----------------------------------------------------------------------
 *
 * faslink --
 *     dynamically load a file into memory, linking it with additional
 *     libraries.
 *
 * Results:
 *     none.
 *----------------------------------------------------------------------
 */

static char *library_search_path[] =
{"/lib", "/usr/lib", "/usr/local/lib", NULL};

#define strdup(string)	strcpy((char *)malloc(strlen(string)+1),(string))

static char*
expand_library_filename(char *filename)
{
    int                 fd;
    char              **dir;
    char                libname[256];
    char                fullname[256];

    if (filename[0] == '-' && filename[1] == 'l') {
	filename +=2;
	strcpy(libname, "lib");
	strcat(libname, filename);
	strcat(libname, ".a");
	for (dir = library_search_path; *dir; dir++) {
	    strcpy(fullname, *dir);
	    strcat(fullname, "/");
	    strcat(fullname, libname);
	    if ((fd = open(fullname, O_RDONLY, 0)) > 0) {
		close(fd);
		return (strdup(fullname));
	    }
	}
	return (strdup(libname));
    }
    return (strdup(filename));
}

static char**
make_ofile_list(char *faslfile, char *argstr)
{
  char                filename[256];
  char               *dst;
  int                 i;
  char              **ofile_list;

  ofile_list = (char **)calloc(1, sizeof(char *));
  ofile_list[0] = strdup(faslfile);
  i = 1;
  if (argstr != NULL) {
    for (;; i++) {
      while ((*argstr == ' ') && (*argstr != '\0'))
	argstr++;
      if (*argstr == '\0')
	break;
      dst = filename;
      while ((*argstr != ' ') && (*argstr != '\0'))
	*dst++ = *argstr++;
      *dst = '\0';
      ofile_list = (char **)realloc((void *)ofile_list,
				    (i + 1) * sizeof(char *));
      ofile_list[i] = expand_library_filename(filename);
    }
  }
  ofile_list = (char **)realloc((void *)ofile_list, (i + 1) * sizeof(char *));
  ofile_list[i] = NULL;
  return (ofile_list);
}

static void
free_ofile_list(char **ofile_list)
{
    int i;

    for (i = 1;; i++) {
	if (ofile_list[i] == NULL)
	    break;
	(void)free(ofile_list[i]);
    }
    (void)free(ofile_list);
}

int
faslink(object faslfile, object ldargstring)
{
  object temp_cfun, dataStream;
  struct codeblock Cblock;
  char *faslfilename;
  char *ldargstr;
  char **ofiles;
  struct mach_header *hdr;

  ldargstr = coerce_to_filename(ldargstring);
  faslfilename = coerce_to_filename(faslfile);

  printf(";;; Linking %s\n", faslfilename);
  rld_address_func(alloc_code_block);
  ofiles = make_ofile_list(faslfilename, ldargstr);
  if (!rld_load(NULL, &hdr, ofiles, NULL)) {
    free_ofile_list(ofiles);
    FEerror(";;; rld_load() failed", 0);
  }
  free_ofile_list(ofiles);
  /* preserve Cblock from GC */
  temp_cfun = alloc_object(t_cfun);
  temp_cfun->cf.cf_name = OBJNULL;
  temp_cfun->cf.cf_block = &Cblock;
  Cblock.cd_size = code_size;
  Cblock.cd_start = code_start;
  Cblock.cd_data = OBJNULL;

  dataStream = (object)open_fasl_data(faslfile);

  asm("trap #2");		/* clear cache */
  (*(int (*)())(Cblock.cd_start))(Cblock.cd_size, dataStream);
  close_stream(dataStream, TRUE);

  return(0);
}
