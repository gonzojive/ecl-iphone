/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*
	unixsave.c
*/

#include "config.h"
#include "page.h"
#include "objff.h"
#include <sys/file.h>

#ifdef COFF
#  define a_text	tsize
#  define a_data	dsize
#  define a_bss		bsize
#endif


#ifdef apollo
#  define SEGSIZ	65536
#endif

#ifdef IBMRT
#  define PAGE_SIZE	2048
#endif

#ifdef SEQ
#  define SEGSIZ	2048
#endif

#ifdef TAHOE
#  define SEGSIZ	1024
#endif

#ifdef VAX
#  define PAGE_SIZE	1024
#  define SEGSIZ	1024
#endif

#ifndef TXTRELOC
#  define TXTRELOC	0
#endif

filecpy(FILE *to, FILE *from, register int n)
{
  char buffer[BUFSIZ];

  while (n > BUFSIZ) {
    fread(buffer, BUFSIZ, 1, from);
    fwrite(buffer, BUFSIZ, 1, to);
    n -= BUFSIZ;
  }
  if (n > 0) {
    fread(buffer, 1, n, from);
    fwrite(buffer, 1, n, to);
  }
}

#define ADJUST(field) if (field) field += diff

unexec(char *save_file, char *original_file,
       unsigned data_start, unsigned bss_start, unsigned entry_address)
{
#ifdef AOUT
	struct exec header;
#endif AOUT
#ifdef COFF
	FILHDR fileheader;
	AOUTHDR header;
	SCNHDR sectionheader;
	long diff, text_scnptr;
#endif COFF

	char *data_begin;
	int text_size, data_size, bss_size, str_size, data_padding;
	FILE *original, *save, *standard_error;
	register int n;
	register char *p;

	extern char stdin_buf[BUFSIZ], stdout_buf[BUFSIZ];

	_cleanup();

	original = freopen(original_file, OPEN_R, stdin);
	if (stdin != original || fileno(original) != 0) {
		fprintf(stderr, "Can't open the original file.\n");
		exit(1);
	}
	setbuf(original, stdin_buf);

#ifndef apollo
	/* not unlinking the previous executable is a trick since I dont know
	   how to create a file whose type is COFF (rather then unstruct) */
	unlink(save_file);
#endif apollo

#ifdef MSDOS
	save = fopen(save_file, OPEN_W);
#else
	save = freopen(save_file, OPEN_W, stdout);
#endif
	chmod(save_file, 0775);
	setbuf(save, stdout_buf); /* avoid malloc call by fwrite */

/* ---------------------------------------------------------------------- */
#ifdef AOUT
/* ---------------------------------------------------------------------- */

# if defined(VAX) || defined(NEWS) || defined(SEQ) || defined(TAHOE)
#  define N_DATADDR(hdr)  ((TXTRELOC + hdr.a_text+(SEGSIZ-1)) & ~(SEGSIZ-1))
# elif defined(IBMRT)
#  define N_DATADDR(hdr)  (TXTRELOC + hdr.a_text);
# elif defined(hp9000s300)
#  define N_DATADDR(hdr)  ((hdr.a_magic.file_type == SHARE_MAGIC || \
			    hdr.a_magic.file_type == DEMAND_MAGIC) ? \
			    EXEC_ALIGN(hdr.a_text) : hdr.a_text);
# endif

	n = 3;	/* section count */
	fread(&header, sizeof(header), 1, original);

	data_begin = (char *)N_DATADDR(header);
	text_size = N_DATOFF(header) - N_TXTOFF(header);
	data_size = header.a_data;
	bss_size = header.a_bss;

	/* The file generated will have:
	   1. updated header;
	   2. same text section as original;
	   3. data section dumped from memory;
	   4. bss section empty;
	   5. syms, trel and drel sections as original;
	   6. strings as original.
	 */
	/* Update header before writing */
	header.a_data = data_end - data_begin;
# if defined(__linux__) || defined(__EMX__)
	data_padding = SEGMENT_SIZE - header.a_data % SEGMENT_SIZE;
	header.a_data += data_padding;
# endif
	header.a_bss = 0;
	fwrite(&header, sizeof(header), 1, save);

# if defined(__linux__) || defined(__EMX__)
	if (N_MAGIC(header) == QMAGIC)
	  filecpy(save, original, text_size - sizeof(header));
	else
	  filecpy(save, original, text_size + _N_HDROFF(header));
# elif defined(MSDOS)
	filecpy(save, original, text_size);
# elif defined(SEQ)
	filecpy(save, original, header.a_text - 
				N_ADDRADJ(header) - sizeof(header));
# elif defined(hp9000s300)
	if (header.a_magic.file_type == DEMAND_MAGIC) {
	  filecpy(save, original, EXEC_PAGESIZE - sizeof(header));
	  filecpy(save, original, EXEC_ALIGN(header.a_text));}
        else
	  filecpy(save, original, header.a_text);
# elif defined(vax) || defined(NEWS) || defined(IBMRT)
	if (N_MAGIC(header) == ZMAGIC)
          filecpy(save, original, PAGE_SIZE - sizeof(header));
	filecpy(save, original, header.a_text);
# elif defined(BSD)
	filecpy(save, original, header.a_text - sizeof(header));
# endif
#endif AOUT

/* ---------------------------------------------------------------------- */
#ifdef COFF
/* ---------------------------------------------------------------------- */
	n = 0;	/* section count */
	fread(&fileheader, sizeof(fileheader), 1, original);
	fread(&header, sizeof(header), 1, original);

	data_begin = (char *)header.data_start;
#  ifdef apollo
	data_begin += FILHSZ + sizeof(header)
		      + fileheader.f_nscns * sizeof(struct scnhdr);
#  endif apollo
	data_size = header.a_data;
	header.a_data = data_end - data_begin;
	diff = header.a_data - data_size;
	fileheader.f_symptr += diff;
	fwrite(&fileheader, sizeof(fileheader), 1, save);

	header.a_bss = 0;
#  ifdef apollo
	header.o_sri += diff;
	header.o_inlib += diff;
#  endif apollo
#  ifdef __mips
	header.bss_start = header.data_start + header.a_data;
	/* tsize includes headers which are also loaded into memory */
#  endif __mips

	fwrite(&header, sizeof(header), 1, save);

	/* .text */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	text_scnptr = sectionheader.s_scnptr;
	ADJUST(sectionheader.s_relptr);
	ADJUST(sectionheader.s_lnnoptr);
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);

#  ifdef apollo

	/* .unwind */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	ADJUST(sectionheader.s_relptr);
	ADJUST(sectionheader.s_lnnoptr);
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);

	/* .aptv */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	ADJUST(sectionheader.s_relptr);
	ADJUST(sectionheader.s_lnnoptr);
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);
#  endif apollo

#  ifdef ECOFF

	/* .init */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);

	/* .data */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);

	/* .rdata */
#  else
	/* .data */
#  endif ECOFF
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	sectionheader.s_size += diff;
#  ifdef apollo
	/* the APTV at the beginning of data section has already
	   been relocated at first initialization.
	   Avoid doing it again.
	 */
	sectionheader.s_nreloc = 0;
#  endif apollo
	ADJUST(sectionheader.s_relptr);
	ADJUST(sectionheader.s_lnnoptr);
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);

	/* copy all remaining section headers */
	for (;  n < fileheader.f_nscns;  n++) {
	  fread(&sectionheader, sizeof(sectionheader), 1, original);
	  if (strcmp(sectionheader.s_name, ".bss") == 0) {
	    sectionheader.s_size = header.a_bss;
	    ADJUST(sectionheader.s_paddr);
	    ADJUST(sectionheader.s_vaddr);
	  }
	  ADJUST(sectionheader.s_scnptr);
	  ADJUST(sectionheader.s_relptr);
	  ADJUST(sectionheader.s_lnnoptr);
	  fwrite(&sectionheader, sizeof(sectionheader), 1, save);
	}

	/* copy the text section */
	text_size = header.a_text;
# ifdef MSDOS
#  define SECTION_ALIGNMENT 1023
#  define ADJUST_TEXT_SCNHDR_SIZE
# endif
# ifdef SECTION_ALIGNMENT
	/* Some systems require special alignment
	   of the sections in the file itself.  */
	text_size = (text_size + SECTION_ALIGNMENT) & ~SECTION_ALIGNMENT;
# endif
# ifdef ADJUST_TEXT_SCNHDR_SIZE
	/* On some machines, `text size' includes all headers.  */
	text_size -= FILHSZ + AOUTSZ + SCNHSZ * fileheader.f_nscns;
# endif
	filecpy(save, original, text_size);

#endif COFF
/* ---------------------------------------------------------------------- */

	/* write the new data section */
#if defined(__linux__) || defined(__EMX__)
	/* The heap starts at a SEGMENT_SIZE boundary */
	for (n = data_size + bss_size, p = data_begin; n > BUFSIZ ;
	     n -= BUFSIZ, p += BUFSIZ)
	  fwrite(p, BUFSIZ, 1, save);
	if (n > 0)
	  fwrite(p, 1, n, save);
	n = SEGMENT_SIZE - (data_size + bss_size) % SEGMENT_SIZE;
	/* write 0's to fill gap */
	{ int j; char pad[512];
	  for (j = n; j > 512 ; j -= 512)
	    fwrite(&pad, 512, 1, save);
	  if (j > 0)
	    fwrite(&pad, 1, j, save);
	}
	header.a_data -= data_size + bss_size + n + data_padding;
	data_begin += data_size + bss_size + n;
#endif
	for (n = header.a_data, p = data_begin; n > BUFSIZ ;
	     n -= BUFSIZ, p += BUFSIZ)
	  fwrite(p, BUFSIZ, 1, save);
	if (n > 0)
	  fwrite(p, 1, n, save);

#if defined(__linux__) || defined(__EMX__)
	/* pad data section up to SEGMENT_SIZE */
	{ int j; char pad[512];
	  for (j = data_padding; j > 512 ; j -= 512)
	    fwrite(&pad, 512, 1, save);
	  if (j > 0)
	    fwrite(&pad, 1, j, save);
	}
#endif __EMX__

	/*  skip data section of original file  */
	fseek(original, data_size, 1);

#ifdef hpux
	fseek(save, MODCAL_OFFSET(header), 0);
	header.a_data = data_size;
	fseek(original, MODCAL_OFFSET(header), 0);
#endif hpux

#ifdef ECOFF
	{  HDRR symhdr; 

	   /* copy up to Symbol Table */
	   filecpy(save, original, N_SYMOFF(fileheader) - ftell(original));
	   /* update Symbol Table Header */
	   fread(&symhdr, cbHDRR, 1, original);
	   ADJUST(symhdr.cbLineOffset);
	   ADJUST(symhdr.cbDnOffset);
	   ADJUST(symhdr.cbPdOffset);
	   ADJUST(symhdr.cbSymOffset);
	   ADJUST(symhdr.cbOptOffset);
	   ADJUST(symhdr.cbAuxOffset);
	   ADJUST(symhdr.cbSsOffset);
	   ADJUST(symhdr.cbSsExtOffset);
	   ADJUST(symhdr.cbFdOffset);
	   ADJUST(symhdr.cbRfdOffset);
	   ADJUST(symhdr.cbExtOffset);

	   fwrite(&symhdr, cbHDRR, 1, save);
	 }
#endif ECOFF

	/* Copy the rest */
	filecpy(save, original, file_len(original) - ftell(original));
	fclose(original);
	fclose(save);
}
