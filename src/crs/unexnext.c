/*
 * unexec for the NeXT Mach environment.
 *
 * Bradley Taylor (btaylor@NeXT.COM) 
 * February 28, 1990
 * 
 * Modified by Noritake Yonezawa (yonezawa@cs.uiuc.edu)
 * July 28, 1991
 * 
 * Modified by Noritake Yonezawa (yone@vcdew25.lsi.tmg.nec.CO.JP)
 * February 16, 1992
 */
#undef __STRICT_BSD__

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <mach.h>
#include <sys/loader.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <libc.h>

#define CEIL(x,quantum) ((((int)(x))+(quantum)-1)&~((quantum)-1))

int malloc_cookie;

#ifndef BIG_HEAP_SIZE
#define BIG_HEAP_SIZE	0x1000000
#endif

int                 big_heap = BIG_HEAP_SIZE;

char               *mach_maplimit = 0;
char               *mach_brkpt = 0;

typedef struct region_t {
	vm_address_t address;
	vm_size_t size;
	vm_prot_t protection;
	vm_prot_t max_protection;
	vm_inherit_t inheritance;
	boolean_t shared;
	port_t object_name;
	vm_offset_t offset;
} region_t;

char *
my_sbrk(int incr)
{
    char               *temp, *ptr;
    kern_return_t       rtn;

    if (mach_brkpt == 0) {
	if ((rtn = vm_allocate(task_self(), (vm_address_t *) & mach_brkpt,
			       big_heap, 1)) != KERN_SUCCESS) {
	    mach_error("my_sbrk(): vm_allocate() failed", rtn);
	    return ((char *)-1);
	}
	mach_maplimit = mach_brkpt + big_heap;
    }
    if (incr == 0) {
	return (mach_brkpt);
    } else {
	ptr = mach_brkpt + incr;
	if (ptr <= mach_maplimit) {
	    temp = mach_brkpt;
	    mach_brkpt = ptr;
	    return (temp);
	} else {
	    fprintf(stderr, "my_sbrk(): no more memory\n");
	    fflush(stderr);
	    return ((char *)-1);
	}
    }
}

static void
grow(
     struct load_command ***the_commands,
     unsigned *the_commands_len
     )
{
	if (*the_commands == NULL) {
		*the_commands_len = 1;
		*the_commands = malloc(sizeof(*the_commands));
	} else {
		(*the_commands_len)++;
		*the_commands = realloc(*the_commands, 
					(*the_commands_len *
					 sizeof(**the_commands)));
	}
}


static void
save_command(
	     struct load_command *command,
	     struct load_command ***the_commands,
	     unsigned *the_commands_len
	     )
{
	struct load_command **tmp;

	grow(the_commands, the_commands_len);
	tmp = &(*the_commands)[*the_commands_len - 1];
	*tmp = malloc(command->cmdsize);
	bcopy(command, *tmp, command->cmdsize);
}

static void
fatal_unexec(char *format, ...)
{
	va_list ap;

	va_start(ap, format);
	fprintf(stderr, "unexec: ");
	vfprintf(stderr, format, ap);
	fprintf(stderr, "\n");
	va_end(ap);
}

static int
read_macho(
	   int fd,
	   struct mach_header *the_header,
	   struct load_command ***the_commands,
	   unsigned *the_commands_len
	   )
{
	struct load_command command;
	struct load_command *buf;
	int i;
	int size;

	if (read(fd, the_header, sizeof(*the_header)) != sizeof(*the_header)) {
		fatal_unexec("cannot read macho header");
		return (0);
	}
	for (i = 0; i < the_header->ncmds; i++) {
		if (read(fd, &command, sizeof(struct load_command)) != 
		    sizeof(struct load_command)) {
		  	fatal_unexec("cannot read macho load command header");
			return (0);
		}
		size = command.cmdsize - sizeof(struct load_command);
		if (size < 0) {
		  	fatal_unexec("bogus load command size");
			return (0);
		}
		buf = malloc(command.cmdsize);
		buf->cmd = command.cmd;
		buf->cmdsize = command.cmdsize;
		if (read(fd, ((char *)buf + 
			      sizeof(struct load_command)), 
			 size) != size) {
		  	fatal_unexec("cannot read load command data");
			return (0);
		}
		save_command(buf, the_commands, the_commands_len);
	}
	return (1);
}

static int
filldatagap(
	    vm_address_t start_address,
	    vm_size_t *size,
	    vm_address_t end_address
	    )
{
	vm_address_t address;
	vm_size_t gapsize;

	address = (start_address + *size);
	gapsize = end_address - address;
	*size += gapsize;
	if (vm_allocate(task_self(), &address, gapsize,
			FALSE) != KERN_SUCCESS) {
		fatal_unexec("cannot vm_allocate");
	        return (0);
	}
	return (1);
}

static int
get_data_region(
		vm_address_t *address,
		vm_size_t *size
		)
{
	region_t region;
	kern_return_t ret;
	struct section *sect;

	sect = (struct section *) getsectbyname(SEG_DATA, SECT_DATA);
	region.address = 0;
	*address = 0;
	for (;;) {
		ret = vm_region(task_self(), 
				&region.address, 
				&region.size, 
				&region.protection, 
				&region.max_protection, 
				&region.inheritance,
				&region.shared, 
				&region.object_name, 
				&region.offset);
		if (ret != KERN_SUCCESS || region.address >= mach_maplimit) {
			break;
		}
		if (*address != 0) {
			if (region.address > *address + *size) {
				if (!filldatagap(*address, size, 
						 region.address)) {
					return (0);
				}
			} 
			*size += region.size;
		} else {
			if (region.address == sect->addr) {
				*address = region.address;
				*size = region.size;
			} 
		}
		region.address += region.size;
	}
	return (1);
}

static char *
my_malloc(
	  vm_size_t size
	  )
{
	vm_address_t address;

	if (vm_allocate(task_self(), &address, size, TRUE) != KERN_SUCCESS) {
		return (NULL);
	}
	return ((char *)address);
}

static void
my_free(
	char *buf,
	vm_size_t size
	)
{
	vm_deallocate(task_self(), (vm_address_t)buf, size);
}

static int
unexec_doit(
	    int infd,
	    int outfd
	    )
{
	int i;
	struct load_command **the_commands = NULL;
	unsigned the_commands_len;
	struct mach_header the_header;
	int fgrowth;
	int fdatastart;
	int fdatasize;
	int size;
	int seg;
	struct stat st;
	char *buf;
	vm_address_t data_address;
	vm_size_t data_size, bss_size;

	struct segment_command *segment;
	struct section *section;

	if (!read_macho(infd, &the_header, &the_commands, &the_commands_len)) {
		return (0);
	}

	if (!get_data_region(&data_address, &data_size)) {
		return (0);
	}


	/*
	 * DO NOT USE MALLOC IN THIS SECTION
	 */
	{
		/*
		 * Fix offsets
		 */
		for (i = 0; i < the_commands_len; i++) {
			switch (the_commands[i]->cmd) {
			case LC_SEGMENT:
				segment = ((struct segment_command *)
					   the_commands[i]);
				if (strcmp(segment->segname, SEG_DATA) == 0) {
					data_size = CEIL(mach_brkpt - data_address, getpagesize());
					bss_size = mach_maplimit - mach_brkpt;
					fdatastart = segment->fileoff;
					fdatasize = segment->filesize;
					fgrowth = (data_size - 
						   segment->filesize);
					segment->vmsize = data_size + bss_size;
					segment->filesize = data_size;
					section = (struct section *) ((char *) (segment + 1));
					for (seg = 0; seg < segment->nsects; ++seg, ++section) {
						if (strcmp(section->sectname, SECT_DATA) == 0) {
						        section->size = data_size;
						}
						else if (strcmp(section->sectname, SECT_BSS) == 0) {
						        section->addr = data_address + data_size;
						        section->size = bss_size;
							section->flags = S_ZEROFILL;
					}
						else if (strcmp(section->sectname, SECT_COMMON) == 0) {
						        section->addr = data_address + data_size + bss_size;
					}
					      }
				}
                                if (strcmp(segment->segname, SEG_LINKEDIT) == 0) {
                                        segment->vmaddr += CEIL(fgrowth + bss_size, getpagesize());
                                        segment->fileoff += fgrowth;
				}
				break;
			case LC_SYMTAB:
				((struct symtab_command *)
				 the_commands[i])->symoff += fgrowth;
				((struct symtab_command *)
				 the_commands[i])->stroff += fgrowth;
				break;
			case LC_SYMSEG:
				((struct symseg_command *)
				 the_commands[i])->offset += fgrowth;
				break;
			default:
				break;
			}
		}
		
		/*
		 * Write header
		 */
		if (write(outfd, &the_header, 
			  sizeof(the_header)) != sizeof(the_header)) {
			fatal_unexec("cannot write header");
			return (0);
		}
		
		/*
		 * Write commands
		 */
		for (i = 0; i < the_commands_len; i++) {
			if (write(outfd, the_commands[i], 
				  the_commands[i]->cmdsize) != 
			    the_commands[i]->cmdsize) {
			  	fatal_unexec("cannot write commands");
				return (0);
			}
		}
		
		/*
		 * Write original text
		 */
		if (lseek(infd, the_header.sizeofcmds + sizeof(the_header), 
			  L_SET) < 0) {
		  	fatal_unexec("cannot seek input file");
			return (0);
		}
		size = fdatastart - (sizeof(the_header) + 
				     the_header.sizeofcmds);
		buf = my_malloc(size);
		if (read(infd, buf, size) != size) {
			my_free(buf, size);
		  	fatal_unexec("cannot read input file");
		}
		if (write(outfd, buf, size) != size) {
			my_free(buf, size);
			fatal_unexec("cannot write original text");
			return (0);
		}
		my_free(buf, size);
		
		
		/*
		 * Write new data
		 */
		if (write(outfd, (char *)data_address, 
			  data_size) != data_size) {
			fatal_unexec("cannot write new data");
			return (0);
		}
		
	}

	/*
	 * OKAY TO USE MALLOC NOW
	 */

	/*
	 * Write rest of file
	 */
	fstat(infd, &st);
	if (lseek(infd, fdatasize, L_INCR) < 0) {
		fatal_unexec("cannot seek input file");
		return (0);
	}
	size = st.st_size - lseek(infd, 0, L_INCR);

	buf = malloc(size);
	if (read(infd, buf, size) != size) {
		free(buf);
		fatal_unexec("cannot read input file");
		return (0);
	}
	if (write(outfd, buf, size) != size) {
		free(buf);
		fatal_unexec("cannot write reset of file");
		return (0);
	}
	free(buf);
	return (1);
}

void
unexec(
       char *outfile,
       char *infile
       )
{
	int infd;
	int outfd;
	char tmpbuf[L_tmpnam];
	char *tmpfile;

	infd = open(infile, O_RDONLY, 0);
	if (infd < 0) {
	  	fatal_unexec("cannot open input file `%s'", infile);
		exit(1);
	}
	
	tmpnam(tmpbuf);
	tmpfile = rindex(tmpbuf, '/');
	if (tmpfile == NULL) {
		tmpfile = tmpbuf;
	} else {
		tmpfile++;
	}
	outfd = open(tmpfile, O_WRONLY|O_TRUNC|O_CREAT, 0755);
	if (outfd < 0) {
		close(infd);
		fatal_unexec("cannot open tmp file `%s'", tmpfile);
		exit(1);
	}
	if (!unexec_doit(infd, outfd)) {
		close(infd);
		close(outfd);
		unlink(tmpfile);
		exit(1);
	}
	close(infd);
	close(outfd);
	if (rename(tmpfile, outfile) < 0) {
		unlink(tmpfile);
		fatal_unexec("cannot rename `%s' to `%s'", tmpfile, outfile);
		exit(1);
	}
}
