/*
    unixfsys.c  -- Unix file system interface.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>
#include <sys/stat.h>
#include "ecl.h"
#ifdef BSD
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

cl_object @':list-all';

/*
 * Interprets an error code from the C library according to the POSIX
 * standard, and produces a suitable error message by combining the user
 * supplied format with an explanation of the cause of the error.
 */
void
FEfilesystem_error(const char *msg, int narg, ...)
{
	va_list args;
	cl_object rest;
	const char *extra_msg;

	va_start(args, narg);
	rest = va_grab_rest_args(narg, args);
	va_end(args);

	switch (errno) {
	case EPERM:
	case EACCES:
	case EROFS:
		extra_msg = "Insufficient permissions";
		break;
	case EEXIST:
		extra_msg = "Already exists";
		break;
	case ENAMETOOLONG:
		extra_msg = "File or directory name too long";
		break;
	case ENOENT:
	case ENOTDIR:
	case ELOOP:
		extra_msg = "Invalid or not existent path";
		break;
	case ENOSPC:
		extra_msg = "Not enough space or quota exceeded";
		break;
	default:
		extra_msg = "Uknown reason";
		break;
	}
	FEerror("~?~%Explanation: ~A.", 3, make_constant_string(msg), rest,
		make_constant_string(extra_msg));
}

/*
 * string_to_pathanme, to be used when s is a real pathname
 */
cl_object
string_to_pathname(char *s)
{
  cl_index e;
  return parse_namestring(s, 0, strlen(s), &e, Cnil);
}

/*
 * Finds current directory by using getcwd() with an adjustable
 * string which grows until it can host the whole path.
 */
static cl_object
current_dir(void) {
	cl_object output;
	const char *ok;
	cl_index size = 128;

	do {
	  output = cl_alloc_adjustable_string(size);
	  ok = getcwd(output->string.self, size);
	  size += 256;
	} while(ok == NULL);
	size = strlen(output->string.self);
	if ((size + 1 /* / */ + 1 /* 0 */) >= output->string.dim) {
	  /* Too large to host the trailing '/' */
	  cl_object other = cl_alloc_adjustable_string(size+2);
	  strcpy(other->string.self, output->string.self);
	  output = other;
	}
	output->string.self[size++] = '/';
	output->string.self[size] = 0;
	output->string.fillp = size;
	return output;
}

/*
 * Using a certain path, guess the type of the object it points to.
 */

enum file_system_type {
  FILE_DOES_NOT_EXIST = 0,
  FILE_REGULAR = 1,
  FILE_DIRECTORY = 2,
  FILE_OTHER = 3
};

static int
get_file_system_type(const char *namestring) {
  struct stat buf;

  if (stat(namestring, &buf) < 0)
    return FILE_DOES_NOT_EXIST;
  if (S_ISREG(buf.st_mode))
    return FILE_REGULAR;
  if (S_ISDIR(buf.st_mode))
    return FILE_DIRECTORY;
  return FILE_OTHER;
}


/*
 * Search the actual name of the directory of a pathname,
 * going through links if they exist. Default is
 * current directory
 */
static cl_object
error_no_dir(cl_object pathname) {
	FEfilesystem_error("truedirectory: ~S cannot be accessed", 1, pathname);
	return Cnil;
}

cl_object
truedirectory(cl_object pathname)
{
	cl_object directory;

	directory = current_dir();
	if (pathname->pathname.directory != Cnil) {
	  cl_object dir = pathname->pathname.directory;
	  if (CAR(dir) == @':absolute')
	    chdir("/");
	  for (dir=CDR(dir); !Null(dir); dir=CDR(dir)) {
	    cl_object name = CAR(dir);
	    if (name == @':up') {
	      if (chdir("..") < 0)
		return error_no_dir(pathname);
	    } else if (type_of(name) == t_string) {
	      if (chdir(name->string.self) < 0)
		return error_no_dir(pathname);
	    } else
	      FEerror("truename: ~A not allowed in filename",1,name);
	  }
	  dir = current_dir();
	  chdir(directory->string.self);
	  directory = dir;
	}
	return directory;
}

cl_object
truename(cl_object pathname)
{
	cl_object directory;
	cl_object truefilename;

	pathname = coerce_to_file_pathname(pathname);

	/* We are looking for a file! */
	if (pathname->pathname.name == Cnil)
	  FEerror("truename: no file name supplied",0);

	/* Wildcards are not allowed */
	if (pathname->pathname.name == @':wild' ||
	    pathname->pathname.type == @':wild')
	  FEerror("truename: :wild not allowed in filename",0);

	directory = truedirectory(pathname);

	/* Compose a whole pathname by adding the
	   file name and the file type */
	if (Null(pathname->pathname.type)) 
	  truefilename = @si::string-concatenate(2, directory, pathname->pathname.name);
	else {
	  truefilename = @si::string-concatenate(4, directory,
				pathname->pathname.name,
				make_simple_string("."),
				pathname->pathname.type);
	}

	/* Finally check that the object exists and it is
	   either a file or a device. (FIXME! Should we
	   reject devices, pipes, etc?) */
	switch (get_file_system_type(truefilename->string.self)) {
	case FILE_DOES_NOT_EXIST:
	  FEerror("truename: file does not exist or cannot be accessed",1,pathname);
	  return OBJNULL;
	case FILE_DIRECTORY:
	  FEerror("truename: ~A is a directory", 1, truefilename);
	  return OBJNULL;
	default:
	  return coerce_to_pathname(truefilename);
	}
}

bool
file_exists(cl_object file)
{
	struct stat filestatus;

	file = coerce_to_filename(file);
	if (stat(file->string.self, &filestatus) >= 0)
		return(TRUE);
	else
		return(FALSE);
}

FILE *
backup_fopen(const char *filename, const char *option)
{
	char backupfilename[MAXPATHLEN];
	char command[MAXPATHLEN * 2];

	strcat(strcpy(backupfilename, filename), ".BAK");
	sprintf(command, "mv %s %s", filename, backupfilename);
	system(command);
	return(fopen(filename, option));
}

int
file_len(FILE *fp)
{
	struct stat filestatus;

	fstat(fileno(fp), &filestatus);
	return(filestatus.st_size);
}

@(defun truename (file)
@
	/* INV: truename() checks type of file */
	@(return truename(file))
@)

@(defun rename_file (oldn newn)
	cl_object filename, newfilename, old_truename, new_truename;
@
	/* INV: coerce_to_file_pathname() checks types */
	oldn = coerce_to_file_pathname(oldn);
	newn = coerce_to_file_pathname(newn);
	newn = merge_pathnames(newn, oldn, Cnil);
	old_truename = truename(oldn);
	filename = coerce_to_filename(oldn);
	newfilename = coerce_to_filename(newn);
	if (rename(filename->string.self, newfilename->string.self) < 0)
		FEfilesystem_error("Cannot rename the file ~S to ~S.", 2,
				   oldn, newn);
	new_truename = truename(newn);
	@(return newn old_truename new_truename)
@)

@(defun delete_file (file)
	cl_object filename;
@
	/* INV: coerce_to_filename() checks types */
	filename = coerce_to_filename(file);
	if (unlink(filename->string.self) < 0)
		FEfilesystem_error("Cannot delete the file ~S.", 1, file);
	@(return Ct)
@)

@(defun probe_file (file)
@
	/* INV: file_exists() and truename() check types */
	@(return (file_exists(file)? truename(file) : Cnil))
@)

@(defun file_write_date (file)
	cl_object filename, time;
	struct stat filestatus;
@
	/* INV: coerce_to_filename() checks types */
	filename = coerce_to_filename(file);
	if (stat(filename->string.self, &filestatus) < 0)
	  time = Cnil;
	else
	  time = UTC_time_to_universal_time(filestatus.st_mtime);
	@(return time)
@)

@(defun file_author (file)
	cl_object filename;
	struct stat filestatus;
	struct passwd *pwent;
#ifndef __STDC__
	extern struct passwd *getpwuid(uid_t);
#endif
@
	/* INV: coerce_to_filename() checks types */
	filename = coerce_to_filename(file);
	if (stat(filename->string.self, &filestatus) < 0)
		FEfilesystem_error("Cannot get the file status of ~S.", 1,
				   file);
	pwent = getpwuid(filestatus.st_uid);
	@(return make_string_copy(pwent->pw_name))
@)

const char *
expand_pathname(const char *name)
{
  const char *path, *p;
  static char pathname[255], *pn;

  if (IS_DIR_SEPARATOR(name[0])) return(name);
  if ((path = getenv("PATH")) == NULL) error("No PATH in environment");
  p = path;
  pn = pathname;
  do {
    if ((*p == '\0') || (*p == PATH_SEPARATOR)) {
      if (pn != pathname) *pn++ = DIR_SEPARATOR; /* on SYSV . is empty */
LAST: strcpy(pn, name);
      if (access(pathname, X_OK) == 0)
	return (pathname);
      pn = pathname;
      if (p[0] == PATH_SEPARATOR && p[1] == '\0') { /* last entry is empty */
	p++;
	goto LAST;
      }
    }
    else
      *pn++ = *p;
  } while (*p++ != '\0');
  return(name);			/* should never occur */
}

cl_object
homedir_pathname(cl_object user)
{
	cl_index i;
	char *p, filename[MAXPATHLEN];
	struct passwd *pwent = NULL;
#ifndef __STDC__
	extern struct passwd *getpwuid(uid_t), *getpwnam();
#endif
	
	if (Null(user))
		pwent = getpwuid(getuid());
	else {
		user = coerce_to_string(user);
		p = user->string.self;
		i = user->string.fillp;
		if (i > 0 && *p == '~') {
			p++;
			i--;
		}
		if (i == 0)
			pwent = getpwuid(getuid());
		else {
			strncpy(filename, p, i);
			filename[i] = '\0';
			pwent = getpwnam(filename);
		}
	}
	if (pwent == NULL)
		FEerror("Unknown user ~S.", 1, user);
	strcpy(filename, pwent->pw_dir);
	i = strlen(filename);
	if (i == 0 || filename[i-1] != '/') {
		filename[i++] = '/';
		filename[i] = '\0';
	}
	return string_to_pathname(filename);
}

@(defun user_homedir_pathname (&optional host)
	cl_object pathname;
@
	/* Ignore optional host argument. */
#ifdef MSDOS
	{ extern char *getenv();
	  char *h = getenv("HOME");
	  pathname = (h == NULL)? make_simple_string("/")
				: make_string_copy(h);
	}
#else
	pathname = homedir_pathname(Cnil);
#endif /* MSDOS */
	@(return pathname)
@)

/*
 * Take two C strings and check if the first one matches
 * against the pattern given by the second one. The pattern
 * is that of a Unix shell except for brackets and curly
 * braces
 */
static bool
string_match(const char *s, const char *p) {
	const char *next;
	while (*s) {
	  switch (*p) {
	  case '*':
	    /* Match any group of characters */
	    next = p+1;
	    if (*next != '?') {
	      if (*next == '\\')
		next++;
	      while (*s && *s != *next) s++;
	    }
	    if (string_match(s,next))
	      return TRUE;
	    /* starts back from the '*' */
	    if (!*s)
	      return FALSE;
	    s++;
	    break;
	  case '?':
	    /* Match any character */
	    s++, p++;
	    break;
	  case '\\':
      /* Interpret a pattern character literally.
         Trailing slash is interpreted as a slash. */
	    if (p[1]) p++;
	    if (*s != *p)
	      return FALSE;
	    s++, p++;
	    break;
	  default:
	    if (*s != *p)
	      return FALSE;
	    s++, p++;
	    break;
	  }
	}
	while (*p == '*')
	  p++;
	return (*p == 0);
}

@(defun si::string_match (s1 s2)
@
	assert_type_string(s1);
	assert_type_string(s2);
	@(return (string_match(s1->string.self, s2->string.self) ? Ct : Cnil))
@)

static cl_object
actual_directory(cl_object namestring, cl_object mask, bool all)
{
	cl_object ret = Cnil;
	cl_object saved_dir = current_dir();
	cl_object *directory = &ret;
	cl_object dir_path = coerce_to_file_pathname(namestring);
	enum file_system_type t;
#if defined(BSD)
/*
 * version by Brian Spilsbury <brian@@bizo.biz.usyd.edu.au>, using opendir()
 * arranged by Juan Jose Garcia Ripoll to understand masks
 */
	DIR *dir;
	struct dirent *entry;

	if (chdir(namestring->string.self) < 0) {
	  chdir(saved_dir->string.self);
	  FEfilesystem_error("directory: cannot access ~A", 1, namestring);
	}
	dir = opendir(".");
	if (dir == NULL) {
	  chdir(saved_dir->string.self);
	  FEfilesystem_error("Can't open the directory ~S.", 1, dir);
	}

	while ((entry = readdir(dir))) {
	  t = (enum file_system_type)get_file_system_type(entry->d_name);
	  if ((all || t == FILE_REGULAR) &&
	      string_match(entry->d_name, mask->string.self))
	    {
	      cl_index e = strlen(entry->d_name);
	      cl_object file = parse_namestring(entry->d_name, 0, e, &e, Cnil);
	      file = merge_pathnames(dir_path, file,Cnil);
	      *directory = CONS(file, Cnil);
	      directory = &CDR(*directory);
	    }
	}
	closedir(dir);
#endif
#if defined(SYSV)
	FILE *fp;
	char iobuffer[BUFSIZ];
	DIRECTORY dir;

	if (chdir(namestring->string.self) < 0) {
	  chdir(saved_dir->string.self);
	  FEfilesystem_error("directory: cannot access ~A",1,namestring);
	}
	fp = fopen(".", OPEN_R);
	if (fp == NULL) {
	  chdir(saved_dir->string.self);
	  FEfilesystem_error("Can't open the directory ~S.", 1, dir);
	}

	setbuf(fp, iobuffer);
	/* FIXME! What are these three lines for? */
	fread(&dir, sizeof(DIRECTORY), 1, fp);
	fread(&dir, sizeof(DIRECTORY), 1, fp);
	for (;;) {
	  if (fread(&dir, sizeof(DIRECTORY), 1, fp) <= 0)
	    break;
	  if (dir.d_ino == 0)
	    continue;
	  t = get_file_system_type(dir.d_name);
	  if ((all || t == FILE_REGULAR) &&
	      string_match(dir.d_name, mask->string.self))
	    {
	      int e = strlen(dir.d_name);
	      cl_object file = parse_namestring(dir.d_name, 0, e, &e);
	      file = merge_pathnames(dir_path, file,Cnil);
	      *directory = CONS(file, Cnil);
	      directory = &CDR(*directory);
	    }
	}
	fclose(fp);
#endif
	chdir(saved_dir->string.self);
	return ret;
}

@(defun directory (&optional (filemask OBJNULL)
			     (kall OBJNULL))
	cl_object directory;
	cl_object name, type, mask;
	bool all = FALSE;
@
	/* Without arguments, it justs lists all files in
	   current directory */
	if (filemask == OBJNULL) {
	  directory = current_dir();
	  mask = make_simple_string("*");
	  goto DO_MATCH;
	}

	if (kall == @':list-all')
	  all = TRUE;
	else if (kall != OBJNULL)
	  FEwrong_type_argument(@'keyword', kall);

	/* INV: coerce_to_file_pathname() checks types */
	filemask = coerce_to_file_pathname(filemask);
	name = filemask->pathname.name;
	type = filemask->pathname.type;

	directory = truedirectory(filemask);

	if (name == @':wild')
	  name = make_simple_string("*");
	else if (name == Cnil) {
	  if (type == Cnil)
	    name = make_simple_string("*");
	  else
	    name = null_string;
	}

	if (type == Cnil)
	  mask = name;
	else {
	  cl_object dot = make_simple_string(".");
	  if (type == @':wild')
	    type = make_simple_string("*");
	  mask = @si::string-concatenate(3, name, dot, type);
	}
 DO_MATCH:
	@(return actual_directory(directory, mask, all))
@)

@(defun si::chdir (directory)
	cl_object filename, previous;
@
	/* INV: coerce_to_filename() checks types */
	filename = coerce_to_filename(directory);
	previous = current_dir();
	if (chdir(filename->string.self) < 0) {
		FEfilesystem_error("Can't change the current directory to ~S",
				   1, filename);
	}
	@(return previous)
@)

@(defun si::mkdir (directory mode)
	cl_object filename;
	int modeint;
@
	/* INV: coerce_to_filename() checks types */
	filename = coerce_to_filename(directory);
	modeint = fixnnint(mode);
	if (mkdir(filename->string.self, modeint) < 0) {
		FEfilesystem_error("Could not create directory ~S", 1,
				   filename);
	}
	@(return filename)
@)

#ifdef sun4sol2
/* These functions can't be used with static linking on Solaris */
struct passwd *
getpwnam(const char *name)
{
  FEerror("~~ expansion not supported on Solaris.", 0);
}
struct passwd *
getpwuid(uid_t uid)
{
  FEerror("ECL can't use getpwuid on Solaris.", 0);
}
#endif
