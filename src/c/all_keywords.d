/*
    all_keywords.d -- All named keywords.
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecls.h"
#include "page.h"

const struct keyword_info all_keywords[] = {
/* bind.c */
{&Kallow_other_keys, "ALLOW-OTHER-KEYS"},

/* error.c */
{&Kpathname, "PATHNAME"},
{&Kdatum, "DATUM"},
{&Kexpected_type, "EXPECTED-TYPE"},
{&Kformat_control, "FORMAT-CONTROL"},
{&Kformat_arguments, "FORMAT-ARGUMENTS"},

/* file.c */
{&Kerror, "ERROR"},
{&Kabort, "ABORT"},
{&Kdirection, "DIRECTION"},
{&Kinput, "INPUT"},
{&Koutput, "OUTPUT"},
{&Kio, "IO"},
{&Kprobe, "PROBE"},
{&Kelement_type, "ELEMENT-TYPE"},
{&Kdefault, "DEFAULT"},
{&Kif_exists, "IF-EXISTS"},
{&Knew_version, "NEW-VERSION"},
{&Krename, "RENAME"},
{&Krename_and_delete, "RENAME-AND-DELETE"},
{&Koverwrite, "OVERWRITE"},
{&Kappend, "APPEND"},
{&Ksupersede, "SUPERSEDE"},
{&Kcreate, "CREATE"},
{&Kprint, "PRINT"},
{&Kif_does_not_exist, "IF-DOES-NOT-EXIST"},
{&Kset_default_pathname, "SET-DEFAULT-PATHNAME"},

/* hash.c */
{&Ksize, "SIZE"},
{&Krehash_size, "REHASH-SIZE"},
{&Krehash_threshold, "REHASH-THRESHOLD"},

/* list.c */
{&Ktest, "TEST"},
{&Ktest_not, "TEST-NOT"},
{&Kkey, "KEY"},
{&Kinitial_element, "INITIAL-ELEMENT"},

/* load.c */
{&Kverbose, "VERBOSE"},

/* package.c */
{&Kinternal, "INTERNAL"},
{&Kexternal, "EXTERNAL"},
{&Kinherited, "INHERITED"},
{&Knicknames, "NICKNAMES"},
{&Kuse, "USE"},

/* pathname.c */
{&Kwild, "WILD"},
{&Kwild_inferiors, "WILD-INFERIORS"},
{&Knewest, "NEWEST"},
{&Khost, "HOST"},
{&Kdevice, "DEVICE"},
{&Kdirectory, "DIRECTORY"},
{&Kname, "NAME"},
{&Ktype, "TYPE"},
{&Kversion, "VERSION"},
{&Kdefaults, "DEFAULTS"},
{&Kabsolute, "ABSOLUTE"},
{&Krelative, "RELATIVE"},
{&Kup, "UP"},

/* print.c */
{&Kupcase, "UPCASE"},
{&Kdowncase, "DOWNCASE"},
{&Kcapitalize, "CAPITALIZE"},
{&Kstream, "STREAM"},
{&Kescape, "ESCAPE"},
{&Kpretty, "PRETTY"},
{&Kcircle, "CIRCLE"},
{&Kbase, "BASE"},
{&Kradix, "RADIX"},
{&Kcase, "CASE"},
{&Kgensym, "GENSYM"},
{&Klevel, "LEVEL"},
{&Klength, "LENGTH"},
{&Karray, "ARRAY"},

/* read.c */
{&Kjunk_allowed, "JUNK-ALLOWED"},

/* stacks.d */
{&Kcatch, "CATCH"},
{&Kcatchall, "CATCHALL"},
{&Kprotect, "PROTECT"},

/* string.c */
{&Kstart1, "START1"},
{&Kend1, "END1"},
{&Kstart2, "START2"},
{&Kend2, "END2"},
{&Kstart, "START"},
{&Kend, "END"},

/* toplevel */
{&Kexecute, "EXECUTE"},
{&Kcompile_toplevel, "COMPILE-TOPLEVEL"},
{&Kload_toplevel, "LOAD-TOPLEVEL"},

/* unixfsys.c */
{&Klist_all, "LIST-ALL"},

/* END */
{NULL, (const char*)NULL}};

void
init_all_keywords(void)
{
  const struct keyword_info *k = all_keywords;
  cl_object *keyword_loc;

  while (k->name != NULL) {
    keyword_loc = k->loc;
    *keyword_loc = make_keyword(k->name);
    k++;
  }
}
