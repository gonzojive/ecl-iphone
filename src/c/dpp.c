/*
    dpp.c -- Defun preprocessor.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


/*
	Usage:
		dpp [in-file [out-file]]

	The file named in-file is preprocessed and the output will be
	written to the file whose name is out-file. If in-file is "-"
	program is read from standard input, while if out-file is "-"
	C-program is written to standard output.


	The function definition:

	@(defun name ({var}*
		      [&optional {var | (var [initform [svar]])}*]
		      [&rest var]
		      [&key {var |
			     ({var | (keyword var)} [initform [svar]])}*
			    [&allow_other_keys]]
		      [&aux {var | (var [initform])}*])

		C-declaration

	@

		C-body

	@)

	name can be either an identifier or a full C procedure header
	enclosed in quotes (').

	&optional may be abbreviated as &o.
	&rest may be abbreviated as &r.
	&key may be abbreviated as &k.
	&allow_other_keys may be abbreviated as &aok.
	&aux may be abbreviated as &a.

	Each variable becomes a C variable.

	Each supplied-p parameter becomes a boolean C variable.

	Initforms are C expressions.
	It an expression contain non-alphanumeric characters,
	it should be surrounded by backquotes (`).


	Function return:

		@(return {form}*)

*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define POOLSIZE        2048
#define MAXREQ          16
#define MAXOPT          16
#define MAXKEY          16
#define MAXAUX          16
#define MAXRES          16

#define TRUE            1
#define FALSE           0

typedef int bool;

FILE *in, *out;

char filename[BUFSIZ];
int lineno;
int tab;
int tab_save;

char pool[POOLSIZE];
char *poolp;

char *function;

char *required[MAXREQ];
int nreq;

struct optional {
	char *o_var;
	char *o_init;
	char *o_svar;
} optional[MAXOPT];
int nopt;

bool rest_flag;
char *rest_var;

bool key_flag;
struct keyword {
	char *k_key;
	char *k_var;
	char *k_init;
	char *k_svar;
} keyword[MAXKEY];
int nkey;
bool allow_other_keys_flag;

struct aux {
	char *a_var;
	char *a_init;
} aux[MAXAUX];
int naux;

char *result[MAXRES];
int nres;

put_lineno(void)
{
	static int flag = 0;
	if (flag)
		fprintf(out, "#line %d\n", lineno);
	else {
		flag++;
		fprintf(out, "#line %d \"%s\"\n", lineno, filename);
	}
}

error(char *s)
{
	printf("Error in line %d: %s.\n", lineno, s);
	exit(1);
}

error_symbol(char *s)
{
	printf("Error in line %d: illegal symbol %s.\n", lineno, s);
	exit(1);
}

readc(void)
{
	int c;

	c = getc(in);
	if (feof(in)) {
		if (function != NULL)
			error("unexpected end of file");
		exit(0);
	}
	if (c == '\n') {
		lineno++;
		tab = 0;
	} else if (c == '\t')
		tab++;
	return(c);
}

nextc(void)
{
	int c;

	while (isspace(c = readc()))
		;
	return(c);
}

unreadc(int c)
{
	if (c == '\n')
		--lineno;
	else if (c == '\t')
		--tab;
	ungetc(c, in);
}

put_tabs(int n)
{
	put_lineno();
	while (n--)
		putc('\t', out);
}

pushc(int c)
{
	if (poolp >= &pool[POOLSIZE])
		error("buffer pool overflow");
	*poolp++ = c;
}

char *
read_name(int is_symbol)
{
	int c, l;
	int oneX = 0, oneC = 0;
	char *colon = NULL;
	char *p;

	c = readc();
	while (isspace(c)) {
		pushc(c);
		c = readc();
	}
	p = poolp;
	do {
		if (isalpha(c))
			; /* c=tolower(c) */
		else if (isnumber(c))
			;
		else if (c == '-' || c == '_')
			c = '_';
		else if (c == '&')
			c = 'A';
		else if (c == '*') {
			if (is_symbol && !oneX && (poolp == colon || poolp == p))
			{
				if (poolp > p && poolp[-1] == 'S')
					poolp--;
				oneX = 1;
				c = 'V';
			} else {
				c = 'X';
			}
		} else if (c == '+') {
			if (is_symbol && !oneC && (poolp == colon || poolp == p))
			{
				if (poolp > p && poolp[-1] == 'S')
					poolp--;
				oneC = 1;
				c = 'C';
			} else {
				c = 'P';
			}
		} else if (c == '<') {
			c = 'L';
		} else if (c == '>') {
			c = 'G';
		} else if (c == '=') {
			c = 'E';
		} else if (c == '/') {
			c = 'N';
		} else if (c == ':') {
			if (colon == poolp) {
				c = readc();
				continue;
			} else if (colon != NULL)
				error("double colon ':' in symbol name");
			else {
				colon = poolp+1;
				if (poolp == p)
					c = 'K';	/* Keyword */
				else if (!is_symbol)
					c = 'L';	/* Function name */
				else if (oneX == NULL)
					c = 'S';	/* Symbol name */
				else {
					c = readc();
					continue;
				}
			}
		} else if (!is_symbol) {
			unreadc(c);
			break;
		} else if (c == '\'') {
			break;
		} else {
			error("Disallowed character in symbol name");
		}
		pushc(c);
		c = readc();
	} while (1);
	l = poolp - p;
	if (l > 2 && oneX && poolp[-1] == 'X')
		poolp--;
	if (l > 2 && oneC && poolp[-1] == 'P')
		poolp--;
	if (colon == NULL) {
		char buf[256];
		poolp[0] = buf[0] = '\0';
		strcpy(buf, "cl");
		if (!oneX && p[0] != 'K')
		  strcat(buf, is_symbol? "S" : "L");
		strcat(buf, p);
		strcpy(p, buf);
		poolp = p + strlen(buf);
	}
	return p;
}

char *
read_symbol_name(void)
{
	return read_name(1);
}

char *
read_func_name(void)
{
	return read_name(0);
}

char *
read_token(void)
{
	int c;
	int left_paren = 0;
	char *p;

	p = poolp;
	c = readc();
	while (isspace(c))
		c = readc();
	do {
		if (c == '(') {
			left_paren++;
			pushc(c);
		} else if (c == ')') {
			if (left_paren == 0) {
				break;
			} else {
				left_paren--;
				pushc(c);
			}
		} else if (isspace(c) && left_paren == 0) {
			do
				c = readc();
			while (isspace(c));
			break;
		} else if (c == '@') {
			c = readc();
			if (c == '\'') {
				(void)read_symbol_name();
			} else if (c == '@') {
				pushc(c);
			} else {
				unreadc(c);
				(void)read_func_name();
			}
		} else {
			pushc(c);
		}
		c = readc();
	} while (1);
	unreadc(c);
	pushc('\0');
	return(p);
}

reset(void)
{
	int i;

	poolp = pool;
	function = NULL;
	nreq = 0;
	for (i = 0;  i < MAXREQ;  i++)
		required[i] = NULL;
	nopt = 0;
	for (i = 0;  i < MAXOPT;  i++)
		optional[i].o_var
		= optional[i].o_init
		= optional[i].o_svar
		= NULL;
	rest_flag = FALSE;
	rest_var = "ARGS";
	key_flag = FALSE;
	nkey = 0;
	for (i = 0;  i < MAXKEY;  i++)
		keyword[i].k_key
		= keyword[i].k_var
		= keyword[i].k_init
		= keyword[i].k_svar
		= NULL;
	allow_other_keys_flag = FALSE;
	naux = 0;
	for (i = 0;  i < MAXAUX;  i++)
		aux[i].a_var
		= aux[i].a_init
		= NULL;
}

get_function(void)
{
	function = read_func_name();
	pushc('\0');
}

get_lambda_list(void)
{
	int c;
	char *p;

	if ((c = nextc()) != '(')
		error("( expected");
	for (;;) {
		if ((c = nextc()) == ')')
			return;
		if (c == '&') {
			p = read_token();
			goto OPTIONAL;
		}
		unreadc(c);
		p = read_token();
		if (nreq >= MAXREQ)
			error("too many required variables");
		required[nreq++] = p;
	}

OPTIONAL:
	if (strcmp(p, "optional") != 0 && strcmp(p, "o") != 0)
		goto REST;
	for (;;  nopt++) {
		if ((c = nextc()) == ')')
			return;
		if (c == '&') {
			p = read_token();
			goto REST;
		}
		if (nopt >= MAXOPT)
			error("too many optional argument");
		if (c == '(') {
			optional[nopt].o_var = read_token();
			if ((c = nextc()) == ')')
				continue;
			unreadc(c);
			optional[nopt].o_init = read_token();
			if ((c = nextc()) == ')')
				continue;
			unreadc(c);
			optional[nopt].o_svar = read_token();
			if (nextc() != ')')
				error(") expected");
		} else {
			unreadc(c);
			optional[nopt].o_var = read_token();
		}
	}

REST:
	if (strcmp(p, "rest") != 0 && strcmp(p, "r") != 0)
		goto KEYWORD;
	rest_flag = TRUE;
	if ((c = nextc()) == ')' || c == '&')
		error("&rest var missing");
	unreadc(c);
	rest_var = read_token();
	if ((c = nextc()) == ')')
		return;
	if (c != '&')
		error("& expected");
	p = read_token();
	goto KEYWORD;

KEYWORD:
	if (strcmp(p, "key") != 0 && strcmp(p, "k") != 0)
		goto AUX;
	key_flag = TRUE;
	for (;;  nkey++) {
		if ((c = nextc()) == ')')
			return;
		if (c == '&') {
			p = read_token();
			if (strcmp(p, "allow_other_keys") == 0 ||
			    strcmp(p, "aok") == 0) {
				allow_other_keys_flag = TRUE;
				if ((c = nextc()) == ')')
					return;
				if (c != '&')
					error("& expected");
				p = read_token();
			}
			goto AUX;
		}
		if (nkey >= MAXKEY)
			error("too many optional argument");
		if (c == '(') {
			if ((c = nextc()) == '(') {
				p = read_token();
				if (p[0] != ':' || p[1] == '\0')
					error("keyword expected");
				keyword[nkey].k_key = p + 1;
				keyword[nkey].k_var = read_token();
				if (nextc() != ')')
					error(") expected");
			} else {
				unreadc(c);
				keyword[nkey].k_key
				= keyword[nkey].k_var
				= read_token();
			}
			if ((c = nextc()) == ')')
				continue;
			unreadc(c);
			keyword[nkey].k_init = read_token();
			if ((c = nextc()) == ')')
				continue;
			unreadc(c);
			keyword[nkey].k_svar = read_token();
			if (nextc() != ')')
				error(") expected");
		} else {
			unreadc(c);
			keyword[nkey].k_key
			= keyword[nkey].k_var
			= read_token();
		}
	}

AUX:
	if (strcmp(p, "aux") != 0 && strcmp(p, "a") != 0)
		error("illegal lambda-list keyword");
	for (;;) {
		if ((c = nextc()) == ')')
			return;
		if (c == '&')
			error("illegal lambda-list keyword");
		if (naux >= MAXAUX)
			error("too many auxiliary variable");
		if (c == '(') {
			aux[naux].a_var = read_token();
			if ((c = nextc()) == ')')
				continue;
			unreadc(c);
			aux[naux].a_init = read_token();
			if (nextc() != ')')
				error(") expected");
		} else {
			unreadc(c);
			aux[naux].a_var = read_token();
		}
		naux++;
	}
}

get_return(void)
{
	int c;

	nres = 0;
	for (;;) {
		if ((c = nextc()) == ')')
			return;
		unreadc(c);
		result[nres++] = read_token();
	}
}

put_fhead(void)
{
	int i;

	put_lineno();
	fprintf(out, "cl_object %s(int narg", function);
	for (i = 0; i < nreq; i++)
		fprintf(out, ", cl_object %s", required[i]);
	if (nopt > 0 || rest_flag || key_flag)
		fprintf(out, ", ...");
	fprintf(out, ")\n{\n");
}

put_declaration(void)
{
  int i;

  for (i = 0;  i < nopt;  i++) {
    put_lineno();
    fprintf(out, "\tcl_object %s;\n", optional[i].o_var);
  }
  for (i = 0;  i < nopt;  i++)
    if (optional[i].o_svar != NULL) {
      put_lineno();
      fprintf(out, "\tbool %s;\n", optional[i].o_svar);
    }
  if (nkey > 0) {
    put_lineno();
    fprintf(out, "\tcl_object KEYS[%d];\n", nkey);
  }
  for (i = 0;  i < nkey;  i++) {
    fprintf(out, "\tcl_object %s;\n", keyword[i].k_var);
    if (keyword[i].k_svar != NULL)
      fprintf(out, "\tbool %s;\n", keyword[i].k_svar);
  }
  for (i = 0;  i < naux;  i++) {
    put_lineno();
    fprintf(out, "\tcl_object %s;\n", aux[i].a_var);
  }
  if (nopt == 0 && !rest_flag && !key_flag) {
    put_lineno();
    fprintf(out, "\tcheck_arg(%d);\n", nreq);
  } else {
    if (key_flag) {
      put_lineno();
      fprintf(out, "\tcl_object KEY_VARS[%d];\n", 2*nkey);
    }
    put_lineno();
    fprintf(out, "\tva_list %s;\n\tva_start(%s, %s);\n", rest_var, rest_var,
	    ((nreq > 0) ? required[nreq-1] : "narg"));
    put_lineno();
    fprintf(out, "\tif (narg < %d) FEtoo_few_arguments(&narg);\n", nreq);
    if (nopt > 0 && !rest_flag && !key_flag) {
      put_lineno();
      fprintf(out, "\tif (narg > %d) FEtoo_many_arguments(&narg);\n", nreq + nopt);
    }
    for (i = 0;  i < nopt;  i++) {
      put_lineno();
      fprintf(out, "\tif (narg > %d) {\n", nreq+i, optional[i].o_var);
      put_lineno();
      fprintf(out, "\t\t%s = va_arg(%s, cl_object);\n",
	      optional[i].o_var, rest_var);
      if (optional[i].o_svar) {
	put_lineno();
	fprintf(out, "\t\t%s = TRUE;\n", optional[i].o_svar);
      }
      put_lineno();
      fprintf(out, "\t} else {\n");
      put_lineno();
      fprintf(out, "\t\t%s = %s;\n",
	      optional[i].o_var,
	      optional[i].o_init == NULL ? "Cnil" : optional[i].o_init);
      if (optional[i].o_svar) {
	put_lineno();
	fprintf(out, "\t\t%s = FALSE;\n", optional[i].o_svar);
      }
      put_lineno();
      fprintf(out, "\t}\n");
    }
    if (key_flag) {
      for (i = 0; i < nkey; i++) {
	put_lineno();
	fprintf(out, "\tKEYS[%d]=K%s;\n", i, keyword[i].k_key);
      }
      put_lineno();
      fprintf(out, "\tparse_key(narg-%d, (cl_object*)ARGS, %d, KEYS, KEY_VARS, %s, %d);\n",
	      nreq+nopt, nkey, rest_flag ? rest_var : "OBJNULL", allow_other_keys_flag);
      for (i = 0;  i < nkey;  i++) {
	put_lineno();
	fprintf(out, "\tif (KEY_VARS[%d]==Cnil) {\n", nkey+i);
	if (keyword[i].k_init != NULL) {
	  put_lineno();
	  fprintf(out, "\t  %s = %s;\n", keyword[i].k_var, keyword[i].k_init);
	} else {
	  put_lineno();
	  fprintf(out, "\t  %s = Cnil;\n", keyword[i].k_var);
	}
	if (keyword[i].k_svar != NULL) {
	  put_lineno();
	  fprintf(out, "\t  %s = FALSE;\n", keyword[i].k_svar);
	}
	fprintf(out, "\t} else {\n");
	if (keyword[i].k_svar != NULL) {
	  put_lineno();
	  fprintf(out, "\t  %s = TRUE;\n", keyword[i].k_svar);
	}
	put_lineno();
	fprintf(out, "\t  %s = KEY_VARS[%d];\n\t}\n", keyword[i].k_var, i);
      }
    }
  }
  for (i = 0;  i < naux;  i++) {
    put_lineno();
    fprintf(out, "\t%s = %s;\n", aux[i].a_var,
	    aux[i].a_init == NULL ? "Cnil" : aux[i].a_init);
  }
}

put_return(void)
{
	int i, t;

	t = tab_save+1;
	if (nres == 0) {
	  fprintf(out, "return0();");
	} else if (nres == 1) {
	  fprintf(out, "return1(%s);", result[0]);
	} else {
	  fprintf(out, "{\n");
	  put_tabs(t);
	  fprintf(out, "cl_object saved_value = %s;\n", result[0]);
	  for (i = 1;  i < nres;  i++) {
		put_tabs(t);
		fprintf(out, "VALUES(%d) = %s;\n", i, result[i]);
	  }
	  put_tabs(t);
	  fprintf(out, "NValues = %d;\n", nres);
	  put_tabs(t);
	  fprintf(out, "return saved_value;\n");
	  put_tabs(tab_save);
	  fprintf(out, "}\n");
	}
}

char
jump_to_at(void)
{
	char c;
 GO_ON:
	while ((c = readc()) != '@')
		putc(c, out);
	if ((c = readc()) == '@') {
		putc(c, out);
		goto GO_ON;
	}
	return c;
}

main_loop(void)
{
	int c;
	int in_defun=0;
	char *p;

	lineno = 1;

	reset();
	put_lineno();
LOOP:
	c = jump_to_at();
	if (c == ')') {
		if (!in_defun)
			error("unmatched @) found");
		in_defun = 0;
		putc('}',out);
		reset();
		goto LOOP;
	} else if (c == '\'') {
		char *p;
		poolp = pool;
		p = read_symbol_name();
		pushc('\0');
		fprintf(out,"%s",p);
		goto LOOP;
	} else if (c != '(') {
		char *p;
		unreadc(c);
		poolp = pool;
		p = read_func_name();
		pushc('\0');
		fprintf(out,"%s",p);
		goto LOOP;
	}
	p = read_token();
	if (strcmp(p, "defun") == 0) {
		if (in_defun)
			error("@) expected before new function definition");
		in_defun = 1;
		get_function();
		get_lambda_list();
		put_fhead();
		put_lineno();
		c = jump_to_at();
		put_declaration();
		put_lineno();
	} else if (strcmp(p, "return") == 0) {
		if (!in_defun)
			error("@(return) found outside @(defun)");
		tab_save = tab;
		get_return();
		put_return();
	} else
		error_symbol(p);
	goto LOOP;
}

main(int argc, char **argv)
{
	char *p, *q;
	char outfile[BUFSIZ];

	if (argc < 2 || !strcmp(argv[1],"-")) {
	  in = stdin;
	  strcpy(filename, "-");
	} else {
	  in = fopen(argv[1],"r");
	  strncpy(filename, argv[1], BUFSIZ);
	}
	if (argc < 3 || !strcmp(argv[2],"-")) {
	  out = stdout;
	  strncpy(outfile, "-", BUFSIZ);
	} else {
	  out = fopen(argv[2],"w");
	  strncpy(outfile, argv[2], BUFSIZ);
	}
	if (in == NULL)
		error("can't open input file");
	if (out == NULL)
		error("can't open output file");
	printf("dpp: %s -> %s\n", filename, outfile);
	main_loop();
}
