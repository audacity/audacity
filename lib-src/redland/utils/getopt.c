/* 
 * Public Domain getopt - history below
 *
 */

/*
 * From: gwyn@brl-tgr.ARPA (Doug Gwyn <gwyn>) Newsgroups: net.sources
 * Subject: getopt library routine Date: 30 Mar 85 04:45:33 GMT
 */

/*
 * getopt -- public domain version of standard System V routine
 * 
 * Strictly enforces the System V Command Syntax Standard; provided by D A
 * Gwyn of BRL for generic ANSI C implementations
 * 
 * #define STRICT to prevent acceptance of clustered options with arguments
 * and ommision of whitespace between option and arg.
 */

/*
 * Modified by Manuel Novoa III on 1/5/01 to use weak symbols.
 * Programs needing long options will link gnu_getopt instead.
 */

/*
 * Last public domain version 1.5 downloaded from uclibc CVS:
 * http://www.uclibc.org/cgi-bin/cvsweb/uClibc/libc/unistd/getopt.c
 * on 2003-02-18 by Dave Beckett and tidied:
 *   Ran through "indent getopt.c -gnu" then fixed up the mess
 *   Removed register - compilers are smart these days
 *   ANSI-fied the declarations
 *   Prefixed with rdfproc_ so that it doesn't clash with any getopt
 *   linked in later.
 */


#include <stdio.h>
#include <string.h>

#include <rdfproc_getopt.h>

int opterr;		/* error => print message */
int optind;		/* next argv[] index */
int optopt;		/* Set for unknown arguments */
char *optarg;		/* option parameter if any */

/*
 * Err:
 * program name argv[0]
 * specific message
 * defective option letter
 */
static int
Err (char *name, char *mess, int c)		/* returns '?' */
{
  optopt = c;
  if (opterr)
    {
      (void) fprintf (stderr, "%s: %s -- %c\n", name, mess, c);
    }

  return '?';			/* erroneous-option marker */
}


int
getopt (int argc, char * const argv[], const char *optstring)
{
  static int sp = 1;		/* position within argument */
  int osp;		/* saved `sp' for param test */

#ifndef STRICT
  int oind;		/* saved `optind' for param test */
#endif
  int c;		/* option letter */
  char *cp;		/* -> option in `optstring' */

  optarg = NULL;

  /* initialise getopt vars */
  if (optind == 0)
    {
      optind = 1;
      opterr = 1;
      optopt = 1;
      optarg = NULL;
    }

  if (sp == 1)
    {				/* fresh argument */
      if (optind >= argc	/* no more arguments */
	  || argv[optind][0] != '-'	/* no more options */
	  || argv[optind][1] == '\0'	/* not option; stdin */
	)
	return EOF;
      else if (strcmp (argv[optind], "--") == 0)
	{
	  ++optind;		/* skip over "--" */
	  return EOF;		/* "--" marks end of options */
	}
    }

  c = argv[optind][sp];		/* option letter */
  osp = sp++;			/* get ready for next letter */

#ifndef STRICT
  oind = optind;		/* save optind for param test */
#endif
  if (argv[optind][sp] == '\0')
    {				/* end of argument */
      ++optind;			/* get ready for next try */
      sp = 1;			/* beginning of next argument */
    }

  if (c == ':' 
      || c == '?'	/* optstring syntax conflict */
      || (cp = strchr (optstring, c)) == NULL) /* not found */
    {
      return Err (argv[0], "illegal option", c);
    }

  if (cp[1] == ':')
    {				/* option takes parameter */
#ifdef STRICT
      if (osp != 1)
	{
	  return Err (argv[0], "option must not be clustered", c);
	}

      /* reset by end of argument */
      if (sp != 1)
	{
	  return Err (argv[0], "option must be followed by white space", c);
	}
#else
      if (oind == optind)
	{			/* argument w/o whitespace */
	  optarg = &argv[optind][sp];
	  sp = 1;		/* beginning of next argument */
	}

      else
#endif
      if (optind >= argc)
	{
	  return Err (argv[0], "option requires an argument", c);
	}

      else			/* argument w/ whitespace */
	optarg = argv[optind];

      ++optind;			/* skip over parameter */
    }

  return c;
}
