/* cmdline.c -- command line parsing routines */
/* Copyright 1989 Carnegie Mellon University */
/*
 * This module is designed to allow various modules to scan (and rescan)
 * the command line for applicable arguments.  The goal is to hide as
 * much information about switches and their names as possible so that
 * switches become more consistent across applications and so that the
 * author of an application need not do a lot of work to provide numerous
 * options.  Instead, each module scans the command line for its own
 * arguments.
 *
 * Command lines are of the following form:
 *    command -s1 -s2 opt2 -s3 arg1 arg2 -s4 opt4 arg3
 * Note that there are three kinds of command line parameters:
 * (1) A Switch is a "-" followed by a name, e.g. "-s1"
 * (2) An Option is a Switch followed by a space and name, e.g. "-s2 opt2"
 * (3) An Argument is a name by itself, e.g. "arg1"
 * Note also that a switch followed by an argument looks just like an
 * option, so a list of valid option names is necessary to disambiguate.
 *
 * Long names are good for readability, but single character abbreviations
 * are nice for experienced users.  cmdline.c allows single character
 * abbreviations provided that they are unambiguous.  These are
 * recognized with no extra work by the programmer.  If an
 * isolated '?' is encountered in the command line, then all of
 * the options and switches are printed as help and for debugging.
 *
 * Given that we must tell this module about option names and switch
 * names, how should we do it?  We can't wait until modules are
 * initialized, since often modules want to read the command line
 * at initialization time.  In the original implementation, the
 * main program was supposed to provide names for the whole program,
 * but this violates modularity: when an option is added to a module,
 * the main program has to be modified too.  This is a real pain when
 * different machines support different options and you want to have
 * a single machine-independent main program.  The solution is to 
 * have the main program import strings describing the options and
 * switches used by each module.  These are passed into cmdline.c
 * before initialization of other modules is begun.
 *
 * A main program that uses cmdline.c should do the following:
 *   call cl_syntax(s) for each module's option/switch string.
 * The string s should have the following format:
 *  "opt1<o>description;opt2<o>description;...;switch1<s>description;..."
 * where opt1 and opt2 are option names (without the preceding "-"), and
 * switch1 is a switch name.  The <o> and <s> indicate whether the
 * name is an option or a switch.  The descriptions are arbitrary strings
 * (without semicolons) that are printed out for the user when "?"
 * is typed on the command line.
 *
 * After calling cl_syntax, main() should call
 *   cl_init(argv, argc)
 * cl_init will report an error (to STDERR) if it finds any illegal
 * switch or option names in argv, and help will be printed if "?"
 * is found in argv.  If cl_init returns false, then the user has been
 * given an error message or help, and main should probably exit.
 *
 * Afterward, switches, options, and arguments can be accessed by
 * calling cl_switch, cl_option, and cl_arg.  If cl_switch or cl_option
 * is called with a switch name that was not mentioned in the call to 
 * cl_init, an error will result.  This indicates that the application
 * author omitted a valid switch or option name when calling cl_init.
 * This is an error because the full set of names is needed for error
 * checking and to distinguish arguments from options.
 *
 */

/*****************************************************************************
*       Change Log
*  Date | Change
*-----------+-----------------------------------------------------------------
* 13-Jun-86 | Created Change Log
*  6-Aug-86 | Modified for Lattice 3.0 -- use "void" to type some routines
* 20-Sep-89 | Redesigned the interface, adding cl_syntax call.
*  2-Apr-91 | JDW : further changes
* 27-Dec-93 | "@file" as first arg reads command line args from file
* 11-Mar-94 | PLu: Add private to cl_search() definition.
* 28-Apr-03 | DM: true->TRUE, false->FALSE
*****************************************************************************/

/* stdlib.h not on PMAX */
#ifndef mips
#include "stdlib.h"
#endif
#include "stdio.h"
#include "cext.h"
#include "userio.h"
#include "cmdline.h"
#include "ctype.h"
#include "string.h"

/* this should really be defined in security.h, but it is in xlisp.h.
 * I don't want to add an xlisp dependency here, nor do I want to 
 * create security.h since that's not how xlisp does things.
 * The C++ linker will type check so this is at least type safe.
 */
int ok_to_open(const char *filename, const char *mode);

#define syntax_max 10           /* allow for 10 syntax strings */
private char *syntax[syntax_max];
private int n_syntax = 0;       /* number of strings so far */
private char **argv;            /* command line argument vector */
private int argc;               /* length of argv */

private boolean cl_rdy = FALSE;    /* set to TRUE when initialized */

#define cl_OPT 1
#define cl_SW 2
#define cl_INIT 3
#define cl_ARG 4

/*****************************************************************************
*    Routines local to this module
*****************************************************************************/
private char *cl_search(char *name, int opt_sw, int n);
private int find_string(char *s, boolean *abbr);
private void indirect_command(char *filename, char *oldarg0);
private void ready_check(void);

/****************************************************************
*           cl_arg
* Inputs:
*    n: the index of the arg needed
* Results:
*    pointer to the nth arg, or NULL if none exists
*    arg 0 is the command name
*****************************************************************/

char *cl_arg(n)
  int n;
{
    return (n <= 0 ? argv[0] :
                     cl_search((char *)NULL, cl_ARG, n));
}

/* cl_help -- print help from syntax strings */
/**/
void cl_help()
{
    register int i, j;
    int count = 0;	/* see if there are any switches or flags */

    for (i = 0; i < n_syntax; i++) {
        register char *ptr = syntax[i];
        register char c = *ptr++;
        while (c != EOS) {
            while (c != EOS && !(isalnum(c))) c = *ptr++;
            if (c != EOS) {
                count++;
                gprintf(TRANS, "-");
                j = 1;
                while (c != EOS && c != '<') {
                    gprintf(TRANS, "%c", c);
                    c = *ptr++;
                    j++;
                }
                if (c != EOS) {
                    c = *ptr++;
                    if (c == 'o') {
                        gprintf(TRANS, " xxx");
                        j += 4;
                    }
                }
                /* attempt to tab */
                do {
                    gprintf(TRANS, " ");
                } while (j++ < 16);
                while (c != EOS && c != '>') c = *ptr++;
                if (c != EOS) c = *ptr++;
                while (c != EOS && c != ';') {
                    gprintf(TRANS, "%c", c);
                    c = *ptr++;
                }
                gprintf(TRANS, "\n");
            }
        }
    }
    if (!count) gprintf(TRANS, "No switches or options exist.\n");
}

/*****************************************************************************
*           cl_init
* Inputs:
*    char *switches[]:    array of switch names
*    int nsw:   number of switch names
*    char *options[]:    array of option names
*    int nopt:  number of option names
*    char *av:  array of command line fields (argv)
*    int ac:        number of command line fields (argc)
* Effect:
*    Checks that all command line entries are valid.
*    Saves info for use by other routines.
* Returns:
*    TRUE if syntax checks OK, otherwise false
*****************************************************************************/

boolean cl_init(char *av[], int ac)
{
    argv = av;      
    argc = ac;

    /* check for help request */
    if (argc == 2 && strcmp(argv[1], "?") == 0) {
        cl_help();
        return FALSE; /* avoid cl_search which would complain about "?" */
    }
    /* check for indirection */
    if (argc == 2 && *(argv[1]) == '@') {
        /* read new args from file */
        indirect_command(av[1] + 1, av[0]);
    }
    /* check command line syntax: */
    cl_rdy = TRUE;
    return (cl_rdy = (cl_search("true", cl_INIT, 0) != NULL));
}


/****************************************************************
*           cl_int_option
* Inputs:
*    char *name:    name of option
*    long default:  default value for option
* Result:
*    returns long encoding of the option, deflt if none
* Implementation:
*    call cl_option and sscanf result
*****************************************************************/

long cl_int_option(name, deflt)
  char *name;
  long deflt;
{
    char *opt = cl_option(name);
    if (opt) {
        if (sscanf(opt, "%ld", &deflt) != 1) {
            gprintf(TRANS, "Warning: option %s %s not an integer, ignored\n",
                        name, opt);
        }
    }
    return deflt;
}


/****************************************************************
*           cl_search
* Inputs:
*    char *name:    name of field, must be non-null if opt_sw == cl_INIT
*    int opt_sw:    option, switch, init, or arg
*    int n:         argument number (if opt_sw is cl_ARG)
* Result:
*    returns pointer to option value/switch if one exists, otherwise null
* Implementation:
*    parse the command line until name or arg is found
*    see if the option is followed by a string that does
*    not start with "-"
*****************************************************************/

private char *cl_search(char *name, int opt_sw, int n)
{
    register int i = 1;    /* index into command line */
    boolean abbr;
    boolean result = TRUE;

    ready_check();

    /* parse command line: */
    while (i < argc) {
        register char *arg = argv[i];
        /* arguments that start with '-' should be quoted and quotes must
           be removed by the application
         */
        if (*arg == '-') {
            int arg_type = find_string(arg + 1, &abbr);
            if (arg_type == cl_OPT) {
                i += 1; /* skip name and option */
                /* don't look for '-' because the option might be a
                 * negative number
                 */
                if (i >= argc /* || *arg == '-' */) {
                    if (opt_sw == cl_INIT) {
                        gprintf(ERROR, "missing argument after %s\n", arg);
                        result = FALSE;
                    }
                } else if (opt_sw == cl_OPT &&
                    (strcmp(arg + 1, name) == 0 ||
                     (abbr && *(arg + 1) == name[0]))) {
                    return argv[i];
                }
            } else if (arg_type == cl_SW) {
                if (opt_sw == cl_SW &&
                    (strcmp(arg + 1, name) == 0 ||
                     (abbr && *(arg + 1) == name[0])))
                    return arg;
            } else if (opt_sw == cl_INIT)  {
                gprintf(ERROR, "invalid switch: %s\n", arg);
                result = FALSE;
            }
        } else if (opt_sw == cl_ARG) {
            if (n == 1) return arg;
            n--;
        }
        i++; /* skip to next field */
    }
    if (opt_sw == cl_INIT) {
        /* return name or NULL to represent TRUE or FALSE */
        return (result ? name : NULL);
    }
    return NULL;
}

/****************************************************************
*           cl_option
* Inputs:
*    char *name:    option name
* Outputs:
*    returns char *: the option string if found, otherwise null
****************************************************************/

char *cl_option(name)
char *name;
{
    return cl_search(name, cl_OPT, 0);
}

/****************************************************************
*           cl_switch
* Inputs:
*    char *name:    switch name
* Outputs:
*    boolean:    TRUE if switch found
****************************************************************/

boolean cl_switch(name)
char *name;
{
    return (boolean)(cl_search(name, cl_SW, 0) != NULL);
}

/* cl_syntax -- install a string specifying options and switches */
/**/
boolean cl_syntax(char *s)
{
    if (n_syntax < syntax_max) {
        syntax[n_syntax++] = s;
        return TRUE;
    } else {
        gprintf(ERROR, "cl_syntax: out of room\n");
        return FALSE;
    }
}

/****************************************************************
*           find_string
* Inputs:
*    char *s:    string to find, terminated by any non-alphanumeric
*    boolean *abbr: set TRUE if s is an abbreviation, otherwise false
* Effect:
*    Looks for s in syntax strings
* Returns:
*    0 = FALSE = not found, 1 = cl_OPT = option, 2 = cl_SW = switch
*****************************************************************/

private int find_string(char *s, boolean *abbr)
{
    int found_it = FALSE;
    int i;
    *abbr = FALSE;
    for (i = 0; i < n_syntax; i++) {    /* loop through strings */
        register char *syntax_ptr = syntax[i];
        while (*syntax_ptr != EOS) {
            register char *s_ptr = s;
            while (*syntax_ptr != EOS &&
                   !(isalnum(*syntax_ptr))) syntax_ptr++;
            while (*s_ptr != EOS && (*s_ptr++ == *syntax_ptr))
                syntax_ptr++; /* only increment if there's a match */
            if (!(isalnum(*s_ptr)) && *syntax_ptr == '<') {
                syntax_ptr++; /* advance to the type field */
                if (*syntax_ptr == 's') return cl_SW;
                if (*syntax_ptr != 'o') 
                    gprintf(ERROR,
                            "(internal error) bad cl_syntax string: %s\n",
                            syntax[i]);
                return cl_OPT;
            }
            /* no match, so go to next */
            while (*syntax_ptr != ';' && *syntax_ptr != EOS) syntax_ptr++;
            if (*syntax_ptr == ';') syntax_ptr++;
        }
    }

    /* no match, maybe there is a single character match */
    if (s[0] == EOS || s[1] != EOS) return FALSE;

    for (i = 0; i < n_syntax; i++) {    /* loop through strings */
        char *syntax_ptr = syntax[i];
        while (*syntax_ptr != EOS) {
            while (*syntax_ptr != EOS &&
                   !(isalnum(*syntax_ptr))) syntax_ptr++;
            if (s[0] == *syntax_ptr) {
                if (found_it) return FALSE;     /* ambiguous */
                /* else, find the type */
                while (*syntax_ptr != '<' && *syntax_ptr != EOS)
                    syntax_ptr++;
                syntax_ptr++;
                if (*syntax_ptr == 's') found_it = cl_SW;
                else if (*syntax_ptr == 'o') found_it = cl_OPT;
                else return FALSE;      /* error in string syntax */
            }
            /* no match, so go to next */
            while (*syntax_ptr != ';' && *syntax_ptr != EOS) syntax_ptr++;
            if (*syntax_ptr == ';') syntax_ptr++;
        }
    }
    if (found_it) *abbr = TRUE;
    return found_it;
}


/* get_arg -- get an argument from a file */
/**/
boolean get_arg(FILE *file, char *arg)
{
    int c;
    while ((c = getc(file)) != EOF && isspace(c)) ;
    if (c == EOF) return FALSE;
    ungetc(c, file);
    while ((c = getc(file)) != EOF && !isspace(c)) {
            *arg++ = c;
    }
    *arg = 0;
    return TRUE;
}


/* indirect_command -- get argv, argc from a file */
/**/
private void indirect_command(char *filename, char *oldarg0)
{
    FILE *argfile = NULL;
    if (ok_to_open(filename, "r"))
        argfile = fopen(filename, "r");
    if (!argfile) {
        argv = (char **) malloc(sizeof(char *));
        argv[0] = oldarg0;
        argc = 1;
    } else {
        int i = 1;
        char arg[100];
        while (get_arg(argfile, arg)) i++;
        fclose(argfile);
        argfile = fopen(filename, "r");
        argv = (char **) malloc(sizeof(char *) * i);
        argv[0] = oldarg0;
        argc = i;
        i = 1;
        while (get_arg(argfile, arg)) {
            argv[i] = (char *) malloc(strlen(arg) + 1);
            strcpy(argv[i], arg);
            i++;
        }
        fclose(argfile);
    }
}

/****************************************************************
*           ready_check
* Effect:
*    Halt program if cl_rdy is not true.
*****************************************************************/
private void ready_check(void)
{
    if (!cl_rdy) {
        gprintf(ERROR,
        "Internal error: cl_init was not called, see cmdline.c\n");
        EXIT(1);
    }
}
