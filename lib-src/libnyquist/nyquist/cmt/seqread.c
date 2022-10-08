/****************************************************************************
               seqread.c -- Phase 1 of adagio compilation...

 this module parses adagio programs and builds a linked list structure
 consisting of notes and control changes in time order.

 Copyright 1989 Carnegie Mellon University
*****************************************************************************/

/*****************************************************************************
*       Change Log
*  Date     | Change
*-----------+-----------------------------------------------------------------
* 31-Dec-85 | Created changelog
* 31-Dec-85 | Add c:\ to include directives
* 31-Dec-85 | Added standard command scanner, metronome variable, need to add 
*           | cmdline_help procedure
* 31-Dec-85 | Call intr_init
* 31-Dec-85 | Set musictrace from command line via -trace
* 31-Dec-85 | Added -poll
*  1-Jan-86 | Put error messages out to stderr
*  1-Jan-86 | Set IsAT.     Can be later overridden by -at and -xt switches,
*           | currently only used for diagnostics (may be needed for
*           | compatibles, who knows?  In which case remove the tests which
*           | confirm the type of processor)
*  1-Jan-86 | <rgd/jmn> Removed dur-adjusted message
*  1-Jan-86 | Added miditrace
* 18-Jan-86 | Shortened durations by 1/200 s to avoid roundoff problems --
*           | see buildnote for details.
*  3-Mar-86 | Allow octave and accidentals in either order after pitch name.
*           | Default octave is now one that gets nearest previous pitch,
*           |  the tritone (half an octave) interval is descending by default.
*           | Special commands handled by table search, !Rate command added
*           |  to scale all times by a percentage (50 = half speed).
*  9-Mar-86 | Use space to limit amount of storage allocation.    Otherwise
*           |    exhausting storage in phase1 caused phase2 to fail.
* 12-Mar-86 | Broke off command line parser into adagio.c, only parser remains
* 24-Mar-86 | Changed representation from note_struct to event_struct
*           | Parse M, N, O, X, and Y as control change commands
* 23-May-86 | Added , and ; syntax: "," means "N0\n", ";" means "\n"
* 16-Jul-86 | modify to only call toupper/lower with upper/lower case as
*           |  parameter to be compatible with standard C functions
*  7-Aug-86 | fixed bug with default pitches and rests
*  5-Jul-87 | F.H: Introduced new memory handling from Mac version.
*           |    Changed:    init()
*           |       ins_event()
*           |       ins_ctrl()
*           |       ins_note()
*           |    Deleted:    reverse()
*           |       nalloc()
*           |    Introduced:    event_alloc()
*           |       phase1_FreeMem()
*           |       system.h & system.c dependencies
* 10-Feb-88 | fixed parseend to accept blanks and tabs,
*           | fixed rate scaling of durations
* 11-Jun-88 | commented out gprintf of \n to ERROR after parsing finished.
* 13-Oct-88 | JCD : exclusive AMIGA version.
* 13-Apr-89 | JCD : New portable version.
* 31-Jan-90 | GWL : Cleaned up for LATTICE
* 30-Jun-90 | RBD : further changes
*  2-Apr-91 | JDW : further changes
* 30-Jun-91 | RBD : parse '+' and '/' in durations, * after space is comment
* 28-Apr-03 |  DM : changes for portability
*****************************************************************************/

#include "switches.h"

#include <stdio.h>
#include <string.h>

#include "cext.h"
#include "cmdline.h"
#include "midifns.h" /* to get time_type */
#include "timebase.h"
#include "moxc.h"    /* to get debug declared */
#include "seq.h"
#include "seqread.h"
#include "userio.h"
/* ctype.h used to be included only by UNIX and AMIGA,
   surely everyone wants this? */
#include "ctype.h"

#ifndef toupper
/* we're already taking precautions, so inline version of toupper is ok: */
#define toupper(c) ((c)-'a'+'A')
/* CAUTION: AZTEC V5.0 defines an inline version of toupper called _toupper,
   but they got it wrong!
 */
#endif

/* cmtcmd.h references amiga message ports */
#ifdef AMIGA
#ifdef LATTICE
#include "amiga.h"
#endif
#include "exec/exec.h"
#endif
#include "cmtcmd.h"

/* public stuff */
extern long space;    /* remaining free bytes */
extern int abort_flag;

/****************************************************************************
 The following are used to simulate fixed point with the radix point
 8 bits from the right:
****************************************************************************/

#define precise(x) (((time_type) x) << 8)
#define seqround(x) ((((time_type) x) + 128) >> 8)
#define trunc(x) (((time_type) x) >> 8)

#define nullstring(s) (s[0] == EOS)


/****************************************************************************
* Routines local to this module:
****************************************************************************/
private void            do_a_rest(void);
private time_type       doabsdur(void);
private int             doabspitch(void);
private void            doclock(void);
private void            docomment(void);
private void            doctrl(int n);
private void            dodef(void);
private time_type       dodur(void);
private void            doerror(void);
private int             doloud(void);
void                    domacro(void);
private void            donextdur(void);
private int             dopitch(void);
private void            doprogram(void);
private void            dorate(void);
private void            doset(boolean vecflag);
private void            dospecial(void);
private time_type       dosymdur(void);
private void            dotempo(void);
private void            dotime(void);
private void            dovoice(void);
private void            fferror(char *s);
private void            init(void);
private int             issymbol(void);
private void            marker(int count);
private void            parseend(void);
private void            parsefield(void);
private boolean         parsenote(void);
private boolean         parseparm(long *valptr);
private int             scan(void);
private int             scan1(char *start);
private long            scanint(void);
private void            scansymb(char *);
private long            scansgnint(void);

/****************************************************************************
* data structures for parser lookup tables
****************************************************************************/

struct durt {    /* duration translation table */
    char symbol;
    time_type value;
};

#define durtable_len 7
struct durt durtable[durtable_len] = {
    {'W', 4800L},
    {'H', 2400L},
    {'Q', 1200L},
    {'I', 600L},
    {'S', 300L},
    {'%', 150L},
    {'^', 75L}
};

struct loudt {    /* loudness translation table */
    char symbol[4];
    int value;
};

struct loudt loudtable[] = {
    {"PPP", 20},
    {"PP\0", 26},
    {"P\0\0", 34},
    {"MP\0", 44},
    {"MF\0", 58},
    {"F\0\0", 75},
    {"FF\0", 98},
    {"FFF", 127}
};

char too_many_error[] = "Too many parameters";

private char *ssymbols[] = {"TEMPO", "RATE", "CSEC", "MSEC", 
                            "SETI", "SETV", "CALL", "RAMP",
                            "CLOCK", "DEF", "END"};

#define sym_tempo 0
#define sym_rate 1
#define sym_csec 2
#define sym_msec 3
#define sym_seti 4
#define sym_setv 5
#define sym_call 6
#define sym_ramp 7
#define sym_clock 8
#define sym_def 9
#define sym_end 10

/* number of symbols */
#define sym_n 11

#define linesize 100
private char line[linesize];    /* the input line */
private char token[linesize];    /* a token scanned from the input line */

private boolean pitch_flag;    /* set when a pitch is indicated */
/* (if controls changes are given, only allocate a note event if
     *  a pitch was specified -- i.e. when pitch_flag is set)
     */
private boolean rest_flag;    /* set when a rest (R) is found */
/* this flag is NOT inherited by the next line */

private boolean symbolic_dur_flag;
/* TRUE if last dur was not absolute
         * (if this is set, then the default duration is changed
         *  accordingly when the tempo is changed.)
         */


#define nctrl 8

private boolean ctrlflag[nctrl];
/* TRUE if control change was present
         * ctrlflag[0] TRUE if ANY control change
         * was present
         */
private int ctrlval[nctrl];
/* the new value of the control */
#define nmacroctrl 10
short macctrlx;                 /* index into the following: */
short macctrlnum[nmacroctrl];   /* macro ctrl number, e.g. for ~4(67), or
                                 * number of parameters for a symbolic macro */
short macctrlparmx[nmacroctrl]; /* ctrl value for ctrl change, or index of
                                 * parameters for symbolic macro */
short macctrlparms[nmacroctrl*nmacroparms]; /* parameters for symbolic macros */
short macctrlnextparm;
def_type macctrldef[nmacroctrl]; /* definition for symbolic macro */

private time_type time_scale; /* 1000 if centisec, 100 if millisec */
/* note: user_specified_time * (time_scale / rate) = millisec */



/****************************************************************************
*
*    variables private to this module
*
****************************************************************************/

private boolean end_flag = FALSE;    /* set "true" when "!END" is seen */

/****************************************************************************
*               state variables
* Because each line of an Adagio score inherits properties from the previous
* line, it makes sense to implement the parser as a collection of routines
* that make small changes to some global state.     For example, pitch is a
* global variable.  When the field G4 is encountered, the dopitch routine
* assigns the pitch number for G4 to the variable pitch.  After all fields
* are processed, these variables describe the current note and contain the
* default parameters for the next note as well.
*
* Global variables that are used in this way by the parsing rountines are:
****************************************************************************/
private int
linex,    /* index of the next character to be scanned */
lineno,    /* current line number */
fieldx,    /* index of the current character within a field */
pitch,    /* pitch of note */
loud,    /* loudness of note */
voice,    /* voice (midi channel) of note */
artic;  /* articulation (a percentage of duration) */

private boolean ndurp;  /* set when a next (N) is indicated */
/* (next time defaults to the current time plus duration unless
     *  overridden by a next (N) command whose presence is signalled
     *  by ndurp.)
     */

private time_type
thetime,    /* the starting time of the note */
rate,    /* time rate -- scales time and duration, default = 100 */
ntime,    /* the starting time of the next note */
dur,    /* the duration of the note */
tempo,    /* the current tempo */
start,    /* the reference time (time of last !tempo or !rate cmd) */
ticksize; /* set by !clock command, zero for no clock */

private int pitchtable[7] = { 
    69, 71, 60, 62, 64, 65, 67 };

extern char score_na[name_length];

private seq_type the_score;  /* this is the score we are parsing */


/* def_append -- append a byte to the current definition */
/*
 * The def data structure:
 *     [code][offset][code][offset]...[0][length][data][data][data]...
 * where code is 1:nmacroparms for %n,
 *               nmacroparms+1 for %v,
 *               nmacroparms+2:nmacroparms*2+1 for ^n
 * and offset is the byte offset (from the offset byte) to the data
 *      where the parameter should be substituted
 * and length is the number of data bytes
 */
boolean def_append(unsigned char def[], int nparms, int data)
{
    int base = (nparms << 1) + 1;       /* this byte is the length */
    /* first parameter has to be able to reference last byte: */
    if ((def[base])++ >= (254 - (nparms << 1))) {
        fferror("Data too long");
        return FALSE;
    }
    def[base + def[base]] = data;
    return TRUE;
}


def_type def_lookup(char *symbol)
{
    def_type defn = seq_dictionary(the_score);
    while (defn) {
        if (strcmp(defn->symbol, symbol) == 0) {
            return defn;
        }
        defn = defn->next;
    }
    return NULL;
}


void def_parm(unsigned char def[], int nparms, int code)
{
    int i, j;
    /* in order to insert a 2-byte parameter descriptor, the offsets from
     * previous descriptors (that precede the data) need to be increased by 2:
     */
    for (i = 1; i < (nparms << 1); i += 2) {
        def[i] += 2;
    }
    /* now i is index of length; work backwards from the last byte, moving
     * everything up by 2 bytes to make room for the new descriptor:
     */
    for (j = i + def[i] + 2; j > i; j--) {
        def[j] = def[j - 2];
    }
    /* now i is index of offset; insert the descriptor code (first byte)
     * and the offset to the parameter location in the message (second byte)
     */
    def[i - 1] = code;
    def[i] = def[i + 2] + 2;
}

/****************************************************************************
*               do_a_rest
* Effect: parses a rest (R) command
****************************************************************************/

private void do_a_rest(void)
{
    if (token[fieldx])
        fferror("Nothing expected after rest");
    rest_flag = TRUE;
}

/****************************************************************************
*               doabsdur
* Effect: parses an absolute dur (U) command
****************************************************************************/

private time_type doabsdur(void)
{
    time_type result=1000L;
    register char c;
    if (isdigit(token[fieldx])) {
        result = precise(scanint());
        /* allow comma or paren for use in parameter lists */
        if ((c = token[fieldx]) && (c != ',') && (c != ')') && (c != '+')) {
            fferror("U must be followed by digits only");
        }
        if (time_scale == 1000) result *= 10; /* convert to ms */
    } else fferror("No digit after U");
    return result;
}

/****************************************************************************
*               doabspitch
* Effect: parses an absolute pitch (P) command
****************************************************************************/

private int doabspitch(void)
{
    int result = 60;
    int startx = fieldx;
    register char c;
    int savex;
    if (isdigit (token[fieldx])) {
        result = (int) scanint();
        /* allow comma or paren for abspitch in parameter */
        if ((c = token[fieldx]) && c != ',' && c != ')')
            fferror("P must be followed by digits only");
        else if (result < minpitch) {
            savex = fieldx;
            fieldx = startx;
            fferror("Minimum pitch of 0 will be used");
            result = minpitch;
            fieldx = savex;
        } else if (result > maxpitch) {
            savex = fieldx;
            fieldx = startx;
            fferror("Maximum pitch of 127 will be used");
            result = maxpitch;
            fieldx = savex;
        }
    } else fferror("No digits after P");
    return result;
}


/* doartic -- compute an articulation factor */
/*
  NOTE: artic is a percentage that scales the duration
  of notes but not the time to the next note onset. It
  is applied to the final computed duration after all
  other scaling is applied.
 */
private void doartic(void)
{
    if (isdigit(token[fieldx])) {
        artic = (int) scanint();
        if (token[fieldx])
            fferror("Only digits were expected here");
    } else fferror("No digits after /");
}


/* docall -- parse a call in the form !CALL fn(p1,p2,p3) */
/**/
private void docall(void)
{
    boolean error_flag = TRUE;
    ndurp = FALSE;

    linex += scan();

    if (token[0] == 0) fferror("Function name expected");
    else {
        char symbol[100];
        struct symb_descr *desc;
        long value[SEQ_MAX_PARMS];
        int i=0;

        scansymb(symbol);
        if (fieldx == 1) fferror("Routine name expected");
        else if (token[fieldx] != '(') fferror("Open paren expected");
        else {
            desc = &HASHENTRY(hash_lookup(symbol));
            if (!desc->symb_type) {
                fieldx = 0;
                fferror("Function not defined");
            } else if (desc->symb_type != fn_symb_type) {
                fieldx = 0;
                gprintf(TRANS, "desc->symb_type is %d\n", desc->symb_type);
                fferror("This is not a function");
            } else {
                error_flag = FALSE;
                fieldx++;       /* skip over paren */
                for (i = 0; i < SEQ_MAX_PARMS; i++) value[i] = 0;
                i = 0;
                /* note that no params "()" is legal */
                while (i < SEQ_MAX_PARMS && token[fieldx] != ')' && 
                       parseparm(&value[i])) {
                    i++;
                    if (token[fieldx] == ',') {
                        fieldx++;
                    } else if (token[fieldx] != ')') {
                        fferror("Unexpected character");
                        error_flag = TRUE;
                        break;
                    }
                }
                fieldx++;
                if (i > SEQ_MAX_PARMS) fferror("Too many parameters");
            }
            while (TRUE) {
                linex += scan();
                if (nullstring(token)) {
                    break;
                } 
                switch (token[0]) {
                  case 'T':
                    fieldx = 1;
                    dotime();
                    break;
                  case 'V':
                    fieldx = 1;
                    dovoice();
                    break;
                  case 'N':
                    fieldx = 1;
                    donextdur();
                    break;
                  default:
                    fferror("Unexpected character");
                }
            }
            if (!error_flag)
                insert_call(the_score, seqround(thetime), lineno, voice, 
                            desc->ptr.routine, value, i);
            /* advance the time only if an N field was given */
            if (ndurp) thetime += ntime;
        }
    }
}


/* doclock -- insert a clock command */
/*
 * derivation: if there is no previous clock running, then start the
 *     clock on time.  Otherwise, start the clock half a tick early.
 *     ticksize = (beattime / 24) = ((60sec/tempo)/24) =
 *      ((60000ms/tempo)/24) = (60000/24)/tempo = 2500/tempo
 */
private void doclock(void)
{
    long oldticksize = ticksize;
    ticksize = (2500L << 16) / tempo;
    insert_clock(the_score, seqround(thetime) - (oldticksize >> 17),
                 lineno, ticksize);
}


/****************************************************************************
*               docomment
* Effect: parses a comment (*) command
****************************************************************************/

private void docomment(void)
{
    line[linex] = '\n'; /* force end of line to skip comment line */
    line[linex+1] = EOS;
}

/****************************************************************************
*               doctrl
* Inputs:
*    n: control number
* Effect: parses a control (K, M, O, X, or Y) command
****************************************************************************/

private void doctrl(int n)
{
    ctrlval[n] = (int) scanint();
    if (token[fieldx]) {
        fferror("Only digits expected here");
    } else {
        ctrlflag[n] = TRUE;
        ctrlflag[0] = TRUE;    /* ctrlflag[0] set if any flag is set */
    }
}


private void dodef(void)
{
    /* maximum def size is 256 + 9 parms * 2 + 2 = 276 */
    unsigned char def[280];
    char symbol[100];
    int nparms = 0;
    int nibcount = 0;
    int data = 0;
    register char c;

    linex += scan();

    if (!token[0]) fferror("Symbol expected");
    else {
        strcpy(symbol, token);
        def[0] = def[1] = 0;
        while (TRUE) {
            linex += scan1(&line[linex]);
            c = token[0];
            if (!c) {
                linex--;
                if (nibcount & 1) {
                    fferror("Expected pairs of hex digits: one missing");
                    return;
                }
                break;
            } else if (c == ' ' || c == '\t' || c == '\n') continue;
            else if (isdigit(c)) {
                data = (data << 4) + (c - '0');
                nibcount++;
                if (!(nibcount & 1)) {
                    if (!def_append(def, nparms, data))
                        return;
                    data = 0;
                }
            } else if ('A' <= c && c <= 'F') {
                data = (data << 4) + (c - 'A') + 10;
                nibcount++; 
                if (!(nibcount & 1)) {
                    if (!def_append(def, nparms, data))
                        return;
                    data = 0;
                }
            } else if (c == 'V') {
                data = data << 4;
                nibcount++;
                /* v without a leading nibble is equivalent to 0v: */
                if (nibcount & 1) nibcount++;
                if (!def_append(def, nparms, data))
                    return;
                def_parm(def, nparms++, nmacroparms+1);
            } else if (c == '%') {
                linex += scan1(&line[linex]);
                c = token[0];
                if (c < '1' || c > ('0' + nmacroparms)) {
                    fferror(parm_expected_error);
                    break;
                }
                if (!def_append(def, nparms, 0))
                    return;
                def_parm(def, nparms++, c - '0');               
            } else if (c == '^') {
                linex += scan1(&line[linex]);
                c = token[0];
                if (c < '1' || c > ('0' + nmacroparms)) {
                    fferror(parm_expected_error);
                    break;
                }
                if (!def_append(def, nparms, 0))
                    return;
                def_parm(def, nparms++, (c - '0') + nmacroparms + 1);
            } else {  /* something unexpected here -- just exit */
                linex--;
                fferror("Unexpected data");
                return;
            }
        }
        insert_def(the_score, symbol, def,
                   (nparms << 1) + def[(nparms << 1) + 1] + 2);
    }
}

/****************************************************************************
*               dodur
* Effect: parses a duration (sum of dosymdur and/or doabsdur)
* sets symbolic_dur_flag (according to the first addend in mixed arithmetic)
*
* Returns: duration in "precise" units
****************************************************************************/
private time_type dodur(void)
{
    time_type result = 0L;
    symbolic_dur_flag = TRUE;

    if (token[fieldx-1] == 'U') {
        result = doabsdur();
        symbolic_dur_flag = FALSE;
    } else result = dosymdur();
    while (token[fieldx] == '+') {
        fieldx += 2;
        if (token[fieldx-1] == 'U') result += doabsdur();
        else result += dosymdur();
    }
    return scale(result, 100L, rate);
}

/****************************************************************************
*               doerror
* Effect: parse an unrecognized field by reporting an error
****************************************************************************/

private void doerror(void)
{
    fieldx = 0;
    fferror("Bad field");
}

/****************************************************************************
*               doloud
* Effect: parse a loudness (L) command
****************************************************************************/

private int doloud(void)
{
    size_t ii;
    int i, j;
    int result;
    int oldfieldx = fieldx;
    int newfieldx;
    char symbol[100];

    if (!token[fieldx] || token[fieldx]==')' || token[fieldx]==',') {
        fferror("L must be followed by loudness indication");
        return 100;
    }
    if (isdigit(token[fieldx])) {
        result = (int) scanint();
        newfieldx = fieldx;
        if (token[fieldx] && token[fieldx]!=')' && token[fieldx]!=',')
            fferror("Digits expected after L");
        else if (result > 127) {
            fieldx = oldfieldx;
            fferror("Maximum loudness of 127 will be used");
            fieldx = newfieldx;
            result = 127;
        } else if (result == 0) {
            fieldx = oldfieldx;
            fferror("Minimum loudness of 1 will be used");
            fieldx = newfieldx;
            result = 1;
        }
        return result;
    }
    scansymb(symbol);
    newfieldx = fieldx;
    if ((ii = strlen(symbol)) > 3 ) {    /* maximum is 3, e.g. "ppp" */
        fieldx = oldfieldx;
        fferror("Loudness field too long");
        fieldx = newfieldx;
        return 100;
    }
    symbol[ii + 1] = '\0';   /* pad short symbols with 0    */
                            /* e.g. "p\0" -> "p\0\0"    */
    for (i = 0; i <= 7; i++) {    /* loop through possibilities    */
        for (j = 0; j <= 2; j++) {    /* test 3 characters    */
            if (symbol[j] != loudtable[i].symbol[j])
                break;
        }
        if (j == 3) {
            return loudtable[i].value;
        }
    }
    fieldx = oldfieldx;
    fferror("Bad loudness indication");
    fieldx = newfieldx;
    return 100;
}


void domacro(void)
{
    int control_num;
    int value;
    if (isdigit(token[1])) {
        control_num = (int) scanint();
        if (token[fieldx] == '(') {
            fieldx++;
            if (!isdigit(token[fieldx])) {
                fferror("Control value expected");
            } else {
                value = (int) scanint();
                if (token[fieldx] != ')') {
                    fferror("Missing close paren");
                } else {
                    fieldx++;
                    if (token[fieldx])
                        fferror("Nothing expected after paren");
                    else if (macctrlx < nmacroctrl - 1) {
                        macctrlnum[macctrlx] = control_num;
                        macctrlparmx[macctrlx] = value;
                        macctrldef[macctrlx] = NULL;
                        macctrlx++;
                    } else fferror("Too many controls");
                }
            }
        } else fferror("Missing paren");
    } else {
        def_type def;
        char symbol[100];
        scansymb(symbol);
        if (fieldx == 1) fferror("Macro name expected");
        else if (token[fieldx] != '(') fferror("Open paren expected");
        else {
            fieldx++;
            def = def_lookup(symbol);
            if (!def) {
                fieldx = 1;
                fferror("Undefined macro");
            } else {
                long val;
                macctrlnum[macctrlx] = 0;
                macctrlparmx[macctrlx] = macctrlnextparm;
                macctrldef[macctrlx] = def;
                while (token[fieldx] != ')' && parseparm(&val)) {
                    macctrlparms[macctrlnextparm++] = (short) val;
                    macctrlnum[macctrlx]++;
                    if (token[fieldx] == ',') {
                        fieldx++;
                    } else if (token[fieldx] != ')') {
                        fferror("Unexpected character");
                        break;
                    }
                }
                fieldx++;
                macctrlx++;
            }
        }
    }
}


/****************************************************************************
*               donextdur
* Effect: parse a next (N) command
* Implementation:
*    The syntax is N followed by a duration, so save dur and use dosymdur()
*    to parse the duration field.
*    The form N<digits> is parsed directly with scanint().
****************************************************************************/

private void donextdur(void)
{
    ndurp = TRUE;    /* flag that N was given */
    if (isdigit(token[fieldx])) {
        ntime = precise(scanint());
        ntime = scale(ntime, (ulong)time_scale, rate);
        if (token[fieldx])
            fferror("Only digits were expected here");
    } else {
        fieldx++;
        ntime = dodur();
    }
}

/****************************************************************************
*               dopitch
* Effect: parses a pitch command
****************************************************************************/

private int dopitch(void)
{
    int p, octave=0;
    int octflag = FALSE;    /* set if octave is specified */
    int oldfieldx = fieldx;

    p = pitchtable[token[fieldx-1]-'A'];
    while (TRUE) {
        if (token[fieldx] == 'S') {                /* sharp */
            p++;
            fieldx++;
        } 
        else if (token[fieldx] == 'N') {            /* skip */
            fieldx++;
        } 
        else if (token[fieldx] == 'F') {            /* flat */
            p--;
            fieldx++;
        } 
        else if (isdigit(token[fieldx]) && !octflag) {      /* octave */
            octave = (int) scanint();
            octflag = TRUE;
        } 
        else break;                /* none of the above */
    }
    if (octflag) p = (p-48) + 12 * octave;  /* adjust p to given octave */
    else {        /* adjust p to note nearest the default pitch */
        int octdiff = (p + 126 - pitch) / 12;
        p = p + 120 - (octdiff * 12);
    }
    if (p > maxpitch) {              /* pitch in range? */
        int newfield = fieldx;
        fieldx = oldfieldx;
        fferror("Pitch too high");
        fieldx = newfield;
        p = maxpitch;
    }
    /* We really should test for end-of-field, but we don't know if we're
       in a parameter list, so comma may or may not be legal */
    return p;
}

/****************************************************************************
*               doprogram
* Effect: parses a program change (Z) command
****************************************************************************/

private void doprogram(void)
{
    register int program = (int) scanint();
    ctrlflag[PROGRAM_CTRL] = ctrlflag[0] = TRUE;
    if (token[fieldx]) {
        fferror("Z must be followed by digits only");
    } else if (program < minprogram) {
        fieldx = 1;
        fferror("Minimum program of 1 will be used");
        program = minprogram;
    } else if (program > maxprogram) {
        fieldx = 1;
        fferror("Maximum program of 128 will be used");
        program = maxprogram;
    }
    ctrlval[PROGRAM_CTRL] = program - 1;
}


private void doramp(void)
{
    int values[2];
    time_type stepsize = 100L;  /* default 10 per second */
    int index = 0;
    ndurp = FALSE;
    values[0] = values[1] = 0;
    while (TRUE) {
        linex += scan();
        fieldx = 1;
        if (nullstring(token)) {
            break;
        } else if (index == 2) { /* must be stepsize in dur syntax */
            stepsize = dodur();
        } else {
            int ctrlx = 0;
            static int ctrl_map[] = { -BEND_CTRL, VOLUME, -TOUCH_CTRL, MODWHEEL };

            switch (token[0]) {
              case 'M': ctrlx++;        /* modwheel */
              case 'O': ctrlx++;        /* aftertouch */
              case 'X': ctrlx++;        /* volume */
              case 'Y':                 /* pitch bend */

                if (index < 2) {
                    macctrlnum[index] = ctrl_map[ctrlx];
                    macctrlparmx[index] = (int) scanint();
                    if (token[fieldx])
                        fferror("Only digits expected here");
                    macctrldef[index] = NULL;
                } else fferror("Unexpected control");
                break;
              case '~':
                if (index < 2) {
                    domacro();
                    if (token[fieldx]) fferror("Unexpected character");
                } else fferror("Unexpected control");
                break;
              case 'T':
                if (index < 2) fferror("Control expected");
                dotime();
                break;
              case 'V':
                if (index < 2) fferror("Control expected");
                dovoice();
                break;
              case 'N':
                if (index < 2) fferror("Control expected");
                donextdur();
                break;
              default:
                if (index < 2) fferror("Control expected");
                dur = dodur();
                break;
            }
            if (index == 1 && (macctrlnum[0] != macctrlnum[1] ||
                               macctrldef[0] != macctrldef[1])) {
                fferror("Controls do not match");
            }
        }
        index++;
    }
    if (index < 3) fferror("Expected 2 controls and step size");
    else {
        if (macctrldef[0]) {
            int i, j, n;
            n = 0;
            i = macctrlparmx[0];
            j = macctrlparmx[1];
            while (n < macctrlnum[0]) {
                if (macctrlparms[i] != macctrlparms[j]) break;
                n++; i++; j++;
            }
            if (n >= macctrlnum[0]) n = 0;
            /* Note: duration shortened to prevent overlap with next ramp */
            insert_deframp(the_score, seqround(thetime), lineno, voice,
                seqround(stepsize), trunc(dur) - 1, macctrldef[0], macctrlnum[0],
                macctrlparms + macctrlparmx[0], n, macctrlparms[j]);
        } else {
            /* Note: duration shortened to prevent overlap with next ramp */
            insert_ctrlramp(the_score, seqround(thetime), lineno, voice,
                seqround(stepsize), trunc(dur) - 1,
                macctrlnum[0], macctrlparmx[0], macctrlparmx[1]);
        }
    }
    /* advance the time only if an N field was given */
    if (ndurp) thetime += ntime;
    else thetime += dur;
}


/****************************************************************************
*               dorate
* Effect: parses a !rate command
****************************************************************************/

private void dorate(void)
{
    linex += scan();
    if (!token[0])
        fferror("rate number expected");
    else {
        long oldrate = rate;
        rate = (int) scanint();
        if (token[fieldx])
            fferror("Only digits expected here");
        if (rate == 0) {
            fieldx = 0;
            fferror("Rate 100 will be used here");
            rate = 100L;
        }
        start = thetime;
        /* adjust dur in case it is inherited by next note */
        dur = (dur * oldrate);
        dur = dur / rate;
    }
}


private void doset(boolean vec_flag)
{
    ndurp = FALSE;
    linex += scan();
    if (!token[0]) fferror("Variable name expected");
    else {
        struct symb_descr *desc = &HASHENTRY(hash_lookup(token));
        if (!desc->symb_type) fferror("Called function not defined");
        else if (vec_flag && (desc->symb_type != vec_symb_type)) {
                fferror("This is not an array");
        } else if (!vec_flag && (desc->symb_type != var_symb_type)) {
                fferror("This is not a variable");
        } else {
            int numargs = 1 + vec_flag;
            int value[2];
            int i;
            int *address = desc->ptr.intptr;
            value[0] = value[1] = 0;
            i = 0;
            while (TRUE) {
                linex += scan();
                if (nullstring(token)) {
                    break;
                } else if (isdigit(token[0]) || token[0] == '-' ||
                           token[0] == '+') {
                    if (i < numargs) {
                        value[i++] = (int) scansgnint();
                        if (token[fieldx])
                            fferror("Only digits expected here");
                    } else fferror(too_many_error);
                } else {
                    switch (token[0]) {
                      case 'T':
                        fieldx = 1;
                        dotime();
                        break;
                      case 'V':
                        fieldx = 1;
                        dovoice();
                        break;
                      case 'N':
                        fieldx = 1;
                        donextdur();
                        break;
                      default:
                        fieldx++;
                        if (i < numargs) {
                            value[i++] = (int) seqround(dodur());
                        } else fferror(too_many_error);
                        break;
                    }
                }
            }
            if (vec_flag && i != 2) fferror("No index given");
            if (vec_flag) {
                if (value[0] >= desc->size) {
                    fferror("Subscript out of bounds");
                    return;
                }
                /* reduce to the seti case: */
                address += value[0];    /* compute the vector address */
                value[0] = value[1];    /* set up value[] and i as if */
                i--;                    /* this were seti, not setv */
            }
            if (i != 1) fferror("No value given");
            insert_seti(the_score, seqround(thetime), lineno, voice,
                        address, value[0]);
            /* advance the time only if an N field was given */
            if (ndurp) thetime += ntime;
        }
    }
}

/****************************************************************************
*               dospecial
* Effect: parses special (those starting with "!") commands
****************************************************************************/

private void dospecial(void)
{
    switch (issymbol()) {
      case sym_tempo: 
        dotempo();
        break;
      case sym_rate: 
        dorate();
        break;
      case sym_csec:
        /* adjust dur for inheritance by next note */
        dur = (dur * 1000L) / time_scale;
        time_scale = 1000L;
        break;
      case sym_msec:
        dur = (dur * 100L) / time_scale;
        time_scale = 100L;
        break;
      case sym_seti:
        doset(FALSE);
        break;
      case sym_setv:
        doset(TRUE);
        break;
      case sym_call:
        docall();
        break;
      case sym_ramp:
        doramp();
        break;
      case sym_clock:
        doclock();
        break;
      case sym_def:
        dodef();
        break;
      case sym_end:
        end_flag = TRUE;
        break;
      default: 
        fferror("Special command expected");
    }
    parseend(); /* flush the rest of the line */
}

/****************************************************************************
*               dosymdur
* Effect: parses a duration (^, %, S, I, Q, H, or W) command
****************************************************************************/

private time_type dosymdur(void)
{
    int i, dotcnt = 0;
    long dotfactor;
    time_type result = 0;

    for (i = 0; i < durtable_len; i++) {
        if (durtable[i].symbol == token[fieldx-1]) {
            /* the shift right is because durs are stored doubled because
             *  otherwise a 64th note would have the value 75/2:  */
            result = precise(durtable[i].value) >> 1;
            break;
        }
    }
    if (i == durtable_len) {
        fieldx--;
        fferror("Duration expected: one of W, H, Q, I, S, %, or ^");
        return 0L;
    }
    while (token[fieldx]) {
        if (token[fieldx] == 'T') {     /* triplet notation */
            result = (result * 2) / 3;  /* lose a bit but avoid scale() */
            fieldx++;
        } 
        else if (token[fieldx] == '.') {    /* dotted notation */
            dotcnt++;
            fieldx++;
        } 
        else if (token[fieldx] == '/') {
            long divisor;
            fieldx++;
            divisor = scanint();
            if (divisor > 0) result = result / divisor;
            else fferror("non-zero integer expected");
        }
        else if (isdigit(token[fieldx])) {    /* numbers are multipliers */
            result = result * scanint();
        } 
        else break;
    }
    dotfactor = 1L;
    for (i=1; i<=dotcnt; i++)
        dotfactor = dotfactor * 2;
    result = (2 * result) - (result / dotfactor);

    return scale(result, 100L, tempo);    /* time in milliseconds */
}

/****************************************************************************
*               dotempo
* Effect: parses a !tempo command
****************************************************************************/

private void dotempo(void)
{
    linex += scan();
    if (!token[0])
        fferror("Tempo number expected");
    else {
        long oldtempo = tempo;
        tempo = scanint();
        if (token[fieldx])
            fferror("Only digits expected here");
        if (tempo == 0) {
            fieldx = 0;
            fferror("Tempo 100 will be used here");
            tempo = 100L;
        }
        start = thetime;
        /* adjust dur in case it is inherited by next note */
        if (symbolic_dur_flag) {
            dur = (dur * oldtempo);
            dur = dur / tempo;
        }
    }
}

/****************************************************************************
*               dotime
* Effect: parses a time (T) command
* Implementation: see implementation of donextdur()
****************************************************************************/

private void dotime(void)
{
    if (isdigit(token[fieldx])) {
        thetime = precise(scanint());
        thetime = scale(thetime, (ulong)time_scale, rate);
        if (token[fieldx] )
            fferror("Only digits were expected here");
    } else {
        fieldx++;
        thetime = dodur(); 
    }
    thetime += start;    /* time is relative to start */
}

/****************************************************************************
*               dovoice
* Effect: parse a voice (V) command (the voice is the MIDI channel)
****************************************************************************/

private void dovoice(void)
{
    if (isdigit(token[fieldx])) {
        voice = (int) scanint();
        if (token[fieldx])
            fferror("V must be followed by digits only");
        if (voice > MAX_CHANNELS) {
                char msg[40];
                sprintf(msg, "number too high, using %d instead", MAX_CHANNELS);
            fferror(msg);
            voice = MAX_CHANNELS;
        } 
        else if (voice < 1) {
            fferror("number too low, using 1 instead");
            voice = 1;
        }
    } 
    else fferror("No digit after V");
}

/****************************************************************************
*               fferror
* Inputs:
*    char *s: an error message string
* Effect:
*    prints the line with the error
*    puts a cursor (^) at the error location
*    prints the error message (s)
* Implementation:
*    this routine prints a carat under the character that
*    was copied into token[fieldx].    E.g. if fieldx = 0, the
*    carat will point to the first character in the field.
****************************************************************************/

private void fferror(char *s)
{
    gprintf(ERROR, "%3d | %s", lineno, line);
    marker((int) (linex - strlen(token) + fieldx + 1 + 6));
    gprintf(ERROR, "Error: %s.\n", s);
}

/****************************************************************************
*               init
* Effect:
*    initializes the state variables
****************************************************************************/

private void init(void)
{
    int i;

    end_flag = FALSE;

    /* initial (default) values for all state variables */
    symbolic_dur_flag = TRUE; /* default dur is symbolic */
    for (i = 0; i < nctrl; i++) {
        /* no initial control changes */
        ctrlflag[i] = FALSE;
        ctrlval[i] = 0;
    }

    lineno = 0;
    pitch = seq_dflt_pitch;
    loud = seq_dflt_loud;
    voice = seq_dflt_voice;
    time_scale = 1000L;
    tempo = 100L;
    rate = 100L;
    dur = precise(600); /* default dur is quarter note */
    thetime = precise(0);
    start = thetime;
    ntime = 0L;
    ticksize = 0L;
    artic = 100;
}

/****************************************************************************
*               ins_a_note
* Returns:
*    boolean: TRUE on success, FALSE if not enough memory
* Effect:
*    note events (if any) corresponding to the current line are inserted
* Implementation:
*    if a note on should occur after a note off and doesn't, and the
*    two notes have the same pitch, then the note off can cancel the
*    note on.  to make it unlikely that roundoff will cause this situation,
*    dur is decreased by one half of a clock tick before rounding.
*    also, phase2 gives precedence to note-offs that are simultaneous
*    with note-ons.
****************************************************************************/

private boolean ins_a_note(void)
{
    long the_dur = (trunc(dur) * artic + 50) / 100;
    int the_pitch = pitch;
    event_type note;
    if (rest_flag) the_pitch = NO_PITCH;
    note = insert_note(the_score, seqround(thetime), lineno, voice,
                       the_pitch, the_dur, loud);
    if (!note) return FALSE;
    return TRUE;    /* success! */
}

/****************************************************************************
*               ins_ctrls
* Returns:
*    boolean: TRUE on success, FALSE if not enough memory
* Effect:
*    control events corresponding to current line are inserted in score
* Implementation:
*    ctrlflag[i] is TRUE if control i was specified in this line, so
*    insert one control change for each ctrlflag[i] that is TRUE
****************************************************************************/

private boolean ins_ctrls(void)
{
    int i;
    event_type ctrl;

    for (i = 1; i < nctrl; i++) {
        if (ctrlflag[i]) {
            ctrl = insert_ctrl(the_score, seqround(thetime), lineno, i, voice,
                               ctrlval[i]);
            if (!ctrl) return FALSE;
            ctrlflag[i] = FALSE;
            ctrlval[i] = 0;
        }
    }
    return TRUE;    /* success! */
}

/****************************************************************************
*               issymbol
* Outputs: returns symbol number, or -1 if no match
* Assumes: token[1] has the symbol to look up (token[0] == '!')
****************************************************************************/

private int issymbol(void)
{
    int i, symb_num;
    char *sym;

    for (symb_num = 0; symb_num < sym_n; symb_num++) {
        sym = ssymbols[symb_num];
        i = 1;
        while (TRUE) {
            if (token[i] != *sym) break;
            if (*sym == 0) return symb_num;
            sym++; 
            i++;
        }
    }
    return -1;
}

/****************************************************************************
*               marker
* Inputs:
*    int count: the number of characters to indent
* Effect: 
*    prints a carat (^) at the position specified on file stderr
****************************************************************************/

private void marker(int count)
{
    int i;
    char s[128];
    for (i=0; i<(count-1); s[i++]=' ') /* */ ;
    s[count-1] = '^';
    s[count] = '\0';
    gprintf(ERROR,"%s\n",s);
}

/*****************************************************************
*           parseend
* Effect:
*    parse the note terminator, either ",", ";", EOS or "\n"
*
****************************************************************/

private void parseend(void)
{
    boolean done = FALSE;
    while (!done) {
        linex += scan1(&line[linex]);
        switch (token[0]) {
        case ',':
            ndurp = TRUE;    /* switch that next time was specified */
            ntime = 0L;
            done = TRUE;
            break;
        case ';':
        case '\n':
        case EOS:
            done = TRUE;
            break;
        case ' ':
        case '\t':
            break;      /* skip over blanks and scan1 again */
        default:
            fferror("Unexpected token");
            linex += scan();  /* flush the token */
            break;
        }
    }
}

/****************************************************************************
*               parsefield
* Effect: looks at first character of token and calls a parsing routine
*
****************************************************************************/

private void parsefield(void)
{
    fieldx = 1;
    switch (token[0]) {
    case 'T' : 
        dotime(); 
        break;
    case 'U':
    case 'W': 
    case 'H':
    case 'Q':
    case 'S':
    case 'I': 
    case '%':
    case '^':
        dur = dodur(); 
        break;
    case 'R': 
        do_a_rest(); 
        break;
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G': 
        pitch = dopitch(); 
        pitch_flag = TRUE;
        break;
    case 'P': 
        pitch = doabspitch(); 
        pitch_flag = TRUE;
        break;
    case 'L': 
        loud = doloud(); 
        break;
    case 'N': 
        donextdur(); 
        break;
/*    case 'J': 
 *      doctrl(1);
 *      break;
 */
    case 'K': 
        doctrl(PSWITCH_CTRL);
        break;
    case 'M': 
        doctrl(MODWHEEL_CTRL);
        break;
    case 'O': 
        doctrl(TOUCH_CTRL);
        break;
    case 'X': 
        doctrl(VOLUME_CTRL);
        break;
    case 'Y': 
        doctrl(BEND_CTRL);
        break;
    case 'Z':
        doprogram();
        break;
    case 'V': 
        dovoice();
        break;
    case '~':
        domacro();
        break;
    case '*':
        docomment();
        break;
    case '#':
        doartic();
        break;
    default : 
        doerror();
        break;
    }
}

/****************************************************************************
*               parsenote
* Effect: 
*    parses a note line -- control events (if any) and note event (if
*    present) are inserted into score
* Assumes:
*    line contains a string to be parsed
****************************************************************************/

private boolean parsenote(void)
{
    boolean out_of_memory = FALSE;
    int i;

    ndurp = FALSE;
    rest_flag = FALSE;

    /* this loop reads tokens for a note */
    while (token[0]) {
        parsefield();
        linex += scan();
    }

    parseend(); /* take care of note terminator */

    /*
     * insert ctrl's first so that will come before the note.
     */
    if (ctrlflag[0]) {
        out_of_memory |= !ins_ctrls();
        /* don't reset ctrlflag[0], it's used below */
    }

    /*
     * insert macro's
     */
    for (i = 0; i < macctrlx; i++) {
        event_type ctrl;
        if (macctrldef[i] == NULL) {
            ctrl = insert_macctrl(the_score, seqround(thetime), lineno,
                                  macctrlnum[i], voice, macctrlparmx[i]);
        } else {
            ctrl = insert_macro(the_score, seqround(thetime), lineno,
                        macctrldef[i], voice, macctrlnum[i],
                        &(macctrlparms[macctrlparmx[i]]));
        }
        out_of_memory |= (ctrl == NULL);
    }

    /* insert a note if
         *    (1) a pitch was specified OR
         *    (2) no control was specified and this is not a rest 
         *      (it's a pitch by default)
         *
         * NOTE: program changes during rests are advised since
         *    synthesizers may not be able to process a program
         *    change followed immediately by a note-on.  In fact, this
         *    is why we insert notes whose pitch is NO_PITCH -- so that
         *    the program change can be processed during the rest.
         */
    if (pitch_flag ||
        (!ctrlflag[0] && !rest_flag && (macctrlx == 0))) {
        out_of_memory |= !ins_a_note();
    }

    if (ndurp) thetime += ntime;
    else thetime += dur;

    return out_of_memory;
}


private boolean parseparm(long *valptr)
{
    register char c = token[fieldx];
    if (isdigit(c) || c == '-') {
        *valptr = scansgnint();
        return TRUE;
    } else {
        switch (c) {
          case 'P':
            fieldx++;
            *valptr = doabspitch();
             return TRUE;
          case 'A':
          case 'B':
          case 'C':
          case 'D':
          case 'E':
          case 'F':
          case 'G':
            fieldx++;
            *valptr = dopitch();
            return TRUE;
          case 'U':
          case 'W':
          case 'H':
          case 'Q':
          case 'I':
          case 'S':
          case '%':
          case '^':
            fieldx++;
            *valptr = seqround(dodur());
            return TRUE;
          case 'L':
            fieldx++;
            *valptr = doloud();
            return TRUE;
          case '\'':
            fieldx++;
            *valptr = token[fieldx];
            fieldx++;
            if (token[fieldx] != '\'') {
                fferror("single quote expected");
            }
            fieldx++;
            return TRUE;
          default:
            fferror("Parameter expected");
            return FALSE;
        }
    }
}



/****************************************************************************
*               scale
* Inputs:
*    time_type x
*    int (ulong?) n, d
* Outputs:
*    returns time_type: result of scaling x by n/d
****************************************************************************/

public time_type scale(x, n, d)
  ulong x;
  ulong n, d;
{
    ulong lo = (x & 0xFFFFL) * n;
    ulong hi = (x >> 16) * n;
    ulong res = hi / d;
    lo = (((hi - (res * d)) << 16) + lo + (d >> 1)) / d;
    return (time_type)( (res << 16) + lo );
}

/****************************************************************************
*               scan
* Inputs:
*    char *start: the string to scan
* Outputs:
*    returns int: the index of the next char in start to scan
* Effect: 
*    skips over leading blanks
*    copies characters from start into token, converting to upper case
*    scanning stops on delimiter: one of space, tab, newline, semicolon
****************************************************************************/

private int scan(void)
{
    char *start = line + linex;
    register char c;
    register int i = 0;
    register int j = 0;
    register int parens = 0;

    while (((c = start[i]) == ' ') || (c == '\t')) i++;

    while ((c = start[i]) != ' ' && c != '\n' && c != '\t' && c != EOS &&
           (c != ',' || token[0] == '~' || parens > 0) && c != ';') {

        if (islower(start[i])) token[j] = toupper(start[i]);
        else token[j] = start[i];
        if (c == '(') parens++;
        else if (c == ')') parens--;
        j++; 
        i++;
    }
    token[j] = '\0';

    fieldx = 0;
    if (parens) fferror("Unbalanced parens");

    return i;
}

/****************************************************************************
*               scan1
* Inputs:
*    char *start: the string to scan
* Outputs:
*    returns int: the index of the next char in start to scan
* Effect: 
*    copies one char from start into token, converting to upper case
****************************************************************************/

private int scan1(char *start)
{
    int i = 0;

    token[0] = *start;
    if (islower(token[0])) token[0] = toupper(token[0]);

    if (!nullstring(token)) {
        token[1] = '\0';
        i = 1;
    }
    fieldx = 0;
    return i;
}

/****************************************************************************
*               scanint
* Outputs:
*    returns long: the scanned integer
* Effect:
*    scans an unsigned long from token, starting at fieldx
*    fieldx is incremented to end of the integer
****************************************************************************/

private long scanint(void)
{
    long i = 0;
    char c;
    while ((c = token[fieldx])) {
        if (isdigit(c)) {
            i = (i*10) + (c - '0');
            fieldx++;
        } else return i;
    }
    return i;
}

private long scansgnint(void)
{
    if (token[fieldx] == '-') {
        fieldx++;
        return -scanint();
    } else {
        if (token[fieldx] == '+') {
            fieldx++;
        }
        return scanint();
    }
}


/* scansymb -- scan a symbol from the token */
/**/
private void scansymb(char *str)
{
    char c;
    while ((c = token[fieldx])) {
        if (isdigit(c) || isalpha(c) || c == '_') {
            *str++ = c;
            fieldx++;
        } else break;
    }
    *str = EOS;
}

/****************************************************************************
*               seq_read
* Inputs:
*    seq_type seq: the sequence to receive the score
*    FILE *fp: input file
* Outputs:
*    none
* Effect: 
*    parses score from input file and builds score data structure
****************************************************************************/

void seq_read(seq, fp)
  seq_type seq;
  FILE *fp;
{
    boolean out_of_memory = FALSE;    /* set when no more memory */
    /* printf("seq_read: chunklist is 0x%x\n", seq->chunklist); */
    the_score = seq;  /* current sequence is a global within this module */
    if (!seq) return;
    init();
    lineno = 0;
        
    /* make sure line is well terminated or scan might run off the end */
    line[linesize - 1] = EOS;
    line[linesize - 2] = '\n';

    /* this loop reads lines */
    while ((fgets(line, linesize - 2, fp) != NULL) && !out_of_memory &&
           !check_aborted() && !end_flag) {
        lineno++;
        linex = 0;
        /* this loop reads notes from a line */
        while ((line[linex] != EOS) && !out_of_memory) {
            /* loop invariant: line[linex] is first char of next note */
            ctrlflag[0] = FALSE;  /* other ctrlflags are reset by ins_ctrls() */
            macctrlx = 0;
            macctrlnextparm = 0;
            pitch_flag = FALSE;
            linex += scan();
            if (!nullstring(token)) {
                if (token[0] == '*') docomment();
                else if (token[0] == '!') dospecial();
                else out_of_memory = parsenote();
            } 
            else parseend();
        }
    }

    if (out_of_memory) {
        gprintf(ERROR, "Out of note memory at line %d,\n", lineno-1);
        gprintf(ERROR, "    the rest of your file will be ignored.\n");
    }

    if (check_aborted()) {
        gprintf(ERROR, "User aborted score input,\n");
        gprintf(ERROR, "    the rest of your file will be ignored.\n");
        if (abort_flag == BREAK_LEVEL) abort_flag = 0;
    }

    /* fclose(fp); -- don't close the file; if you do, Nyquist's garbage
       collector will close Nyquist's copy, and closing the file twice
       in Linux will crash Nyquist */

    gprintf(TRANS, "\nLoaded Adagio file with %ld note(s), %ld ctrl(s).\n\n",
            seq_notecount(the_score), seq_ctrlcount(the_score));
}
