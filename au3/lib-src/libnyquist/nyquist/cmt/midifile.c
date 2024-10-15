/*
 * Read a Standard MIDI File.  Externally-assigned function pointers are
 * called upon recognizing things in the file.  See midifile(3).
 */

/*****************************************************************************
* Change Log
*   Date    | who : Change
*-----------+-----------------------------------------------------------------
*  2-Mar-92 | GWL : created changelog; MIDIFILE_ERROR to satisfy compiler
* 28-Apr-03 | DM  : changed #includes and give return types for portability
*****************************************************************************/

#include "switches.h"

#include <stdio.h>
#include <stdlib.h>

#include "mfmidi.h"
#include "midifile.h"
#include "cext.h"
#include "userio.h"
#include "string.h"

#define MIDIFILE_ERROR -1

#ifdef PROTOTYPES
#define NOARGS void
#else
#define NOARGS
#endif

/* public stuff */
extern int abort_flag;

/* Functions to be called while processing the MIDI file. */
void (*Mf_starttrack)(NOARGS) = 0;
void (*Mf_endtrack)(NOARGS) = 0;
int (*Mf_getc)(NOARGS) = 0;
void (*Mf_eot)(NOARGS) = 0;
#ifdef PROTOTYPES
void (*Mf_error)(char *) = 0;
void (*Mf_header)(int,int,int) = 0;
void (*Mf_on)(int,int,int) = 0;
void (*Mf_off)(int,int,int) = 0;
void (*Mf_pressure)(int,int,int) = 0;
void (*Mf_controller)(int,int,int) = 0;
void (*Mf_pitchbend)(int,int,int) = 0;
void (*Mf_program)(int,int) = 0;
void (*Mf_chanpressure)(int,int) = 0;
void (*Mf_sysex)(int,char*) = 0;
void (*Mf_arbitrary)(int,char*) = 0;
void (*Mf_metamisc)(int,int,char*) = 0;
void (*Mf_seqnum)(int) = 0;
void (*Mf_smpte)(int,int,int,int,int) = 0;
void (*Mf_timesig)(int,int,int,int) = 0;
void (*Mf_tempo)(int) = 0;
void (*Mf_keysig)(int,int) = 0;
void (*Mf_sqspecific)(int,char*) = 0;
void (*Mf_text)(int,int,char*) = 0;
#else
void (*Mf_error)() = 0;
void (*Mf_header)() = 0;
void (*Mf_on)() = 0;
void (*Mf_off)() = 0;
void (*Mf_pressure)() = 0;
void (*Mf_controller)() = 0;
void (*Mf_pitchbend)() = 0;
void (*Mf_program)() = 0;
void (*Mf_chanpressure)() = 0;
void (*Mf_sysex)() = 0;
void (*Mf_arbitrary)() = 0;
void (*Mf_metamisc)() = 0;
void (*Mf_seqnum)() = 0;
void (*Mf_smpte)() = 0;
void (*Mf_tempo)() = 0;
void (*Mf_timesig)() = 0;
void (*Mf_keysig)() = 0;
void (*Mf_sqspecific)() = 0;
void (*Mf_text)() = 0;
#endif

int Mf_nomerge = 0;             /* 1 => continue'ed system exclusives are */
                                /* not collapsed. */
long Mf_currtime = 0L;          /* current time in delta-time units */
int Mf_skipinit = 0;            /* 1 if initial garbage should be skipped */

/* private stuff */

static long Mf_toberead = 0L;

static long readvarinum(NOARGS);
static long read32bit(NOARGS);
static int read16bit(NOARGS);
static void msgenlarge(NOARGS);
static char *msg(NOARGS);
static int readheader(NOARGS);
static void readtrack(NOARGS);
static void sysex(NOARGS), msginit(NOARGS);
static int egetc(NOARGS);
static int msgleng(NOARGS);

#ifdef PROTOTYPES
static int readmt(char*,int);
static long to32bit(int,int,int,int);
static int to16bit(int,int);
static void mferror(char *);
static void badbyte(int);
static void metaevent(int);
static void msgadd(int);
static void chanmessage(int,int,int);
#else
static long to32bit();
static int to16bit();
static void mferror();
static void badbyte();
static void metaevent();
static void msgadd();
static void chanmessage();
#endif

static int midifile_error;

void
midifile()              /* The only non-static function in this file. */
{
        int ntrks;
        midifile_error = 0;

        if ( Mf_getc == 0 ) {
                mferror("mf.h() called without setting Mf_getc"); 
                return;
        }
        ntrks = readheader();
        if (midifile_error) return;
        if ( ntrks <= 0 ) {
                mferror("No tracks!");
                /* no need to return since midifile_error is set */
        }
        while ( ntrks-- > 0 && !midifile_error && !check_aborted())
                readtrack();
        if (check_aborted()) {
                mferror("Midifile read aborted\n\
\tthe rest of your file will be ignored.\n");
            if (abort_flag == BREAK_LEVEL) abort_flag = 0;
        }
}

static int
readmt(s,skip)          /* read through the "MThd" or "MTrk" header string */
char *s;
int skip;               /* if 1, we attempt to skip initial garbage. */
{
        int nread = 0;
        char b[4];
        char buff[32];
        int c;
        char *errmsg = "expecting ";

    retry:
        while ( nread<4 ) {
                c = (*Mf_getc)();
                if ( c == EOF ) {
                        errmsg = "EOF while expecting ";
                        goto err;
                }
                b[nread++] = c;
        }
        /* See if we found the 4 characters we're looking for */
        if ( s[0]==b[0] && s[1]==b[1] && s[2]==b[2] && s[3]==b[3] )
                return(0);
        if ( skip ) {
                /* If we are supposed to skip initial garbage, */
                /* try again with the next character. */
                b[0]=b[1];
                b[1]=b[2];
                b[2]=b[3];
                nread = 3;
                goto retry;
        }
    err:
        (void) strcpy(buff,errmsg);
        (void) strcat(buff,s);
        mferror(buff);
        return(0);
}

static int
egetc()                 /* read a single character and abort on EOF */
{
        int c = (*Mf_getc)();

        if ( c == EOF ) {
                mferror("premature EOF");
                return EOF;
        }
        Mf_toberead--;
        return(c);
}

static int
readheader()            /* read a header chunk */
{
        int format, ntrks, division;

        if ( readmt("MThd",Mf_skipinit) == EOF )
                return(0);

        Mf_toberead = read32bit();
        if (midifile_error) return MIDIFILE_ERROR;
        format = read16bit();
        if (midifile_error) return MIDIFILE_ERROR;
        ntrks = read16bit();
        if (midifile_error) return MIDIFILE_ERROR;
        division = read16bit();
        if (midifile_error) return MIDIFILE_ERROR;

        if ( Mf_header )
                (*Mf_header)(format,ntrks,division);

        /* flush any extra stuff, in case the length of header is not 6 */
        while ( Mf_toberead > 0 && !midifile_error)
                (void) egetc();
        return(ntrks);
}

static void
readtrack()              /* read a track chunk */
{
        /* This array is indexed by the high half of a status byte.  It's */
        /* value is either the number of bytes needed (1 or 2) for a channel */
        /* message, or 0 (meaning it's not  a channel message). */
        static int chantype[] = {
                0, 0, 0, 0, 0, 0, 0, 0,         /* 0x00 through 0x70 */
                2, 2, 2, 2, 1, 1, 2, 0          /* 0x80 through 0xf0 */
        };
        long lookfor, lng;
        int c, c1, type;
        int sysexcontinue = 0;  /* 1 if last message was an unfinished sysex */
        int running = 0;        /* 1 when running status used */
        int status = 0;         /* (possibly running) status byte */
        int needed;

        if ( readmt("MTrk",0) == EOF )
                return;

        Mf_toberead = read32bit();

        if (midifile_error) return;

        Mf_currtime = 0L;

        if ( Mf_starttrack ){

                (*Mf_starttrack)();
        }
        while ( Mf_toberead > 0 ) {

                Mf_currtime += readvarinum();   /* delta time */
                if (midifile_error) return;

                c = egetc(); if (midifile_error) return;

                if ( sysexcontinue && c != 0xf7 ) {
                        mferror("didn't find expected continuation of a sysex");
                        return;
                }
                if ( (c & 0x80) == 0 ) {         /* running status? */
                        if ( status == 0 ) {
                                mferror("unexpected running status");
                                return;
                        }
                        running = 1;
                }
                else {
                        status = c;
                        running = 0;
                }

                needed = chantype[ (status>>4) & 0xf ];

                if ( needed ) {         /* ie. is it a channel message? */

                        if ( running )
                                c1 = c;
                        else {
                                c1 = egetc();
                                if (midifile_error) return;
                        }
                        chanmessage( status, c1, (needed>1) ? egetc() : 0 );
                        if (midifile_error) return;
                        continue;;
                }

                switch ( c ) {

                case 0xff:                      /* meta event */

                        type = egetc();
                        if (midifile_error) return;
                        /* watch out - Don't combine the next 2 statements */
                        lng = readvarinum();
                        if (midifile_error) return;
                        lookfor = Mf_toberead - lng;
                        msginit();

                        while ( Mf_toberead > lookfor ) {
                                char c = egetc();
                                if (midifile_error) return;
                                msgadd(c);
                        }
                        metaevent(type);
                        break;

                case 0xf0:              /* start of system exclusive */

                        /* watch out - Don't combine the next 2 statements */
                        lng = readvarinum();
                        if (midifile_error) return;
                        lookfor = Mf_toberead - lng;
                        msginit();
                        msgadd(0xf0);

                        while ( Mf_toberead > lookfor ) {
                                c = egetc();
                                if (midifile_error) return;
                                msgadd(c);
                        }
                        if ( c==0xf7 || Mf_nomerge==0 )
                                sysex();
                        else
                                sysexcontinue = 1;  /* merge into next msg */
                        break;

                case 0xf7:      /* sysex continuation or arbitrary stuff */

                        /* watch out - Don't combine the next 2 statements */
                        lng = readvarinum();
                        if (midifile_error) return;
                        lookfor = Mf_toberead - lng;

                        if ( ! sysexcontinue )
                                msginit();

                        while ( Mf_toberead > lookfor ) {
                                c = egetc();
                                if (midifile_error) return;
                                msgadd(c);
                        }
                        if ( ! sysexcontinue ) {
                                if ( Mf_arbitrary )
                                        (*Mf_arbitrary)(msgleng(),msg());
                        }
                        else if ( c == 0xf7 ) {
                                sysex();
                                sysexcontinue = 0;
                        }
                        break;
                default:

                        badbyte(c);

                        break;
                }
        }
        if ( Mf_endtrack )
                (*Mf_endtrack)();
        return;
}

static void
badbyte(c)
int c;
{
        char buff[32];

        (void) sprintf(buff,"unexpected byte: 0x%02x",c);
        mferror(buff);
}

static void
metaevent(int type)
{
        int leng = msgleng();
        char *m = msg();

        switch  ( type ) {
        case 0x00:
                if ( Mf_seqnum )
                        (*Mf_seqnum)(to16bit(m[0],m[1]));
                break;
        case 0x01:      /* Text event */
        case 0x02:      /* Copyright notice */
        case 0x03:      /* Sequence/Track name */
        case 0x04:      /* Instrument name */
        case 0x05:      /* Lyric */
        case 0x06:      /* Marker */
        case 0x07:      /* Cue point */
        case 0x08:
        case 0x09:
        case 0x0a:
        case 0x0b:
        case 0x0c:
        case 0x0d:
        case 0x0e:
        case 0x0f:
                /* These are all text events */
                if ( Mf_text )
                        (*Mf_text)(type,leng,m);
                break;
        case 0x2f:      /* End of Track */
                if ( Mf_eot )
                        (*Mf_eot)();
                break;
        case 0x51:      /* Set tempo */
                if ( Mf_tempo )
                        (*Mf_tempo)(to32bit(0,m[0],m[1],m[2]));
                break;
        case 0x54:
                if ( Mf_smpte )
                        (*Mf_smpte)(m[0],m[1],m[2],m[3],m[4]);
                break;
        case 0x58:
                if ( Mf_timesig )
                        (*Mf_timesig)(m[0],m[1],m[2],m[3]);
                break;
        case 0x59:
                if ( Mf_keysig )
                        (*Mf_keysig)(m[0],m[1]);
                break;
        case 0x7f:
                if ( Mf_sqspecific )
                        (*Mf_sqspecific)(leng,m);
                break;
        default:
                if ( Mf_metamisc )
                        (*Mf_metamisc)(type,leng,m);
        }
}

static void
sysex()
{
        if ( Mf_sysex )
                (*Mf_sysex)(msgleng(),msg());
}

static void
chanmessage(status,c1,c2)
int status;
int c1, c2;
{
        int chan = status & 0xf;

        switch ( status & 0xf0 ) {
        case NOTEOFF:
                if ( Mf_off )
                        (*Mf_off)(chan,c1,c2);
                break;
        case NOTEON:
                if ( Mf_on )
                        (*Mf_on)(chan,c1,c2);
                break;
        case PRESSURE:
                if ( Mf_pressure )
                        (*Mf_pressure)(chan,c1,c2);
                break;
        case CONTROLLER:
                if ( Mf_controller )
                        (*Mf_controller)(chan,c1,c2);
                break;
        case PITCHBEND:
                if ( Mf_pitchbend )
                        (*Mf_pitchbend)(chan,c1,c2);
                break;
        case PROGRAM:
                if ( Mf_program )
                        (*Mf_program)(chan,c1);
                break;
        case CHANPRESSURE:
                if ( Mf_chanpressure )
                        (*Mf_chanpressure)(chan,c1);
                break;
        }
}

/* readvarinum - read a varying-length number, and return the */
/* number of characters it took. */

static long
readvarinum()
{
        long value;
        int c;

        c = egetc();
        if (midifile_error) return 0;

        value = (long) c;
        if ( c & 0x80 ) {
                value &= 0x7f;
                do {
                        c = egetc();
                        if (midifile_error) return 0;
                        value = (value << 7) + (c & 0x7f);
                } while (c & 0x80);
        }
        return (value);
}

static long
to32bit(int c1, int c2, int c3, int c4)
{
        long value = 0L;

        value = (c1 & 0xff);
        value = (value<<8) + (c2 & 0xff);
        value = (value<<8) + (c3 & 0xff);
        value = (value<<8) + (c4 & 0xff);
        return (value);
}

static int
to16bit(int c1, int c2)
{
        return ((c1 & 0xff ) << 8) + (c2 & 0xff);
}

static long
read32bit()
{
        int c1, c2, c3, c4;

        c1 = egetc(); if (midifile_error) return 0;
        c2 = egetc(); if (midifile_error) return 0;
        c3 = egetc(); if (midifile_error) return 0;
        c4 = egetc(); if (midifile_error) return 0;
        return to32bit(c1,c2,c3,c4);
}

static int
read16bit()
{
        int c1, c2;
        c1 = egetc(); if (midifile_error) return 0;
        c2 = egetc(); if (midifile_error) return 0;
        return to16bit(c1,c2);
}

static void
mferror(s)
char *s;
{
        if ( Mf_error )
                (*Mf_error)(s);
        midifile_error = 1;
}

/* The code below allows collection of a system exclusive message of */
/* arbitrary length.  The Msgbuff is expanded as necessary.  The only */
/* visible data/routines are msginit(), msgadd(), msg(), msgleng(). */

#define MSGINCREMENT 128
static char *Msgbuff = 0;       /* message buffer */
static int Msgsize = 0;         /* Size of currently allocated Msg */
static int Msgindex = 0;        /* index of next available location in Msg */

static void
msginit()
{
        Msgindex = 0;
}

static char *
msg()
{
        return(Msgbuff);
}

static int
msgleng()
{
        return(Msgindex);
}

static void
msgadd(c)
int c;
{
        /* If necessary, allocate larger message buffer. */
        if ( Msgindex >= Msgsize )
                msgenlarge();
        Msgbuff[Msgindex++] = c;
}

static void
msgenlarge()
{
        char *newmess;
        char *oldmess = Msgbuff;
        int oldleng = Msgsize;

        Msgsize += MSGINCREMENT;
        newmess = MALLOC((sizeof(char)*Msgsize) );

        /* copy old message into larger new one */
        if ( oldmess != 0 ) {
                register char *p = newmess;
                register char *q = oldmess;
                register char *endq = &oldmess[oldleng];

                for ( ; q!=endq ; p++,q++ )
                        *p = *q;
                free(oldmess);
        }
        Msgbuff = newmess;
}
