/*
 * Read a Standard MIDI File.  Externally-assigned function pointers are
 * called upon recognizing things in the file.  See midifile(3).
 */

/*****************************************************************************
*       Change Log
*       Date    | who : Change
*-----------+-----------------------------------------------------------------
*  2-Mar-92 | GWL : created changelog; MIDIFILE_ERROR to satisfy compiler
*****************************************************************************/

#include "stdio.h"
#include "mfmidi.h"
#include "string.h"
#include "assert.h"

#define MIDIFILE_ERROR -1

/* public stuff */
extern int abort_flag;


void Midifile_reader::midifile()
{
    int ntrks;
    midifile_error = 0;

    ntrks = readheader();
    if (midifile_error) return;
    if (ntrks <= 0) {
        mferror("No tracks!");
        /* no need to return since midifile_error is set */
    }
    while (ntrks-- > 0 && !midifile_error) readtrack();
}

int Midifile_reader::readmt(const char *s, int skip)
    /* read through the "MThd" or "MTrk" header string */
    /* if skip == 1, we attempt to skip initial garbage. */
{
    assert(strlen(s) == 4); // must be "MThd" or "MTrk"
    int nread = 0;
    char b[4];
    char buff[32];
    int c;
    const char *errmsg = "expecting ";

    retry:
    while ( nread<4 ) {
        c = Mf_getc();
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
#pragma warning(disable: 4996) // strcpy is safe since strings have known lengths
    (void) strcpy(buff,errmsg);
    (void) strcat(buff,s);
#pragma warning(default: 4996) // turn it back on
    mferror(buff);
    return(0);
}

int Midifile_reader::egetc()
    /* read a single character and abort on EOF */
{
    int c = Mf_getc();

    if ( c == EOF ) {
        mferror("premature EOF");
        return EOF;
    }
    Mf_toberead--;
    return(c);
}

int Midifile_reader::readheader()
    /* read a header chunk */
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

    Mf_header(format,ntrks,division);

    /* flush any extra stuff, in case the length of header is not 6 */
    while ( Mf_toberead > 0 && !midifile_error)
        (void) egetc();
    return(ntrks);
}

void Midifile_reader::readtrack()
    /* read a track chunk */
{
    /* This array is indexed by the high half of a status byte.  It's */
    /* value is either the number of bytes needed (1 or 2) for a channel */
    /* message, or 0 (meaning it's not  a channel message). */
    static int chantype[] = {
        0, 0, 0, 0, 0, 0, 0, 0,       /* 0x00 through 0x70 */
        2, 2, 2, 2, 1, 1, 2, 0        /* 0x80 through 0xf0 */
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

    Mf_starttrack();

    while ( Mf_toberead > 0 ) {

        Mf_currtime += readvarinum();   /* delta time */
        if (midifile_error) return;

        c = egetc();
       if (midifile_error) return;

        if ( sysexcontinue && c != 0xf7 ) {
            mferror("didn't find expected continuation of a sysex");
            return;
        }
        if ( (c & 0x80) == 0 ) {        /* running status? */
            if ( status == 0 ) {
                mferror("unexpected running status");
                return;
            }
            running = 1;
       } else {
            status = c;
            running = 0;
        }

        needed = chantype[ (status>>4) & 0xf ];

        if ( needed ) {        /* ie. is it a channel message? */

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

        case 0xff:                  /* meta event */

            type = egetc();
            if (midifile_error) return;
            /* watch out - Don't combine the next 2 statements */
            lng = readvarinum();
            if (midifile_error) return;
            lookfor = Mf_toberead - lng;
            msginit();

            while ( Mf_toberead > lookfor ) {
                unsigned char c = egetc();
                if (midifile_error) return;
                msgadd(c);
            }
            metaevent(type);
            break;

        case 0xf0:            /* start of system exclusive */

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

        case 0xf7:     /* sysex continuation or arbitrary stuff */

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
                    Mf_arbitrary(msgleng(), msg());
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
        Mf_endtrack();
    return;
}

void Midifile_reader::badbyte(int c)
{
    char buff[32];
#pragma warning(disable: 4996) // safe in this case
    (void) sprintf(buff,"unexpected byte: 0x%02x",c);
#pragma warning(default: 4996)
    mferror(buff);
}

void Midifile_reader::metaevent(int type)
{
    int leng = msgleng();
    // made this unsigned to avoid sign extend
    unsigned char *m = msg();

    switch ( type ) {
    case 0x00:
        Mf_seqnum(to16bit(m[0],m[1]));
        break;
    case 0x01:     /* Text event */
    case 0x02:     /* Copyright notice */
    case 0x03:     /* Sequence/Track name */
    case 0x04:     /* Instrument name */
    case 0x05:     /* Lyric */
    case 0x06:     /* Marker */
    case 0x07:     /* Cue point */
    case 0x08:
    case 0x09:
    case 0x0a:
    case 0x0b:
    case 0x0c:
    case 0x0d:
    case 0x0e:
    case 0x0f:
        /* These are all text events */
        Mf_text(type,leng,m);
        break;
    case 0x20:
        Mf_chanprefix(m[0]);
        break;
    case 0x21:
        Mf_portprefix(m[0]);
        break;
    case 0x2f:     /* End of Track */
        Mf_eot();
        break;
    case 0x51:     /* Set tempo */
        Mf_tempo(to32bit(0,m[0],m[1],m[2]));
        break;
    case 0x54:
        Mf_smpte(m[0],m[1],m[2],m[3],m[4]);
        break;
    case 0x58:
        Mf_timesig(m[0],m[1],m[2],m[3]);
        break;
    case 0x59:
        Mf_keysig(m[0],m[1]);
        break;
    case 0x7f:
        Mf_sqspecific(leng,m);
        break;
    default:
        Mf_metamisc(type,leng,m);
    }
}


void Midifile_reader::sysex()
{
    Mf_sysex(msgleng(), msg());
}


void Midifile_reader::chanmessage(int status, int c1, int c2)
{
    int chan = status & 0xf;

    switch ( status & 0xf0 ) {
    case NOTEOFF:
        Mf_off(chan,c1,c2);
        break;
    case NOTEON:
        Mf_on(chan,c1,c2);
        break;
    case PRESSURE:
        Mf_pressure(chan,c1,c2);
        break;
    case CONTROLLER:
        Mf_controller(chan,c1,c2);
        break;
    case PITCHBEND:
        Mf_pitchbend(chan,c1,c2);
        break;
    case PROGRAM:
        Mf_program(chan,c1);
        break;
    case CHANPRESSURE:
        Mf_chanpressure(chan,c1);
        break;
    }
}

/* readvarinum - read a varying-length number, and return the */
/* number of characters it took. */

long Midifile_reader::readvarinum()
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

long Midifile_reader::to32bit(int c1, int c2, int c3, int c4)
{
    long value = 0L;

    value = (c1 & 0xff);
    value = (value<<8) + (c2 & 0xff);
    value = (value<<8) + (c3 & 0xff);
    value = (value<<8) + (c4 & 0xff);
    return (value);
}

int Midifile_reader::to16bit(int c1, int c2)
{
    return ((c1 & 0xff ) << 8) + (c2 & 0xff);
}

long Midifile_reader::read32bit()
{
    int c1, c2, c3, c4;

    c1 = egetc(); if (midifile_error) return 0;
    c2 = egetc(); if (midifile_error) return 0;
    c3 = egetc(); if (midifile_error) return 0;
    c4 = egetc(); if (midifile_error) return 0;
    return to32bit(c1,c2,c3,c4);
}

int Midifile_reader::read16bit()
{
    int c1, c2;
    c1 = egetc(); if (midifile_error) return 0;
    c2 = egetc(); if (midifile_error) return 0;
    return to16bit(c1,c2);
}

void Midifile_reader::mferror(const char *s)
{
    Mf_error(s);
    midifile_error = 1;
}

/* The code below allows collection of a system exclusive message of */
/* arbitrary length.  The Msgbuff is expanded as necessary.  The only */
/* visible data/routines are msginit(), msgadd(), msg(), msgleng(). */

#define MSGINCREMENT 128

Midifile_reader::Midifile_reader()
{
    Mf_nomerge = 0;
    Mf_currtime = 0L;
    Mf_skipinit = 0;
    Mf_toberead = 0;

    Msgbuff = 0;      /* message buffer */
    Msgsize = 0;        /* Size of currently allocated Msg */
    Msgindex = 0;       /* index of next available location in Msg */
}

void Midifile_reader::finalize()
{
    if (Msgbuff) Mf_free(Msgbuff, Msgsize);
    Msgbuff = NULL;
}


void Midifile_reader::msginit()
{
    Msgindex = 0;
}

unsigned char *Midifile_reader::msg()
{
    return(Msgbuff);
}

int Midifile_reader::msgleng()
{
    return(Msgindex);
}

void Midifile_reader::msgadd(int c)
{
    /* If necessary, allocate larger message buffer. */
    if ( Msgindex >= Msgsize )
        msgenlarge();
    Msgbuff[Msgindex++] = c;
}

void Midifile_reader::msgenlarge()
{
    unsigned char *newmess;
    unsigned char *oldmess = Msgbuff;
    int oldleng = Msgsize;

    Msgsize += MSGINCREMENT;
    newmess = (unsigned char *) Mf_malloc((sizeof(unsigned char) * Msgsize) );

    /* copy old message into larger new one */
    if ( oldmess != 0 ) {
        memcpy(newmess, oldmess, oldleng);
        Mf_free(oldmess, oldleng);
    }
    Msgbuff = newmess;
}
