/*****************************************************************************
*                            midifns.c
* Copyright 1989 Carnegie Mellon University
*  Date | Change
*-----------+-----------------------------------------------------------------
* 29-Mar-88 | Created from IBM PC version of mpu.c
*           | Added settime()
* 02-May-88 | AMIGA  2000 version. portable version.
* 12-Oct-88 | JCD : Exclusive AMIGA Version.
* 13-Apr-89 | JCD : New portable version.
* 19-Apr-89 | JCD : Amiga CAMD Version added.
*  5-Apr-91 | JDW : Further modification
* 17-Feb-92 | GWL : incorporate JMN's new mpu.c
*  8-Jun-92 | JDZ : add support for ITC midi interface
* 16-Dec-92 | RBD : replace JMN's mpu.c with LMS's mpu.c
* 11-Mar-94 | PLu : port to  IRIX
* 25-Apr-97 | RBD : it looks like SGI changed their interface.  I
*	        |       made it compile again, but MIDI does not work, so
*	        |       took out calls to actually send/recv MIDI data
* 28-Apr-03 | DM  : Renamed random -> cmtrand, true->TRUE, false->FALSE
*           |       Use features rather than system names in #ifdef's
*****************************************************************************/

#include "switches.h"

#ifdef UNIX
#include <sys/resource.h>
#include <sys/param.h>
#ifndef OPEN_MAX
/* this is here for compiling the UNIX version under AIX. This is a BSDism */
#define OPEN_MAX 2000
#endif /* OPEN_MAX */
#endif /* UNIX */

#ifdef UNIX_MACH
#include "machmidi.h"
#endif


#ifdef AMIGA
#ifdef AZTEC
#include "functions.h"
#endif /* AZTEC */
#include "midi/camd.h"
#include "clib/camd_protos.h"
/* note: azt_camd_pragmas.h was produced by running MAPFD on the
 * lib/camd_lib.fd file included in the CAMD disk from Commodore.
 * The "CamdClient" calls are manually removed from 
 */
#ifdef AZTEC
#include "pragmas/azt_camd_pragmas.h"
#else /* !AZTEC */
#include "pragmas/lat_camd_pragmas.h"
#endif /* AZTEC */
#include "camdmidi.h"
#include "ctype.h"
#endif /* AMIGA */

#ifdef UNIX_IRIX
/* #define UNIX_IRIX_MIDIFNS -- this would enable actual midi I/O
 * if the actual midi I/O code worked
 */
/* IRIX changed the MIDI interface,
 * retain this for older systems:
 */
#ifdef UNIX_IRIX_MIDIFNS
#include <dmedia/midi.h>
#endif
#endif

#include "stdio.h"
#include "cext.h"
#include "midicode.h"
#include "cmdline.h"
#include "pitch.h"
#include "midifns.h"
#include "userio.h"
#include "string.h"
#ifdef MACINTOSH_OR_DOS
#ifndef WINDOWS
#include "midibuff.h"
#endif
#endif

#ifdef UNIX_ITC /* was ITC */
#include "sys/param.h"
/* since boolean is defined, block its definition in midistruct.h.
 * CMT defines boolean as ushort, but midistruct.h uses int.  
 * This is not a problem on RS/6000s, but beware!
 */
/* the following would be included if we had the BSD switch set.  I think
   we should try to avoid BSDisms; when fixed, the following should be
   removed
 */
#define NBBY 8
#include "sys/select.h" /* defines fd_set */
#define MIDI_HAS_BOOLEAN
#include "midistruct.h"
#include "cmtio.h"
#endif /* UNIX_ITC */

#ifdef  DOS
#ifndef WINDOWS
#include "timer.h"
#include "mpu.h"
#endif /* ifndef WINDOWS */
#endif /* ifdef DOS */

#ifndef BREAKTEST
#define BREAKTEST
#endif

#ifdef UNIX_MACH
#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>
#else
#ifdef UNIX
#ifndef UNIX_IRIX
#include "sys/time.h"
#ifndef __OpenBSD__
#include "sys/timeb.h"
#endif
#include "cmtio.h"
#else
#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>

#ifdef UNIX_IRIX_MIDIFNS
#include <midi.h>
#include <midiio.h>
#endif /* UNIX_IRIX_MIDIFNS */
#endif  /* UNIX_IRIX */
#endif /* UNIX */
#endif /* UNIX_MACH */

#ifdef ITC
static int ignore_realtime = 0;
#endif /* ITC */

#ifdef MACINTOSH

/* added for ThinkC 7: */
#include <OSUtils.h>

/* port numbers are in the range 0..MAX_PORTS-1 */
#define CHANNELS_PER_PORT 16
#define MAX_PORTS ((MAX_CHANNELS + CHANNELS_PER_PORT - 1) / CHANNELS_PER_PORT)

/* here are some MIDIMGR specific definitions */
#ifdef MIDIMGR
#include "MIDI.h"
#include "midimgr.h"

#define TICKS_TO_MS(t) t
#define MS_TO_TICKS(t) t

#else
/* here are some non-MIDIMGR definitions for the Mac */
/****************************************************************************
*
*       DMH: constants from macmidi.c
*
****************************************************************************/

/* the modem port, also called port A */
#define portA 0

/* the printer port, also called port B */
#define portB 1

/* a tick is 1/60 of a second
 *
 * the following tables and routines are used to convert
 * between ticks and milliseconds
 */
#define TICKS_TO_MS(t)  (((t) * 50) / 3)
#define MS_TO_TICKS(t)  (((t) * 3) / 50)
#endif  /* def MIDIMGR */
#endif  /* def MACINTOSH */

#ifdef WINDOWS
#define huge
#endif

/****************************************************************************
*
* exported flags
*
****************************************************************************/

boolean miditrace = FALSE;      /* enables printed trace of MIDI output */
boolean musictrace = FALSE;     /* enables printed trace of commands */
#ifdef MACINTOSH_OR_DOS
boolean ctrlFilter = TRUE;    /* suppress continuous controller data */
boolean exclFilter = TRUE;    /* suppress exclusive messages */
boolean realFilter = TRUE;    /* suppress realtime messages */
#endif

/****************************************************************************
*
* exported variables
*
****************************************************************************/

public int keyloud;    /* set to velocity of last getkey event */
/* public long error; */
public short midi_error_flags = 0;

/* The following midifns_syntax lists command line switches and options.
   Since these are machine dependent, use conditional compilation.
   Conditional compilation within a string is a bit tricky: you want to
   write "\" for line continuation within the string, but "\" gets eaten
   by the macro preprocessor.
   That's why we define macros like AMIGAINPORT.
   Regretably it doesn't work for all compilers.
 */
 

/* Lattice and RT/Unix aren't happy expanding the embedded macros below, so
   I made a separate declaration of midifns_syntax for Unix
 */
#ifdef UNIX
public char *midifns_syntax = "block<s>Turn off midi THRU;\
    miditrace<s>Trace low-level midi functions;\
    noalloff<s>Do not send alloff message when done;\
    trace<s>Trace music operations;\
    tune<o>Load a tuning file";
#else
#ifdef MACINTOSH
#ifdef MIDIMGR
public char *midifns_syntax = "miditrace<s>Trace low-level midi functions;\
    noalloff<s>Do not send alloff message when done;\
    patch<s>Remember/reuse Midi Mgr patches;\
    trace<s>Trace music operations;\
    keep<s>Keep other processes running;\
    tune<o>Load a tuning file";
#else /* no MIDIMGR */
public char *midifns_syntax = "miditrace<s>Trace low-level midi functions;\
    noalloff<s>Do not send alloff message when done;\
    patch<s>Remember/reuse Midi Mgr patches;\
    trace<s>Trace music operations;\
    tune<o>Load a tuning file";		
#endif /* MIDIMGR */
#else 
#ifdef AMIGA
public char *midifns_syntax = "block<s>Turn off midi THRU;\
    inport<o>Inpur port number;\
    miditrace<s>Trace low-level midi functions;\
    noalloff<s>Do not send alloff message when done;\
    outport<o>Output port number;\
    trace<s>Trace music operations;\
    tune<o>Load a tuning file";
#else /* not UNIX or MACINTOSH or MIDIMGR or AMIGA */
#ifdef DOS
public char *midifns_syntax = "miditrace<s>Trace low-level midi functions;\
    noalloff<s>Do not send alloff message when done;\
    trace<s>Trace music operations;\
    tune<o>Load a tuning file";
#endif /* DOS */
#endif /* AMIGA */
#endif /* MACINTOSH */
#endif /* UNIX */

#ifdef MACINTOSH
boolean do_midi_thru = FALSE; /* exported: copy midi in to midi out */
#endif


/****************************************************************************
*
* local module variables
*
****************************************************************************/

private int initialized = FALSE;   /* set by musicinit, cleared by musicterm */
private boolean tune_flag = FALSE; /* set by musicinit, never cleared */
#ifdef DOS
private boolean metroflag = FALSE; /* flag to turn on metronome */
#endif
private int user_scale = FALSE;    /* TRUE if user-defined scale */
private int bend[MAX_CHANNELS];    /* current pitch bend on channel */
short cur_midi_prgm[MAX_CHANNELS];
private pitch_table pit_tab[128];  /* scale definition */

#ifdef DOS
private ulong timeoffset = 0;
public boolean exclerr = FALSE;
public byte xcodemask; /* mask (00 or FF) */
public byte xcode; /* mfr code */
#endif

#ifdef MACINTOSH_OR_DOS
boolean sysex_pending = FALSE;
#endif

#ifdef AMIGA

#define CONTCONT ((CMF_Ctrl & ~CMF_CtrlSwitch) | CMF_PitchBend | \
        CMF_ChanPress)
#endif  /* def AMIGA */

#ifdef UNIX
private ulong timeoffset = 0;
#endif

#ifdef UNIX_IRIX_MIDIFNS
static MIport *miport;
static int ignore_realtime = 0;

private byte *sysex_p;
private int sysex_n;
#endif 

#ifdef ITC
mi_id midiconn;
#endif

#ifdef MACINTOSH
private ulong ticksAtStart = 0L;
    /* clock ticks at time of last musicinit or timereset
     * ASSUME: tick clock never wraps.  this is a good assumption, since
     * the tick clock is set to zero when the power is turned on and the
     * tick counter is 32 bits.  the Macintosh would need to be on for
     * 828.5 days for the tick counter to wrap around! */

#endif  /* def MACINTOSH */

/****************************************************************************
*
* functions declared in this module
*
****************************************************************************/

private void fixup(void);
private void midi_init(void);
extern boolean check_ascii(void); /*userio.c*/
private void musicterm(void);


/****************************************************************************
*                alloff
* Inputs:
*    none
* Effect: 
*    Sends MIDI all notes off command on every channel.
****************************************************************************/

#define ALL_NOTES_OFF 0x7B /*DMH: from macmidi.c*/

void alloff(void)
{
    int c;

    if (!initialized) fixup();
    if (musictrace)
    gprintf(TRANS,"alloff()\n");
    for (c = 1; c <= MAX_CHANNELS; c++) {
    midi_write(3, MIDI_PORT(c), (byte) (0xb0 | MIDI_CHANNEL(c)), ALL_NOTES_OFF, 0);
    }
}



/***************************************************************
*                           eventwait
*
* Input : wakeup time, -1 means forever
* Output : none
* Return: none
* Effect: waits until ascii or midi input or timeout
***************************************************************/

#ifdef UNIX_ITC
void eventwait(long timeout)
{
    struct timeval unix_timeout;
    struct timeval *waitspec = NULL;
    fd_set readfds;
    struct rlimit file_limit;

    FD_ZERO(&readfds);
    FD_SET(MI_CONNECTION(midiconn), &readfds);
    FD_SET(fileno(stdin), &readfds);
    if (timeout >= 0) {
    timeout -= gettime();   /* convert to millisecond delay */
    unix_timeout.tv_sec = timeout / 1000;
    /* remainder become microsecs: */
    unix_timeout.tv_usec = (timeout - (unix_timeout.tv_sec * 1000)) * 1000;
    waitspec = &unix_timeout;
    }
    getrlimit(RLIMIT_NOFILE, &file_limit);
    select(file_limit.rlim_max+1, &readfds, 0, 0, waitspec);
    return;
}
#else /* !UNIX_ITC */
#ifdef UNIX
/* see machmidi.c for UNIX_MACH (OS X) implementation */
#ifndef UNIX_MACH
#ifdef UNIX_IRIX_MIDIFNS
void eventwait(timeout)
  long timeout;
{
    struct timeval unix_timeout;
    struct timeval *waitspec = NULL;
    fd_set readfds;

    FD_ZERO(&readfds);
    FD_SET(mdGetFd(miport), &readfds);
    FD_SET(fileno(stdin), &readfds);
    if (timeout >= 0) {
      
        timeout -= gettime();   /* convert to millisecond delay */
        unix_timeout.tv_sec = timeout / 1000;
        /* remainder become microsecs: */
        unix_timeout.tv_usec = (timeout - (unix_timeout.tv_sec * 1000)) * 1000;
        waitspec = &unix_timeout;
    }
     select(FD_SETSIZE, &readfds, 0, 0, waitspec);

    return;
}
#else
#ifdef BUFFERED_SYNCHRONOUS_INPUT
void eventwait(long timeout)
{
    struct timeval unix_timeout;
    struct timeval *waitspec = NULL;
    struct rlimit file_limit;

    if (timeout >= 0) {
    timeout -= gettime();   /* convert to millisecond delay */
    unix_timeout.tv_sec = timeout / 1000;
    /* remainder become microsecs: */
    unix_timeout.tv_usec = (int)
            (timeout - (unix_timeout.tv_sec * 1000)) * 1000;
    waitspec = &unix_timeout;
    getrlimit(RLIMIT_NOFILE, &file_limit);
    select((int) (file_limit.rlim_max+1), 0, 0, 0, waitspec);
    } else {
    int c = getc(stdin);
    ungetc(c, stdin);
    }
    return;
}
#else
void eventwait(long timeout)
{
    struct timeval unix_timeout;
    struct timeval *waitspec = NULL;
    int readfds = 1 << IOinputfd;
    struct rlimit file_limit;

    if (timeout >= 0) {
    timeout -= gettime();   /* convert to millisecond delay */
    unix_timeout.tv_sec = timeout / 1000;
    /* remainder become microsecs: */
    unix_timeout.tv_usec = (timeout - (unix_timeout.tv_sec * 1000)) * 1000;
    waitspec = &unix_timeout;
    }
    getrlimit(RLIMIT_NOFILE, &file_limit);
    select(file_limit.rlim_max+1, &readfds, 0, 0, waitspec);
    return;
}
#endif /* BUFFERED_SYNCHRONOUS_INPUT */
#endif /* UNIX_IRIX */
#endif /* UNIX_MACH */
#endif /* UNIX */ /* I wanted to put an else here, but this confused a Unix C compiler */
#endif /* UNIX_ITC */
#ifdef AMIGA
/* see camdmidi.c for Amiga implementation */
#else
#ifndef UNIX /* since I couldn't use an else above, have to check UNIX here */
#ifdef WINDOWS
void eventwait(long timeout)
{
    if (timeout >= 0) {
    gprintf(TRANS, "eventwait: not implemented\n");
    return;
    } else {
    int c = getc(stdin);
    ungetc(c, stdin);
    }
    return;
}
#else
void eventwait(timeout)
  long timeout;
{
    while (timeout > gettime() || timeout == -1) {
        if (check_ascii() || check_midi()) return;
    }
}
#endif /* WINDOWS */
#endif /* UNIX */
#endif /* AMIGA */


/****************************************************************************
*                exclusive
* Inputs:
*    boolean onflag -- set to TRUE to receive midi exclusive data
* Effect: 
*    Tells module to read exclusive messages into buffer
****************************************************************************/

void exclusive(boolean onflag)
{
    if  (!initialized) fixup();
    if (musictrace) gprintf(TRANS, "exclusive: %d\n", onflag);
#ifdef AMIGA
    if (onflag) SetMidiFilters(cmt_mi, 
    cmt_mi->PortFilter, cmt_mi->TypeFilter | CMF_SysEx, cmt_mi->ChanFilter);
    else SetMidiFilters(cmt_mi, 
    cmt_mi->PortFilter, cmt_mi->TypeFilter & ~CMF_SysEx, cmt_mi->ChanFilter);
#endif
#ifdef  MACINTOSH_OR_DOS
    exclFilter = !onflag;
#endif
}


/****************************************************************************
*                    fixup
* Effect: 
*    Print error message and call musicinit
****************************************************************************/

private void fixup(void)
{
    gprintf(ERROR, "You forgot to call musicinit.  I'll do it for you.\n");
    musicinit();
}

#ifdef UNIX_IRIX_MIDIFNS
private void flush_sysex(void);
#endif

long get_excl(byte *buffer, long len)
{
    long ret = 0;
#ifdef UNIX_IRIX_MIDIFNS
    byte *sxp = sysex_p;
    long l = len;
#endif
#ifdef UNIX_ITC  /* was ITC */
    ret = mi_getx(midiconn, FALSE, len, (char *) buffer);
#endif
#ifdef UNIX_MACH
    ret = mi_getx(midiconn, FALSE, len, (unsigned char *)buffer);
#endif
#ifdef UNIX_IRIX_MIDIFNS
    if (!sysex_p) return 0;
    if (len > sysex_n) len = sysex_n;
    while (l--)
      {
        *buffer = *(sxp++);
        if (*(buffer++) == CMT_MIDI_EOX)
          {
        flush_sysex();
        break;
          }
      }
    ret = len - l - 1;
#endif
#ifdef AMIGA
    ret = GetSysEx(cmt_mi, (UBYTE *) buffer, len);
    AMIGA_ERROR_CHECK;
#endif
#ifdef MACINTOSH_OR_DOS
#ifndef WINDOWS
    /* I'm not sure the following line is a good thing: it forces the
     * caller to wait until a full sysex message is received and the
     * 1st 4 bytes are fetched via getbuf() before a sysex message can
     * be read via get_excl().  Without this, both mm.c and exget.c
     * were fetching ahead and getting out of sync with getbuf().  I
     * fixed mm.c and exget.c to work (by checking for EOX), but I added
     * this line (which should never have any effect) just to make the
     * DOS interface behave more like the Amiga and Mac interfaces.  The
     * drawback is that you can't fetch bytes until the EOX is seen,
     * because nothing goes into the getbuf() buffer until then.
     */
    if (!sysex_pending) return 0;
    while (len-- && (xbufhead != xbuftail)) {
    *buffer = xbuff[xbufhead++];
    ret++;
    if (*buffer == MIDI_EOX) {
        sysex_pending = FALSE;
        break;
    }
    buffer++;
    xbufhead &= xbufmask;
    }
#endif
#endif
    return ret;
}

/****************************************************************************
*                   getbuf
* Inputs:
*    boolean waitflag: TRUE if routine should wait for data
*    byte * p: Pointer to data destination
* Result: boolean
*    TRUE if data was written to *p
*    FALSE if data not written to *p
* Effect: 
*    copies data from buffer to *p
*    will wait for buffer to become nonempty if waitflag is TRUE
*
* Modified 24 May 1988 for AMIGA (JCD)
****************************************************************************/

#ifdef UNIX_IRIX_MIDIFNS
    private void setup_sysex(MDevent *event, u_char *buffer);
#endif /* UNIX_IRIX */
boolean getbuf(boolean waitflag, unsigned char * p)
{
#ifdef UNIX_IRIX_MIDIFNS
    MDevent event;
    int ret;
#endif /* UNIX_IRIX */

    if (!initialized) fixup();
#ifdef UNIX
#ifdef UNIX_IRIX_MIDIFNS
/* current IRIX version ignores the waitflag (it never waits) */

  if (sysex_p) flush_sysex();
  if (ignore_realtime == 0) {
      ret = mdReceive(miport, &event, 1);
      if (ret) {
      if (event.msg[0] != 0xF0) {
          *((u_long*) p) = *((u_long*) event.msg);
      } else {
          setup_sysex(&event, p);
      }
      }
      return ret;
  } else {
      do /* skip realtime messages */
    {
       ret = mdReceive(miport, &event, 1);
       if (ret == -1) return ret;
     } while (event.msg[0] == 0xf8);
      if (event.msg[0] != 0xF0) {
      *((u_long*) p) = *((u_long*) event.msg);
      } else {
      setup_sysex(&event, p);
      }
      return ret;
  }
#endif /* UNIX_IRIX */
#ifdef UNIX_ITC
    if (ignore_realtime == 0) {
        return(mi_get(midiconn, waitflag, (char *) p));
    }
    else {
        boolean ret=false;
        /* filter out realtime msgs */
        do {
            ret = mi_get(midiconn, waitflag, (char *) p);
            if (ret == FALSE)
                return(ret);
        } while(p[0] == 0xf8);
        return(ret);
    }
#else /* UNIX_ITC */
#ifndef UNIX_IRIX
    if (waitflag) {
    gprintf(ERROR, "getbuf called with waitflag!");
    EXIT(1);
    }
    return FALSE;
#endif /* UNIX_IRIX */
#endif /* UNIX_ITC */
#endif /* UNIX */

#ifdef MACINTOSH_OR_DOS
#ifndef WINDOWS
    if (sysex_pending) { /* flush sysex to keep buffers in sync */
        while (xbuff[xbufhead++] != MIDI_EOX) {
        xbufhead &= xbufmask;
        if (xbufhead == xbuftail) break;
        }
        sysex_pending = FALSE;
    }
    if (waitflag) while (buffhead == bufftail) /* wait */ ;
    else if (buffhead == bufftail) return(false);
    *(long *)p = *(long *)(((char *)buff)+buffhead);
    buffhead = (buffhead + 4) & BUFF_MASK;
    if (*p == MIDI_SYSEX) { /* if sys-ex, remember to fetch from xbuff */
        sysex_pending = TRUE;
    }
    return(true);
#else
    return FALSE;
#endif /* WINDOWS */
#endif /* MACINTOSH_OR_DOS */

#ifdef AMIGA
    if (waitflag) {
        do {
        WaitMidi(cmt_mi, &cmt_msg);
        AMIGA_ERROR_CHECK;
        } while (amigaerrflags);
    } else {
        AMIGA_ERROR_CHECK;
        if (!GetMidi(cmt_mi, &cmt_msg)) return(false);
    }    
    *(long *)p = *(long *)&cmt_msg;
    clearmsg(cmt_msg);
    return(true);
#endif /* AMIGA */
}

#ifdef UNIX_IRIX_MIDIFNS

private void setup_sysex(MDevent *event, u_char *buffer)
/* N.B. do not leak memory remember to call free(sysex_p) */
{
   u_char *sxp = (u_char *) event->sysexmsg;
   int i;

   for (i=0;i<4;i++)
     *(buffer++) = *(sxp++);
   sysex_p = event->sysexmsg;
   sysex_n = event->msglen;
}

private void flush_sysex(void)
{
  mdFree(sysex_p);
  sysex_p = 0;
  sysex_n = 0;
}
#endif

#ifdef MACINTOSH_OR_DOS
#ifndef WINDOWS
public boolean check_midi(void)
{
    if (buffhead == bufftail) return FALSE;
    else return TRUE;
}
#endif
#endif


/****************************************************************************
*                   getkey
* Inputs:
*    boolean waitflag: TRUE if wait until key depression, FALSE if
*             return immediately
* Result: int
*    key number of key which has been depressed
*    It returns -1 if waitflag is FALSE and no key has been pressed
*    If waitflag is TRUE this routine will block until a key is pressed
* Effect: 
*    reads a key
****************************************************************************/

/*DMH: in previous version, macmidi.c subtracted 12 from msg to get key at each occurence...*/

short getkey(boolean waitflag)
{
    byte msg[4];
    short k;

    if (!initialized) fixup();

    while (TRUE) {    /* process data until you find a note */
    /* look for data and exit if none found */
    /* NOTE: waitflag will force waiting until data arrives */
    if (!getbuf(waitflag, msg)) { /* nothing there */
        k = -1;
        break;
    } else if ((msg[0] & MIDI_CODE_MASK) == MIDI_ON_NOTE) {
        if (msg[2] == 0) { /* velocity 0 -> note off */
        keyloud = 0;
        k = msg[1] + 128;
        } else {
        keyloud = msg[2];
        k = msg[1];
        }
        break;
    } else if ((msg[0] & MIDI_CODE_MASK) == MIDI_OFF_NOTE) {
        keyloud = 0;
        k = msg[1] + 128;
        break;
    }
    }
    if (musictrace) {
    if (k != -1) gprintf(TRANS,"getkey got %d\n", k);
    }
    return k;
}


/****************************************************************************
*                   gettime
* Result: ulong
*    current timestamp since the last call to
*    musicinit or timereset
* Effect: 
*    fakes it
****************************************************************************/

ulong gettime(void)         /*DMH: ulong is from mpu->midifns conversion, for Mac*/
{
#if HAS_GETTIMEOFDAY
    struct timeval timeval;
#endif
#if HAS_FTIME
    struct timeb ftime_res;
#endif
    register ulong ticks = 0L;

    BREAKTEST    /* abort if user typed Ctrl Break */
    if (!initialized) fixup();

#ifdef MACINTOSH
#ifdef MIDIMGR
    ticks = MIDIGetCurTime(OutputRefNum) - ticksAtStart;
#else
    ticks = TickCount() - ticksAtStart;
#endif
    if (initialized) abort_check();     /* give user a chance to abort */
    ticks = TICKS_TO_MS(ticks);
#endif

#ifdef AMIGA
    ticks = (*camdtime - timeoffset) << 1;      /* return milliseconds */
#endif

#ifdef  DOS
#ifndef WINDOWS       
    ticks = elapsedtime(timeoffset, readtimer()); /* return milliseconds */
    /* gprintf(TRANS, "currtime = %ld, timeoffset = %ld\n", currtime, timeoffset); */
#endif
#endif  /* ifdef DOS */

#if HAS_GETTIMEOFDAY
    gettimeofday(&timeval, 0);
    ticks = timeval.tv_sec * 1000 + timeval.tv_usec / 1000 - timeoffset;
#endif
#if HAS_FTIME
    ftime(&ftime_res);
    ticks = ((ftime_res.time - timeoffset) * 1000) + ftime_res.millitm;
#endif

    /* if (miditrace) gprintf(TRANS, "."); */
    return(ticks);
}


/****************************************************************************
*                   l_rest
* Inputs:
*    long time: Amount of time to rest
* Effect: 
*    Waits until the amount of time specified has lapsed
****************************************************************************/

void l_rest(long time)
{
    if (!initialized) fixup();
    l_restuntil(time + gettime());    
}

/****************************************************************************
*                 l_restuntil
* Inputs:
*    long time: Event time to rest until
* Effect: 
*    Waits until the specified time has been reached (absolute time)
****************************************************************************/

void l_restuntil(long time)
{
#ifdef MACINTOSH
    ulong now = gettime();  
    ulong junk; /* changed from ulong for ThinkC 7, back to ulong for CW5 */
#endif

#ifdef AMIGA
    while (time > gettime()) eventwait(time);
#else  
    for(; (time_type) time > gettime(););
#endif

#ifdef MACINTOSH
    now = gettime();        

    if (time > now)  Delay(MS_TO_TICKS(time - now), &junk);
    /* else time <= now, so return immediately */
#endif

}

/****************************************************************************
*               metronome
* Inputs:
*    boolean onflag: TRUE or FALSE
* Effect:
*    enables (true) or disables (false) MPU-401 metronome function.
*    must be called before musicinit
****************************************************************************/

void metronome(boolean onflag)
{
#ifdef DOS
metroflag = onflag;
#endif
}


/****************************************************************************
*                  midi_bend
* Inputs:
*    int channel: midi channel on which to send data
*    int value: pitch bend value
* Effect: 
*    Sends a midi pitch bend message
****************************************************************************/

void midi_bend(int channel, int value)
{
    if (!initialized) fixup();
    if (musictrace)
    gprintf(TRANS,"midi_bend: ch %d, val %d\n", channel, value - (1 << 13));
        bend[MIDI_CHANNEL(channel)] = value;
    
    midi_write(3, MIDI_PORT(channel), (byte) (MIDI_BEND | MIDI_CHANNEL(channel)),
        (byte) MIDI_DATA(value), (byte) MIDI_DATA(value >> 7));
}


/****************************************************************************
*               midi_buffer
* Inputs:
*    byte * buffer: the buffer address
*    int size: number of bytes in buffer
* Returns:
*    FALSE if size is less than 16 or buffer is NULL, otherwise TRUE
* Effect: DOS, MAC:
*    tells interrupt routine to store system exclusive messages in
*    buffer.     The largest power of 2 bytes less than size will be
*    used.  xbufhead and xbuftail will be initialized to zero,
*    and xbuftail will be one greater than the index of the last
*    system exclusive byte read.  Since there may already be a buffer
*    and therefore the normal midi message buffer may have the first
*    4 bytes of some sysex messages, clear the normal midi buffer too.
* AMIGA:
*    adds buffer to midi interface
*
****************************************************************************/

boolean midi_buffer(byte huge *buffer, ulong size)
{
    if (!buffer) return FALSE;
#ifdef AMIGA
    if (!SetSysExQueue(cmt_mi, (UBYTE *) buffer, (ULONG) size)) return(false);
    cu_register(remove_sysex_buffer, buffer);
#endif

#ifdef MACINTOSH_OR_DOS
#ifndef WINDOWS
    {
    int mask = 0x000F;

    if (size < 16) return(false);
    while (mask < size && mask > 0) mask = ((mask << 1) | 1);
    midi_flush();
    xbuff = NULL;    /* turn off buffering */
    xbufmask = mask >> 1;
    xbufhead = xbuftail = 0;
    xbuff = buffer;    /* set buffer, turn on buffering */
    }
#endif
#endif
#ifdef UNIX
    return FALSE;
#else
    exclusive(TRUE);
    return TRUE;
#endif
}


/* midi_clock -- send a midi time clock message */
/**/
void midi_clock()
{
    if (!initialized) fixup();
    if (musictrace) gprintf(TRANS, "+");
    midi_write(1, 0, MIDI_TIME_CLOCK, 0, 0);
}


/****************************************************************************
*               midi_cont
* Inputs:
*    boolean onflag: TRUE or FALSE
* Effect:
*    enables (true) or disables (false) continuous control
****************************************************************************/

void midi_cont(boolean onflag)
{
    if (!initialized) fixup();
    if (onflag) {
#ifdef AMIGA
    SetMidiFilters(cmt_mi, cmt_mi->PortFilter, 
        cmt_mi->TypeFilter | CONTCONT, cmt_mi->ChanFilter);
#endif
#ifdef  DOS
#ifndef WINDOWS
    mPutCmd(BENDERON);
#endif
#endif
    } else {
#ifdef AMIGA
    SetMidiFilters(cmt_mi, cmt_mi->PortFilter,
        cmt_mi->TypeFilter & ~CONTCONT, cmt_mi->ChanFilter);
#endif
    }
#ifdef MACINTOSH_OR_DOS
    ctrlFilter = !onflag;
#endif
    if (musictrace) gprintf(TRANS,"midi_cont: %d\n", onflag);
}


/****************************************************************************
*                  midi_ctrl
* Inputs:
*    int channel: midi channel on which to send data
*    int control: control number
*    int value: control value
* Effect: 
*    Sends a midi control change message
****************************************************************************/

void midi_ctrl(int channel, int control, int value)
{
    if (!initialized) fixup();
    if (musictrace)
    gprintf(TRANS,"midi_ctrl: ch %d, ctrl %d, val %d\n",
        channel, control, value);

    midi_write(3, MIDI_PORT(channel), (byte) (MIDI_CTRL | MIDI_CHANNEL(channel)), 
          (byte) MIDI_DATA(control), (byte) MIDI_DATA(value));
}


/****************************************************************************
*                midi_exclusive
* Inputs:
*    byte *msg: pointer to a midi exclusive message, terminated by 0xF7
* Effect: 
*    Sends a midi exclusive message
* Bugs:
*   18-mar-94 PLu : This function does not know which port to send to in
*                    case of multiple midi-ports (MAC, IRIX)
****************************************************************************/

#ifdef MACINTOSH
#define INTERBYTE_DELAY 10
#endif

void midi_exclusive(msg)
unsigned char *msg; /* the data to be sent */
{
#ifdef ITC
    int count, done, tosend, willsend;
    unsigned char *m;
    mi_status ret;
#endif
#ifdef UNIX_IRIX_MIDIFNS
    unsigned char *m;
    MDevent mdevent;
#endif

#ifdef MACINTOSH
#ifndef NYQUIST
    int i;                      /* for DX7 delay loop */
    int count = 0;      /* counter for formatting midi byte trace */
    MIDIPacket TheMIDIPacket;
    unsigned char prev = 0;
    boolean first_packet = TRUE;
#endif
#endif

    /*
     *  if user mistakenly called midi_exclusive instead of exclusive,
     *  the argument will be TRUE or FALSE, both of which are highly    
     *  unlikely valid arguments for midi_exclusive:
     */

    if (msg == (byte *) FALSE || msg == (byte *) TRUE) {
    gprintf(ERROR,"midi_exclusive: invalid argument %u.\n", msg);
    EXIT(1);
    }
    if (!initialized) fixup();
    if (musictrace) gprintf(TRANS,"midi_exclusive\n");

#ifdef AMIGA
    PutSysEx(cmt_mi, msg);
#endif
#ifdef MACINTOSH
#ifndef NYQUIST /* if NYQUIST, do nothing */
#ifdef MIDIMGR
    while (prev != MIDI_EOX) {
    int len = 0;
    while (prev != MIDI_EOX && len < 249) {
        TheMIDIPacket.data[len++] = prev = *msg++;
    }
    TheMIDIPacket.len = 6 + len;
    TheMIDIPacket.tStamp = 0;
    if (first_packet && (prev != MIDI_EOX)) {
        TheMIDIPacket.flags = midiTimeStampCurrent + midiStartCont;
        first_packet = FALSE;
    } else if (first_packet) {
        TheMIDIPacket.flags = midiTimeStampCurrent + midiNoCont;
    } else if (prev == MIDI_EOX) {
        TheMIDIPacket.flags = midiTimeStampCurrent + midiEndCont;
    } else {
        TheMIDIPacket.flags = midiTimeStampCurrent + midiMidCont;
    }               
    MIDIWritePacket(OutputRefNum, &TheMIDIPacket);
    }
#else
    while (*msg != MIDI_EOX) {
    Xmit(0, *msg);
    msg++;
    count++;
    /* this is a delay loop, without which your DX7 will crash */
    for (i = INTERBYTE_DELAY; i > 0; i--)
        abort_check();
    }
    Xmit(0, MIDI_EOX);
#endif /* MIDIMGR */
#endif /* NYQUIST */
#endif /* MACINTOSH */

#ifdef DOS
#ifndef WINDOWS
    do {
    mPutData(*msg);
    } while (*msg++ != MIDI_EOX);
#endif
#endif
#ifdef ITC
    for (m = msg, tosend = 1; (*m) != MIDI_EOX; m++, tosend++);
    for (count = 0; count < tosend; count += done) {
    willsend = min(16384, tosend);
    ret = mi_exclusive(midiconn, 1, msg, (short) willsend);
    if (ret != MI_SUCCESS) {
        gprintf(GWARN, "Got %d from mi_exclusive\n", ret);
    }
    done = willsend;
    }
#endif
#ifdef UNIX_IRIX_MIDIFNS
/* we don't know which device to sent SYSEX messages to so port zero is
   assumed. */
    for (m = msg, mdevent.msglen = 1; (*m) != CMT_MIDI_EOX; m++, mdevent.msglen++);
    mdevent.sysexmsg = msg;
    if (mdSend(miport, &mdevent, 1) == -1) {
      gprintf(GWARN, "could not send SYSEX message\n");
    }
#endif

    if (miditrace) {
    do { gprintf(TRANS, "~%2x", *msg);
#ifdef UNIX_IRIX_MIDIFNS
        } while (*msg++ != CMT_MIDI_EOX);
#else
        } while (*msg++ != MIDI_EOX);
#endif
    }
}


/****************************************************************************
*                  midi_note
* Inputs:
*    int channel: midi channel on which to send data
*    int pitch: midi pitch code
*    int velocity: velocity with which to sound it (0=> release)
* Effect: 
*    Sends a midi note-play request out
****************************************************************************/

void midi_note(int channel, int pitch, int velocity)
{
    if (!initialized) fixup();
    if (musictrace) gprintf(TRANS,"midi_note: ch %d, key %d, vel %d\n",
    channel, pitch, velocity);
    if (user_scale) {
    /* check for correct pitch bend */
    if ((pit_tab[pitch].pbend != bend[MIDI_CHANNEL(channel)]) 
        && (velocity != 0)) {
        midi_bend(channel, pit_tab[pitch].pbend);
        bend[channel] = pit_tab[pitch].pbend;
    }
    pitch = pit_tab[pitch].ppitch;  
    }
    midi_write(3, MIDI_PORT(channel), (byte) (MIDI_ON_NOTE | MIDI_CHANNEL(channel)),
          (byte) MIDI_DATA(pitch), (byte) MIDI_DATA(velocity));
}


/****************************************************************************
*                midi_program
* Inputs:
*    int channel: Channel on which to send midi program change request
*    int program: Program number to send (decremented by 1 before
*           being sent as midi data)
* Effect: 
*    Sends a program change request out the channel
****************************************************************************/

void midi_program(int channel, int program)
{
#ifdef MACINTOSH
    int port, midi_chan;
#endif

    if (!initialized) fixup();
    if (musictrace) gprintf(TRANS,"midi_program: ch %d, prog %d\n",
                channel, program);
    channel = MIDI_CHANNEL(channel);    
    if (cur_midi_prgm[channel] != program) {  
    midi_write(2, MIDI_PORT(channel), (byte) (MIDI_CH_PROGRAM | channel),
            (byte) (MIDI_PROGRAM(program)), 0);
    cur_midi_prgm[channel] = program;
    }
}


/****************************************************************************
*               midi_real
* Inputs:
*    boolean onflag: TRUE or FALSE
* Effect:
*    enables (true) or disables (false) midi realtime messages F8-FF
****************************************************************************/

void midi_real(boolean onflag)  
{
    if (!initialized) fixup();
#ifdef UNIX_ITC
    {
        mi_status ret;

        ret = mi_realtime(midiconn, onflag);
        if (ret != MI_SUCCESS) {
            gprintf(ERROR, "Warning: bad ret = %d in midi_real\n", ret);
        }
    }
#endif /* UNIX_ITC */
#ifdef ITC
    ignore_realtime = !onflag;
#endif /* ITC */
#ifdef AMIGA
    if (onflag) {
        SetMidiFilters(cmt_mi, cmt_mi->PortFilter,
        cmt_mi->TypeFilter | CMF_RealTime, cmt_mi->ChanFilter);
    } else {
        SetMidiFilters(cmt_mi, cmt_mi->PortFilter,
        cmt_mi->TypeFilter & ~CMF_RealTime, cmt_mi->ChanFilter);
    }
#endif
#ifdef MACINTOSH_OR_DOS
    realFilter = !onflag;
#endif

    if (musictrace) gprintf(TRANS,"midi_real: %d\n", onflag);
}


/* midi_start -- send a midi start message */
/**/
void midi_start()
{
    if (!initialized) fixup();
    if (musictrace) gprintf(TRANS, "`");
    midi_write(1, 0, MIDI_START, 0, 0);
}


/* midi_stop -- send a midi stop message */
/**/
void midi_stop()
{
    if (!initialized) fixup();
    if (musictrace) gprintf(TRANS, "'");
    midi_write(1, 0 /* ignored */, MIDI_STOP, 0, 0);
}


/****************************************************************************
*               midi_thru
* Inputs:
*    boolean onflag: TRUE or FALSE
* Effect:
* DOS:      enables (true) or disables (false) midi thru info from
*      MPU-401 to host.  (Default is set; reset with cmdline -block.)
* AMIGA:  enables (true) or disables (false) midi route from AMIGA
*        midi input to AMIGA midi output.
****************************************************************************/

void midi_thru(boolean onflag)  /* DMH: midi thru is not supported on the MAC or DOS */
{
    if (!initialized) fixup();
#ifndef MIDI_THRU
    gprintf(ERROR, "midi_thru called but not implemented\n");
#else
#ifdef AMIGA
    MidiThru(0L, (long) onflag);
#endif
#ifdef MACINTOSH
    /* this currently does not do anything - Mac driver doesn't
     * support THRU
     */
    do_midi_thru = onflag;
#endif
#endif

    if (musictrace) gprintf(TRANS,"midi_thru: %d\n", onflag);
}


/****************************************************************************
*                  midi_touch
* Inputs:
*    int channel: midi channel on which to send data
*    int value: control value
* Effect: 
*    Sends a midi after touch message
****************************************************************************/

void midi_touch(int channel, int value)
{
    if (!initialized) fixup();
    if (musictrace) gprintf(TRANS,"midi_touch: ch %d, val %d\n",channel,value);
    midi_write(2, MIDI_PORT(channel), (byte) (MIDI_TOUCH | MIDI_CHANNEL(channel)),
        (byte) MIDI_DATA(value), 0);
}


/****************************************************************************
*                  midi_write
* Inputs:
*       UBYTE n: number of characters to send (1, 2 or 3);
    int port: the port number (usually 0), on MAC, this may be 1
*    char c1,c2,c3: Character(s) to write to MIDI data port
* Effect: 
*    Writes the data to the serial interface designated by port
****************************************************************************
* Change log
*  Date     | Change
*-----------+----------------------------------------------------------------
* 15-Mar-94 | PLu : Added IRIX version
****************************************************************************/
  
#ifdef UNIX
#ifdef UNIX_IRIX_MIDIFNS
void midi_write(int n, int port, unsigned char c1, unsigned char c2, unsigned char c3)
{
  MDevent event;
  
  if (port < 0) return;

  * ((u_long *) event.msg) = 0xe0000000 | ((port & 0x1f) << 24) | (c1 << 16) | 
           (c2 << 8) | c3;
  if (mdSend(miport, &event, 1) == -1)
    gprintf(ERROR, "Can not send midi message in midi_write");

  midi_write_trace(n, port, c1, c2, c3);
}
#else

#ifdef ITC
void midi_write(int n, int port,
        unsigned char c1, unsigned char c2, unsigned char c3)
{
    unsigned char outb[3];
    mi_channel mch;
    mi_status ret;

    if (port < 0) return;
    outb[0] = c1;
    outb[1] = c2;
    outb[2] = c3;
    mch = (16*port)+((int)MI_CHANNEL(c1));
    ret = mi_put(midiconn, mch, outb);
    if (ret != MI_SUCCESS)
    gprintf(ERROR, "Warning: bad ret = %d in midi_write\n", (int)ret);
    midi_write_trace(n, port, c1, c2, c3);
}
#else
void midi_write(int n, int port, 
        unsigned char c1, unsigned char c2, unsigned char c3)
{
    /* no output */
    midi_write_trace(n, port, c1, c2, c3);
}
#endif /* ITC */
#endif /*  UNIX_IRIX */
#endif /* UNIX */

#ifdef DOS
#ifndef WINDOWS
void midi_write(int n, int port,
        unsigned char c1, unsigned char c2, unsigned char c3)
{
    if (n >= 1) mPutData(c1);
    if (n >= 2) mPutData(c2);
    if (n >= 3) mPutData(c3);
    midi_write_trace(n, port, c1, c2, c3);
}
#else
void midi_write(int n, int port, 
        unsigned char c1, unsigned char c2, unsigned char c3)
{
    midi_write_trace(n, port, c1, c2, c3);
}
#endif
#endif

#ifdef MACINTOSH
#ifdef MIDIMGR
void midi_write(int n, int port,
        unsigned char c1, unsigned char c2, unsigned char c3)
{
    MIDIPacket TheMIDIPacket;
    
    TheMIDIPacket.flags = midiTimeStampCurrent;
    TheMIDIPacket.len = 6 + n;
    TheMIDIPacket.tStamp = 0;
    TheMIDIPacket.data[0] = c1;
    TheMIDIPacket.data[1] = c2;
    TheMIDIPacket.data[2] = c3;
    MIDIWritePacket(OutputRefNum, &TheMIDIPacket);
    midi_write_trace(n, port, c1, c2, c3);
}
#else
void midi_write(int n, int port, unsigned char c1, unsigned char c2, unsigned char c3)
{
#ifndef NYQUIST
    Xmit(port, c1);
    if (n >= 2) Xmit(port, c2);
    if (n >= 3) Xmit(port, c3);
#endif
    midi_write_trace(n, port, c1, c2, c3);
}
#endif
#endif

void midi_write_trace(int n, int port,
              unsigned char c1, unsigned char c2, unsigned char c3)
{
    if (miditrace) {
    /* to indicate bytes going out on port 1, put message in brackets
     * with the port number, e.g. [1:~90~3c~64]
     */
    if (port > 0) gprintf(TRANS, "[%d:", port);
    if (n >= 1) gprintf(TRANS, "~%2x", c1);
    if (n >= 2) gprintf(TRANS, "~%2x", c2);
    if (n >= 3) gprintf(TRANS, "~%2x", c3);
    if (port > 0) gprintf(TRANS, "]", port);
    }
}



/*****************************************************************
*           set_pitch_default
*****************************************************************/

private void set_pitch_default()
{
    int i;

    for (i = 0; i < 128; i++) {
    pit_tab[i].pbend = 8192;
    pit_tab[i].ppitch = i;
    }
}

/*****************************************************************
*           read_tuning
*****************************************************************/

void read_tuning(filename)
char *filename;
{
    int index, pit, lineno = 0;
    float bend;
    FILE *fpp;

    user_scale = TRUE;
    set_pitch_default();

    fpp = fileopen(filename, "tun", "r", "Tuning definition file");
    while ((fscanf(fpp, "%d %d %f\n", &index, &pit, &bend) > 2) &&
    (lineno < 128)) {
    lineno++;
    if (index >= 0 && index <= 127) {
        pit_tab[index].pbend = (int)(8192 * bend/100 + 8192);
        pit_tab[index].ppitch = pit;
    }
    }
}


/****************************************************************************
*                  musicinit
* Effect: 
****************************************************************************/

void musicinit()
{
    int i;
    char *filename;

    if (!tune_flag) {    /* do this code only once */
    miditrace = cl_switch("miditrace");
    musictrace = cl_switch("trace");
    }
    
    if (!initialized) {
    cu_register((cu_fn_type) musicterm, NULL);
    midi_init();
    }
    initialized = TRUE;
    /* this does some random cleanup activity */

#ifndef APPLICATION
    if (!tune_flag) {    /* do this code only once */
#ifdef DOS
#ifndef WINDOWS
#if 0
    version = mPutGetCmd(GETMPUVER);
    revision = mPutGetCmd(GETMPUREV);
    gprintf(TRANS, "MPU version %d.%d%c\n", version >> 4, version & 0x0f,
        revision + 'A' - 1);
#endif
    mPutCmd(UARTMODE);
    mPutCmd(NOREALTIME);    /* initially prevent Real Time MIDI info */
    mPutCmd(EXCLUSIVOFF);   /* initially prevent Sys-Ex data */
#endif
#endif
    tune_flag = TRUE;
    filename = cl_option("tune");
    if (filename != NULL) read_tuning(filename);
    }
    /* now that flags are set, print the trace message */
    if (musictrace) gprintf(TRANS, "musicinit()\n");

    if (user_scale) {
    for (i = 0; i < MAX_CHANNELS; i++) {
        midi_bend(i, 8192);
        bend[i] = 8192;
    }
    }
#endif /* ifndef APPLICATION */

    for (i = 0; i < MAX_CHANNELS; i++) {
    /* initialize to impossible values so that the
     * next call to midi_bend or midi_program will
     * not match and therefore send an output:
     */
    bend[i] = -1;
    cur_midi_prgm[i] = -1;
    }
#ifdef MIDI_THRU
    midi_thru(!(cl_switch("block")));    /* set MIDI thru */
#endif
    timereset();            /* Reset clock */
#ifdef AMIGA
    event_mask |= (1L << ascii_signal()) | (1L << cmt_mi->AlarmSigBit) |
          (1L << cmt_mi->RecvSigBit);
#endif
}


/****************************************************************************
*                  musicterm
* Effect: 
*     Miscellaneous cleanup of things done by musicinit.
****************************************************************************/

private void musicterm(void)
{
    if (musictrace) gprintf(TRANS, "musicterm()\n");
    initialized = FALSE;
}


/****************************************************************************
*                   cmtrand
* Inputs:
*    int lo: Lower limit of value
*    int hi: Upper limit of value
* Result: int
*    random number (lo <= result <= hi)
****************************************************************************/

/* to avoid confusion and dead code, take this out */
#ifdef ALL_CMT
long randseed = 1534781L;

short cmtrand(short lo, short hi)
{
    randseed *= 13L;
    randseed += 1874351L;
    return((short)(lo + (((hi + 1 - lo) * ((0x00ffff00 & randseed) >> 8)) >> 16)));
}


#ifdef AMIGA
/* remove_sysex_buffer -- a cleanup procedure for the Amiga */
/**/
void remove_sysex_buffer(void *obj)
{
    ClearSysExQueue(cmt_mi);
}
#endif /* AMIGA */


/****************************************************************************
*                  settime
* Inputs: new time
* Effect: 
*    Sets the current time to the new time.
*        DMH: for MAC, sets the clock to absTime
*                 implemented by adjusting ticksATStart
****************************************************************************/

void settime(newtime)
  time_type newtime;
{
    if (musictrace) gprintf(TRANS, "settime(%lu)\n", newtime);
#ifdef AMIGA
    timeoffset = *camdtime - (newtime >> 1);
#endif

#ifdef MACINTOSH
#ifdef MIDIMGR
    ticksAtStart = MIDIGetCurTime(OutputRefNum);
#else
    ticksAtStart = TickCount() - MS_TO_TICKS(newtime);
#endif
#endif  
}
#endif //ALL_CMT


/****************************************************************************
*                  timereset
* Effect: 
*    Resets the time.
*       DMH: for MAC, implemented by setting ticksAtStart to
*            current value of system tick counter
*       JMN: for DOS, resets the time on the MPU-401. Ticks is reset to 0
****************************************************************************/

void timereset()
{
#if HAS_GETTIMEOFDAY
    struct timeval timeval;
#endif
#if HAS_FTIME
    struct timeb ftime_res;
#endif

    if (!initialized) fixup();
    if (musictrace) gprintf(TRANS,"timereset()\n");

#ifdef AMIGA
    timeoffset = *camdtime;
#endif

#ifdef DOS
#ifndef WINDOWS
    timeoffset = (ulong) readtimer();
#endif
#endif

#ifdef MACINTOSH
#ifdef MIDIMGR
    ticksAtStart = MIDIGetCurTime(OutputRefNum);
#else
    ticksAtStart = TickCount();
#endif
#endif
    
#if HAS_GETTIMEOFDAY
    gettimeofday(&timeval, 0);
    timeoffset = timeval.tv_sec * 1000 + timeval.tv_usec / 1000 - timeoffset;
#endif
#if HAS_FTIME
    ftime(&ftime_res);
    timeoffset = ftime_res.time;
#endif
}


/****************************************************************************
*                  trace
* Inputs:
*    boolean flag: TRUE for trace on
* Effect: 
*    turns tracing on (flag == TRUE) or off (flag == FALSE)
****************************************************************************/

void trace(boolean flag)
{
    musictrace = flag;
}

/****************************************************************************
*                  tracemidi
* Inputs:
*    boolean flag: TRUE for trace on
* Effect: 
*    turns midi tracing on (flag == TRUE) or off (flag == FALSE)
****************************************************************************/

void tracemidi(boolean flag)
{
    miditrace = flag;
}



/***********************************************************************
*
* midi and timer initialization
*
***********************************************************************/

#ifdef  DOS
#include <ctype.h>
/* binary value of hex char */

private int xval(int c)
{
    int i;
    static char t[]="0123456789abcdef";

    for (i=0; i<16; i++)
        if (tolower(c)==t[i]) return(i);
    return (-1);
}

/* binary value of hex string */

private int atox(char *t)
{
    int             i=0;
    int             x;
    while(*t)
    {
        if ((x=xval(*t++))<0)return (0);
        i=(i<<4)+x;
    }
    return (i);
}
#endif  /* def DOS */


private void midi_init(void)
{
#ifdef UNIX_IRIX_MIDIFNS
#define PBUFLEN 4
  MIconfig *config;
  static u_int pbuf[] = { MI_STAMPING, MINOSTAMP, MI_BLOCKING, MINONBLOCKING};
#endif

#ifdef UNIX_MACH
    mach_midi_init();
#else
#ifdef ITC
    midiconn = mi_open(NULL);
    if (midiconn == NULL) {
    gprintf(FATAL, "could not open a MIDI device\n");
    EXIT(1);
    }
    cu_register((cu_fn_type) mi_close, (void *) midiconn);
#endif
#endif
#ifdef AMIGA
    amiga_midi_init();
#endif /* def AMIGA */
#ifdef DOS
#ifndef WINDOWS
    int err;
    int irq=SEARCHIRQ;
    int base=MPUBASEADDR;
    char *t;

    if (t=getenv("MPUIRQ")) {
    if (musictrace)
        gprintf(TRANS,"MPUIRQ %s\n",t);
    irq=atoi(t);
    }
    if (t=getenv("MPUBASE")) {
    if (musictrace)
        gprintf(TRANS,"MPUBASE %s\n",t);
    base=atox(t);
    }
    if (err = mOpen(base, irq)) {
    mClose(err);
    EXIT(1);
    }
    cu_register((cu_fn_type) mClose, 0);
    cu_register((cu_fn_type) mPutCmd, (cu_parm_type) MPURESET);
    initializetimer();
    cu_register((cu_fn_type) restoretimer, NULL);
#endif
#endif

#ifdef MACINTOSH
#ifndef NYQUIST /* if NYQUIST, do nothing */
#ifdef MIDIMGR
    setup_midimgr(); /* this registers itself for cleanup */
#else
    init_abort_handler();
    cu_register(cleanup_abort_handler, NULL);
    setupMIDI(portA, 0x80);
    cu_register(restoreMIDI, (long) portA);
    /* only initialize portB if necessary */
    if (MAX_CHANNELS > CHANNELS_PER_PORT) {
    setupMIDI(portB, 0x80);
    cu_register(restoreMIDI, (long) portB);
    }
#endif
#endif /* NYQUIST */
#ifdef MIDIMGR
    ticksAtStart = MIDIGetCurTime(OutputRefNum);
#else
    ticksAtStart = TickCount(); /* reset the clock */
#endif
#endif /* def MACINTOSH */

    if (!(cl_switch("noalloff")))
    cu_register((cu_fn_type) alloff, NULL);
}

#ifdef  DOS
/****************************************************************************
*                                  set_x_mfr
* Inputs:
*       unsigned char mfr: Manufacturer ID for MIDI
* Result: void
*       
* Effect: 
*       Sets the xcode and xcodemask to allow only these sysex messages
****************************************************************************/

void set_x_mfr(mfr)
unsigned char mfr;
{
    xcode = mfr;
    xcodemask = 0xFF;
}

/****************************************************************************
*                                 clear_x_mfr
* Result: void
*       
* Effect: 
*       Clears sysex manufacturer code filter; accepts all sysex messages
****************************************************************************/

void clear_x_mfr()
{
    xcode = 0;
    xcodemask = 0;
}
#endif /* DOS */
