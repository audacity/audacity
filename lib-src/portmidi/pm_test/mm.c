/* mm.c -- midi monitor */

/*****************************************************************************
*       Change Log
*  Date | Change
*-----------+-----------------------------------------------------------------
*  7-Apr-86 | Created changelog
* 31-Jan-90 | GWL : use new cmdline
*  5-Apr-91 | JDW : Further changes
* 16-Feb-92 | GWL : eliminate label mmexit:; add error recovery
* 18-May-92 | GWL : continuous clocks, etc.
* 17-Jan-94 | GWL : option to display notes
* 20-Nov-06 | RBD : port mm.c from CMU Midi Toolkit to PortMidi
*           |       mm.c -- revealing MIDI secrets for over 20 years!
*****************************************************************************/

#include "stdlib.h"
#include "ctype.h"
#include "string.h"
#include "stdio.h"
#include "porttime.h"
#include "portmidi.h"

#define STRING_MAX 80

#define MIDI_CODE_MASK  0xf0
#define MIDI_CHN_MASK   0x0f
/*#define MIDI_REALTIME   0xf8
  #define MIDI_CHAN_MODE  0xfa */
#define MIDI_OFF_NOTE   0x80
#define MIDI_ON_NOTE    0x90
#define MIDI_POLY_TOUCH 0xa0
#define MIDI_CTRL       0xb0
#define MIDI_CH_PROGRAM 0xc0
#define MIDI_TOUCH      0xd0
#define MIDI_BEND       0xe0

#define MIDI_SYSEX      0xf0
#define MIDI_Q_FRAME	0xf1
#define MIDI_SONG_POINTER 0xf2
#define MIDI_SONG_SELECT 0xf3
#define MIDI_TUNE_REQ	0xf6
#define MIDI_EOX        0xf7
#define MIDI_TIME_CLOCK 0xf8
#define MIDI_START      0xfa
#define MIDI_CONTINUE	0xfb
#define MIDI_STOP       0xfc
#define MIDI_ACTIVE_SENSING 0xfe
#define MIDI_SYS_RESET  0xff

#define MIDI_ALL_SOUND_OFF 0x78
#define MIDI_RESET_CONTROLLERS 0x79
#define MIDI_LOCAL	0x7a
#define MIDI_ALL_OFF	0x7b
#define MIDI_OMNI_OFF	0x7c
#define MIDI_OMNI_ON	0x7d
#define MIDI_MONO_ON	0x7e
#define MIDI_POLY_ON	0x7f


#define private static

#ifndef false
#define false 0
#define true 1
#endif

typedef int boolean;

int debug = false;	/* never set, but referenced by userio.c */
PmStream *midi_in;      /* midi input */
boolean active = false;     /* set when midi_in is ready for reading */
boolean in_sysex = false;   /* we are reading a sysex message */
boolean inited = false;     /* suppress printing during command line parsing */
boolean done = false;       /* when true, exit */
boolean notes = true;       /* show notes? */
boolean controls = true;    /* show continuous controllers */
boolean bender = true;      /* record pitch bend etc.? */
boolean excldata = true;    /* record system exclusive data? */
boolean verbose = true;     /* show text representation? */
boolean realdata = true;    /* record real time messages? */
boolean clksencnt = true;   /* clock and active sense count on */
boolean chmode = true;      /* show channel mode messages */
boolean pgchanges = true;   /* show program changes */
boolean flush = false;	    /* flush all pending MIDI data */

uint32_t filter = 0;            /* remember state of midi filter */

uint32_t clockcount = 0;        /* count of clocks */
uint32_t actsensecount = 0;     /* cout of active sensing bytes */
uint32_t notescount = 0;        /* #notes since last request */
uint32_t notestotal = 0;        /* total #notes */

char val_format[] = "    Val %d\n";

/*****************************************************************************
*    Imported variables
*****************************************************************************/

extern  int     abort_flag;

/*****************************************************************************
*    Routines local to this module
*****************************************************************************/

private    void    mmexit(int code);
private    void    output(PmMessage data);
private    int     put_pitch(int p);
private    void    showhelp();
private    void    showbytes(PmMessage data, int len, boolean newline);
private    void    showstatus(boolean flag);
private    void    doascii(char c);
private    int     get_number(char *prompt);


/* read a number from console */
/**/
int get_number(char *prompt)
{
    char line[STRING_MAX];
    int n = 0, i;
    printf(prompt);
    while (n != 1) {
        n = scanf("%d", &i);
        fgets(line, STRING_MAX, stdin);

    }
    return i;
}


void receive_poll(PtTimestamp timestamp, void *userData)
{
    PmEvent event;
    int count;
    if (!active) return;
    while ((count = Pm_Read(midi_in, &event, 1))) {
        if (count == 1) output(event.message);
        else            printf(Pm_GetErrorText(count));
    }
}


/****************************************************************************
*               main
* Effect: prompts for parameters, starts monitor
****************************************************************************/

int main(int argc, char **argv)
{
    char *argument;
    int inp;
    PmError err;
    int i;
    if (argc > 1) { /* first arg can change defaults */
        argument = argv[1];
        while (*argument) doascii(*argument++);
    }
    showhelp();
    /* use porttime callback to empty midi queue and print */
    Pt_Start(1, receive_poll, 0);
    /* list device information */
    printf("MIDI input devices:\n");
    for (i = 0; i < Pm_CountDevices(); i++) {
        const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
        if (info->input) printf("%d: %s, %s\n", i, info->interf, info->name);
    }
    inp = get_number("Type input device number: ");
    err = Pm_OpenInput(&midi_in, inp, NULL, 512, NULL, NULL);
    if (err) {
        printf(Pm_GetErrorText(err));
        Pt_Stop();
        mmexit(1);
    }
    Pm_SetFilter(midi_in, filter);
    inited = true; /* now can document changes, set filter */
    printf("Midi Monitor ready.\n");
    active = true;
    while (!done) {
        char s[100];
        if (fgets(s, 100, stdin)) {
            doascii(s[0]);
        }
    }
    active = false;
    Pm_Close(midi_in);
    Pt_Stop();
    Pm_Terminate();
    mmexit(0);
    return 0; // make the compiler happy be returning a value
}


/****************************************************************************
*               doascii
* Inputs:
*    char c: input character
* Effect: interpret to revise flags
****************************************************************************/

private void doascii(char c)
{
    if (isupper(c)) c = tolower(c);
    if (c == 'q') done = true;
    else if (c == 'b') {
        bender = !bender;
        filter ^= PM_FILT_PITCHBEND;
        if (inited)
            printf("Pitch Bend, etc. %s\n", (bender ? "ON" : "OFF"));
    } else if (c == 'c') {
        controls = !controls;
        filter ^= PM_FILT_CONTROL;
        if (inited)
            printf("Control Change %s\n", (controls ? "ON" : "OFF"));
    } else if (c == 'h') {
        pgchanges = !pgchanges;
        filter ^= PM_FILT_PROGRAM;
        if (inited)
            printf("Program Changes %s\n", (pgchanges ? "ON" : "OFF"));
    } else if (c == 'n') {
        notes = !notes;
        filter ^= PM_FILT_NOTE;
        if (inited)
            printf("Notes %s\n", (notes ? "ON" : "OFF"));
    } else if (c == 'x') {
        excldata = !excldata;
        filter ^= PM_FILT_SYSEX;
        if (inited)
            printf("System Exclusive data %s\n", (excldata ? "ON" : "OFF"));
    } else if (c == 'r') {
        realdata = !realdata;
        filter ^= (PM_FILT_PLAY | PM_FILT_RESET | PM_FILT_TICK | PM_FILT_UNDEFINED);
        if (inited)
            printf("Real Time messages %s\n", (realdata ? "ON" : "OFF"));
    } else if (c == 'k') {
        clksencnt = !clksencnt;
        filter ^= PM_FILT_CLOCK;
        if (inited)
            printf("Clock and Active Sense Counting %s\n", (clksencnt ? "ON" : "OFF"));
        if (!clksencnt) clockcount = actsensecount = 0;
    } else if (c == 's') {
        if (clksencnt) {
            if (inited)
                printf("Clock Count %ld\nActive Sense Count %ld\n", 
                        (long) clockcount, (long) actsensecount);
        } else if (inited) {
            printf("Clock Counting not on\n");
        }
    } else if (c == 't') {
        notestotal+=notescount;
        if (inited)
            printf("This Note Count %ld\nTotal Note Count %ld\n",
                    (long) notescount, (long) notestotal);
        notescount=0;
    } else if (c == 'v') {
        verbose = !verbose;
        if (inited)
            printf("Verbose %s\n", (verbose ? "ON" : "OFF"));
    } else if (c == 'm') {
        chmode = !chmode;
        if (inited)
            printf("Channel Mode Messages %s", (chmode ? "ON" : "OFF"));
    } else {
        if (inited) {
            if (c == ' ') {
                PmEvent event;
                while (Pm_Read(midi_in, &event, 1)) ;	/* flush midi input */
                printf("...FLUSHED MIDI INPUT\n\n");
            } else showhelp();
        }
    }
    if (inited) Pm_SetFilter(midi_in, filter);
}



private void mmexit(int code)
{
    /* if this is not being run from a console, maybe we should wait for
     * the user to read error messages before exiting
     */
    exit(code);
}


/****************************************************************************
*               output
* Inputs:
*    data: midi message buffer holding one command or 4 bytes of sysex msg
* Effect: format and print  midi data
****************************************************************************/

char vel_format[] = "    Vel %d\n";

private void output(PmMessage data)
{
    int command;    /* the current command */
    int chan;   /* the midi channel of the current event */
    int len;    /* used to get constant field width */

    /* printf("output data %8x; ", data); */

    command = Pm_MessageStatus(data) & MIDI_CODE_MASK;
    chan = Pm_MessageStatus(data) & MIDI_CHN_MASK;

    if (in_sysex || Pm_MessageStatus(data) == MIDI_SYSEX) {
#define sysex_max 16
        int i;
        PmMessage data_copy = data;
        in_sysex = true;
        /* look for MIDI_EOX in first 3 bytes 
         * if realtime messages are embedded in sysex message, they will
         * be printed as if they are part of the sysex message
         */
        for (i = 0; (i < 4) && ((data_copy & 0xFF) != MIDI_EOX); i++) 
            data_copy >>= 8;
        if (i < 4) {
            in_sysex = false;
            i++; /* include the EOX byte in output */
        }
        showbytes(data, i, verbose);
        if (verbose) printf("System Exclusive\n");
    } else if (command == MIDI_ON_NOTE && Pm_MessageData2(data) != 0) {
        notescount++;
        if (notes) {
            showbytes(data, 3, verbose);
            if (verbose) {
                printf("NoteOn  Chan %2d Key %3d ", chan, Pm_MessageData1(data));
                len = put_pitch(Pm_MessageData1(data));
                printf(vel_format + len, Pm_MessageData2(data));
            }
        }
    } else if ((command == MIDI_ON_NOTE /* && Pm_MessageData2(data) == 0 */ ||
               command == MIDI_OFF_NOTE) && notes) {
        showbytes(data, 3, verbose);
        if (verbose) {
            printf("NoteOff Chan %2d Key %3d ", chan, Pm_MessageData1(data));
            len = put_pitch(Pm_MessageData1(data));
            printf(vel_format + len, Pm_MessageData2(data));
        }
    } else if (command == MIDI_CH_PROGRAM && pgchanges) {
        showbytes(data, 2, verbose);
        if (verbose) {
            printf("  ProgChg Chan %2d Prog %2d\n", chan, Pm_MessageData1(data) + 1);
        }
    } else if (command == MIDI_CTRL) {
               /* controls 121 (MIDI_RESET_CONTROLLER) to 127 are channel
                * mode messages. */
        if (Pm_MessageData1(data) < MIDI_ALL_SOUND_OFF) {
            showbytes(data, 3, verbose);
            if (verbose) {
                printf("CtrlChg Chan %2d Ctrl %2d Val %2d\n",
                       chan, Pm_MessageData1(data), Pm_MessageData2(data));
            }
        } else /* channel mode */ if (chmode) {
            showbytes(data, 3, verbose);
            if (verbose) {
                switch (Pm_MessageData1(data)) {
                  case MIDI_ALL_SOUND_OFF:
                      printf("All Sound Off, Chan %2d\n", chan);
                    break;
                  case MIDI_RESET_CONTROLLERS:
                    printf("Reset All Controllers, Chan %2d\n", chan);
                    break;
                  case MIDI_LOCAL:
                    printf("LocCtrl Chan %2d %s\n",
                            chan, Pm_MessageData2(data) ? "On" : "Off");
                    break;
                  case MIDI_ALL_OFF:
                    printf("All Off Chan %2d\n", chan);
                    break;
                  case MIDI_OMNI_OFF:
                    printf("OmniOff Chan %2d\n", chan);
                    break;
                  case MIDI_OMNI_ON:
                    printf("Omni On Chan %2d\n", chan);
                    break;
                  case MIDI_MONO_ON:
                    printf("Mono On Chan %2d\n", chan);
                    if (Pm_MessageData2(data))
                        printf(" to %d received channels\n", Pm_MessageData2(data));
                    else
                        printf(" to all received channels\n");
                    break;
                  case MIDI_POLY_ON:
                    printf("Poly On Chan %2d\n", chan);
                    break;
                }
            }
        }
    } else if (command == MIDI_POLY_TOUCH && bender) {
        showbytes(data, 3, verbose);
        if (verbose) {
            printf("P.Touch Chan %2d Key %2d ", chan, Pm_MessageData1(data));
            len = put_pitch(Pm_MessageData1(data));
            printf(val_format + len, Pm_MessageData2(data));
        }
    } else if (command == MIDI_TOUCH && bender) {
        showbytes(data, 2, verbose);
        if (verbose) {
            printf("  A.Touch Chan %2d Val %2d\n", chan, Pm_MessageData1(data));
        }
    } else if (command == MIDI_BEND && bender) {
        showbytes(data, 3, verbose);
        if (verbose) {
            printf("P.Bend  Chan %2d Val %2d\n", chan,
                    (Pm_MessageData1(data) + (Pm_MessageData2(data)<<7)));
        }
    } else if (Pm_MessageStatus(data) == MIDI_SONG_POINTER) {
        showbytes(data, 3, verbose);
        if (verbose) {
            printf("    Song Position %d\n",
                    (Pm_MessageData1(data) + (Pm_MessageData2(data)<<7)));
        }
    } else if (Pm_MessageStatus(data) == MIDI_SONG_SELECT) {
        showbytes(data, 2, verbose);
        if (verbose) {
            printf("    Song Select %d\n", Pm_MessageData1(data));
        }
    } else if (Pm_MessageStatus(data) == MIDI_TUNE_REQ) {
        showbytes(data, 1, verbose);
        if (verbose) {
            printf("    Tune Request\n");
        }
    } else if (Pm_MessageStatus(data) == MIDI_Q_FRAME && realdata) {
        showbytes(data, 2, verbose);
        if (verbose) {
            printf("    Time Code Quarter Frame Type %d Values %d\n",
                    (Pm_MessageData1(data) & 0x70) >> 4, Pm_MessageData1(data) & 0xf);
        }
    } else if (Pm_MessageStatus(data) == MIDI_START && realdata) {
        showbytes(data, 1, verbose);
        if (verbose) {
            printf("    Start\n");
        }
    } else if (Pm_MessageStatus(data) == MIDI_CONTINUE && realdata) {
        showbytes(data, 1, verbose);
        if (verbose) {
            printf("    Continue\n");
        }
    } else if (Pm_MessageStatus(data) == MIDI_STOP && realdata) {
        showbytes(data, 1, verbose);
        if (verbose) {
            printf("    Stop\n");
        }
    } else if (Pm_MessageStatus(data) == MIDI_SYS_RESET && realdata) {
        showbytes(data, 1, verbose);
        if (verbose) {
            printf("    System Reset\n");
        }
    } else if (Pm_MessageStatus(data) == MIDI_TIME_CLOCK) {
        if (clksencnt) clockcount++;
        else if (realdata) {
            showbytes(data, 1, verbose);
            if (verbose) {
                printf("    Clock\n");
            }
        }
    } else if (Pm_MessageStatus(data) == MIDI_ACTIVE_SENSING) {
        if (clksencnt) actsensecount++;
        else if (realdata) {
            showbytes(data, 1, verbose);
            if (verbose) {
                printf("    Active Sensing\n");
            }
        }
    } else showbytes(data, 3, verbose);
    fflush(stdout);
}


/****************************************************************************
*               put_pitch
* Inputs:
*    int p: pitch number
* Effect: write out the pitch name for a given number
****************************************************************************/

private int put_pitch(int p)
{
    char result[8];
    static char *ptos[] = {
        "c", "cs", "d", "ef", "e", "f", "fs", "g",
        "gs", "a", "bf", "b"    };
    /* note octave correction below */
    sprintf(result, "%s%d", ptos[p % 12], (p / 12) - 1);
    printf(result);
    return strlen(result);
}


/****************************************************************************
*               showbytes
* Effect: print hex data, precede with newline if asked
****************************************************************************/

char nib_to_hex[] = "0123456789ABCDEF";

private void showbytes(PmMessage data, int len, boolean newline)
{
    int count = 0;
    int i;

/*    if (newline) {
        putchar('\n');
        count++;
    } */
    for (i = 0; i < len; i++) {
        putchar(nib_to_hex[(data >> 4) & 0xF]);
        putchar(nib_to_hex[data & 0xF]);
        count += 2;
        if (count > 72) {
            putchar('.');
            putchar('.');
            putchar('.');
            break;
        }
        data >>= 8;
    }
    putchar(' ');
}



/****************************************************************************
*               showhelp
* Effect: print help text
****************************************************************************/

private void showhelp()
{
    printf("\n");
    printf("   Item Reported  Range     Item Reported  Range\n");
    printf("   -------------  -----     -------------  -----\n");
    printf("   Channels       1 - 16    Programs       1 - 128\n");
    printf("   Controllers    0 - 127   After Touch    0 - 127\n");
    printf("   Loudness       0 - 127   Pitch Bend     0 - 16383, center = 8192\n");
    printf("   Pitches        0 - 127, 60 = c4 = middle C\n");
    printf(" \n");
    printf("n toggles notes");
    showstatus(notes);
    printf("t displays noteon count since last t\n");
    printf("b toggles pitch bend, aftertouch");
    showstatus(bender);
    printf("c toggles continuous control");
    showstatus(controls);
    printf("h toggles program changes");
    showstatus(pgchanges);
    printf("x toggles system exclusive");
    showstatus(excldata);
    printf("k toggles clock and sense counting only");
    showstatus(clksencnt);
    printf("r toggles other real time messages & SMPTE");
    showstatus(realdata);
    printf("s displays clock and sense count since last k\n");
    printf("m toggles channel mode messages");
    showstatus(chmode);
    printf("v toggles verbose text");
    showstatus(verbose);
    printf("q quits\n");
    printf("\n");
}

/****************************************************************************
*               showstatus
* Effect: print status of flag
****************************************************************************/

private void showstatus(boolean flag)
{
    printf(", now %s\n", flag ? "ON" : "OFF" );
}
