/* record.c -- keyboard to adagio recorder
 * Copyright 1989 Carnegie Mellon University
 *
 * the interface consists of three routines:
 *      rec_init()      -- initialization
 *      rec_event(byte *data)   -- called to insert (record) midi data, 
 *                              -- returns FALSE if no more space
 *      rec_final()     -- called to finish up
 */

/*****************************************************************************
*       Change Log
*  Date | Change
*-----------+-----------------------------------------------------------------
* 27-Feb-86 | Created changelog
*           | Use pedal information when computing durations (code taken
*           |  from transcribe.c)
* 23-Mar-86 | Determine size of transcription when rec_init is called.
* 21-May-86 | Major rewrite to use continuous controls (code taken 
*           |  from transcribe.c)
*  1-Aug-87 | F.H. Changed rec_init() to new memory handling.
* 17-Oct-88 | JCD : portable version.
* 31-Jan-90 | GWL : cleaned up for LATTICE
* 30-Jun-90 | RBD : further changes
*  2-Apr-91 | JDW : further changes
* 28-Apr-03 | DM  : changed for portability; true->TRUE, false->FALSE
*****************************************************************************/

#include "switches.h"

#include <stdio.h>
#include <stdlib.h>

#include "cext.h"
#include "midifns.h"
#include "userio.h"
#include "midicode.h"
#include "record.h"
#include "cmdline.h"

extern long space;    /* how much space is left? */

int debug_rec = FALSE;    /* verbose debug flag for this module */

long max_notes = -1L;    /* -1 is flag that space must be allocated */

time_type previous_time;

/****************************************************************
* data structure notes: the midi stream is stored as an array 
* of 4-byte records, each of which is either a time or midi
* data.     Midi data always begins with a control byte (high
* order bit set), and it is assumed times are positive (high
* order bit clear), so the two are easy to distinguish
* IF THE COMPILER PUTS THESE BITS IN THE SAME PLACE.  It looks
* like the high order byte of the time lines up with the last
* byte of a 4 byte array, so we will always set the high order
* bit of the last array byte when the first 3 bytes are filled
* with MIDI data.  This is refered to as the "tag" bit.
* WARNING: Lattice C longs are UNSIGNED, therefore always
* positive.  Test the high order bit with a mask.
****************************************************************/

#define MIDI_CMD_BIT    0x80
#define HIGH_BIT        0x80000000
#define istime(note) (!(((note)->when) & HIGH_BIT))

typedef union note_struct {
    byte n[4];
    long when;
} 
*note_type, note_node;

private note_type event_buff;    /* pointer to allocated buffer */
private note_type next;    /* pointer to next entry in buffer */
private note_type last;    /* pointer to last entry in buffer */
private int pile_ups;    /* inner loop iteration count */
private int max_pile_up;    /* maximum of pile_ups */
private boolean fixed_octave;	/* used to avoid many error messages */

/****************************************************************************
*    Routines local to this module
****************************************************************************/
private void    bend_filter(note_type note, note_type last, long now);
private void    byteorder(void);
private void    ctrl_filter(note_type note, note_type last, long now);
private int     event_bend(note_type note);
private void    filter(note_type last);
private long    getdur(int i, note_type last, int ped, long now);
private long    getnext(int i, note_type last, long now);
private char    map_ctrl(int control);
private void    output(FILE *fp, note_type last, boolean absflag);

/****************************************************************************
*               bend_filter
* Inputs:
*    note_type note: the current note
*    note_type last: the last recorded event
*    long now: the current time
* Effect:
*    remove pitch bend events in same 0.01 sec time slot
* Implementation:
*    If the current event is a pitch bend that bends again
*    in the same time slot, make it a no-op by replacing it with
*    the time.
****************************************************************************/

private void bend_filter(note_type note, note_type last, long now)
{
    /* first see if there is another bend in this time
         * slot.
         */
    note_type note2 = note + 1;
    while (note2 < last) {
        if (istime(note2) && (note2->when > now)) {
            break; /* new time slot */
        } 
        else if (note->n[0] == note2->n[0]) {
            note->when = now;
            return; /* found another bend */
        }
        note2++;
    }
}

/****************************************************************************
*               byteorder
* Effect: 
*    check out assumptions about byte order and placement
****************************************************************************/

private void byteorder(void)
{
    note_node test_event;
    if ((sizeof(test_event) != 4) ||
        (sizeof(test_event.when) != 4) ||
        (sizeof(test_event.n[0]) != 1)) {
        gprintf(ERROR, "implementation error: size problem\n");
        EXIT(1);
    }
    test_event.n[0] = 0x12;
    test_event.n[1] = 0x34;
    test_event.n[2] = 0x56;
    test_event.n[3] = 0x78;
    if ((test_event.when != 0x78563412) &&
        (test_event.when != 0x12345678)) {
        gprintf(ERROR, "implementation error: layout problem\n");
        EXIT(1);
    }
}

/****************************************************************************
*               ctrl_filter
* Inputs:
*    note_type note: the current note
*    note_type last: the last recorded event
*    long now: the current time
* Effect:
*    remove ctrl change events in same 0.01 sec time slot
* Implementation:
*    If the current event is a control change that changes again
*    in the same time slot, make it a no-op by replacing it with
*    the time.
****************************************************************************/

private void ctrl_filter(note_type note, note_type last, long now)
{
    /* see if there is another control change in this time
         * slot.
         */
    note_type note2 = note+1;
    while (note2 < last) {
        if (istime(note2) && (note2->when > now)) {
            break;    /* new time slot */
        } 
        else if ((note->n[0] == note2->n[0]) &&
            (note->n[1] == note2->n[1])) {
            note->when = now;
            return; /* found another change */
        }
        note2++;
    }
}

/****************************************************************************
*               event_bend
* Inputs:
*    note_type note: pointer to a pitch bend event
* Outputs:
*    returns int: an 8 bit pitch bend number
****************************************************************************/

private int event_bend(note_type note)
{
    return((int) (((note->n[1]) >> 6) + ((note->n[2]) << 1)));
}

/****************************************************************************
*               filter
* Inputs:
*    note_type last: the last note recorded
* Effect: allow only one control change per time slot (0.01 sec)
* Implementation:
*    call ctrl_filter and bend_filter to overwrite control changes with
*    noop data (the current time is used as a noop)
****************************************************************************/

private void filter(note_type last)
{
    note_type note;    /* loop control variable */
    long now=0;   /* last time seen */
    int command;    /* command pointed to by note */

    for (note = event_buff; note <= last; note++) {
        if (istime(note)) {
            now = note->when;
        } 
        else {
            command = note->n[0] & MIDI_CODE_MASK;

            if (command == MIDI_CTRL &&
                note->n[1] == SUSTAIN) {
                /* do nothing */;
            } 
            else if (command == MIDI_CTRL) {
                ctrl_filter(note, last, now);
            } 
            else if (command == MIDI_TOUCH) {
                bend_filter(note, last, now);    /* bend and touch use the */
            } 
            else if (command == MIDI_BEND) {    /*  same filter routines  */
                bend_filter(note, last, now);
            }
        }
    }
}


/****************************************************************************
*               getdur
* Inputs:
*    int i: index of the note
*    note_type last: pointer to the last event recorded
*    int ped: TRUE if pedal is down at event i
*    long now: the time at event i
* Outputs:
*    returns long: the duration of note i
* Assumes:
*    assumes i is a note
* Implementation:
*    This is tricky because of pedal messages.  The note is kept on by
*    either the key or the pedal.  Keep 2 flags, key and ped.  Key is
*    turned off when a key is released, ped goes off and on with pedal.
*    Note ends when (1) both key and ped are FALSE, (2) key is
*    pressed (this event will also start another note).
****************************************************************************/

private long getdur(int i, note_type last, int ped, long now)
{
    int key = TRUE;    /* flag that says if note is on */
    long start = now;
    int chan = event_buff[i].n[0] & MIDI_CHN_MASK;
    int pitch = event_buff[i].n[1];
    note_type note = &(event_buff[i+1]);
    int noteon; /* TRUE if a noteon message received on chan */
    int keyon;    /* TRUE if noteon message had non-zero velocity */

    /* search from the next event (i+1) to the end of the buffer:
         */
    for (; note < last; note++) {
        if (istime(note)) {
            now = note->when;
        } 
        else {
            noteon = keyon = FALSE;
            if ((note->n[0] & MIDI_CHN_MASK) == chan) {
                noteon = ((note->n[0] & MIDI_CODE_MASK) == MIDI_ON_NOTE) &&
                    (note->n[1] == pitch);
                keyon = noteon && (note->n[2] != 0);
                if ((noteon && (note->n[2] == 0)) ||
                    (((note->n[0] & MIDI_CODE_MASK) == MIDI_OFF_NOTE) &&
                    (note->n[1] == pitch))) key = FALSE;
                if (((note->n[0] & MIDI_CODE_MASK) == MIDI_CTRL) &&
                    note->n[1] == SUSTAIN && note->n[2] == 127) ped = TRUE;
                if (((note->n[0] & MIDI_CODE_MASK) == MIDI_CTRL) &&
                    note->n[1] == SUSTAIN && note->n[2] == 0) ped = FALSE;

                if ((!key && !ped) || keyon)
                    return(now - start);
            }
        }
    }
    return(last->when - start);
}

/****************************************************************************
*               getnext
* Inputs:
*    int i: the index of the current note
*    note_type last: pointer to last valid data
*    long now: the current time
* Outputs:
*    returns long: the time of the next note, program, or control change
*       (returns time of last event if nothing else is found)
****************************************************************************/

private long getnext(int i, note_type last, long now)
{
    i++;    /* advance to next item */
    for (; event_buff + i < last; i++) {
        note_type note = &(event_buff[i]);
        int cmd = note->n[0] & MIDI_CODE_MASK;

        if (istime(note)) {
            now = note->when;
        } 
        else if (((cmd == MIDI_ON_NOTE) &&
            (note->n[2] != 0)) /* note on */ ||
            (cmd == MIDI_CH_PROGRAM) /* program change */ ||
            ((cmd == MIDI_CTRL) &&
            (note->n[1] != SUSTAIN) /* control change */ ) ||
            (cmd == MIDI_TOUCH) ||
            (cmd == MIDI_BEND)) {
            return(now);
        }
    }
    return(last->when);
}

/****************************************************************************
*               map_ctrl
* Inputs:
*    int control: a midi control number
* Outputs:
*    returns char: an adagio control change command letter, EOS if
*       control change is not one of PORTARATE, PORTASWITCH,
*       MODWHEEL, FOOT
****************************************************************************/

private char map_ctrl(int control)
{
    switch (control) {
/* 'J' is no longer code for PORTARATE
      case PORTARATE:
        return 'J'; */
      case PORTASWITCH:
        return 'K';
      case MODWHEEL:
        return 'M';
      case VOLUME:
        return 'X';
      default:
        return EOS;
    }
#ifdef LATTICE322
    return EOS;    /* make Lattice C type checker happy */
#endif
}

/****************************************************************************
*               output
* Inputs:
*    FILE *fp: an opened file pointer
*    note_type last: the last data in the buffer
*    boolean absflag: set to TRUE if first line of the adagio score should
*       include the absolute time
* Effect: 
*    write adagio file using data in event_buff
* Implementation:
*    NOTE: put all program changes in rests
*    use N(ext) notation for all timing
*    output no more than one continuous parameter change per
*    clock tick for each continuous change parameter
****************************************************************************/

private void output(FILE *fp, note_type last, boolean absflag)
{
    int i;                      /* loop counter */
    int command;                /* the current command */
    int voice;			/* the midi channel of the current event */
    int last_velocity = -1;	/* used to filter repeated Lnn attributes */
    int last_voice = 0; 	/* the default adagio channel (1) */
    int ped = FALSE;            /* flag maintains state of pedal */
    /* size of event_buff is seems to be about 20000 events */
    int how_many = (int) (last - event_buff);
    long now=0;                 /* the time of the next event */

    if (fp == NULL) {
        gprintf(ERROR, "internal error: output called with NULL file.\n");
        EXIT(1);
    }

    if (debug_rec)
        gprintf(GDEBUG,"hint: if file is not being closed, decrease MAXSPACE\n");


    fprintf(fp, "!MSEC\n");     /* times will be in milliseconds */
    /* set the initial absolute time, all other times are relative */

    if (absflag) {
        now = event_buff[0].when;
        if (now < 0) {
            fprintf(fp, "* First event took place at Adagio time %d,\n",
                    (int)now);
            fprintf(fp, "*  but Adagio cannot represent negative times,\n");
            fprintf(fp, "*  so this entire score will be %d ms late\n",
                    (int)-now);
            gprintf(TRANS, "First event took place at Adagio time %d!\n",
                    (int)now);
            gprintf(TRANS, "All events times will be %d ms late\n",
                    (int)-now);
            now = 0L;
        }
        fprintf(fp, "T%ld ", now);
    }

    for (i = 0; i < how_many; i++) {
        if (debug_rec) {
            gprintf(GDEBUG,"ev %d: %x %x %x (%ld)\n", i, event_buff[i].n[0],
            event_buff[i].n[1], event_buff[i].n[2], event_buff[i].when);
        }

        if (istime(event_buff+i)) {
            now = event_buff[i].when;
            if (debug_rec) gprintf(GDEBUG,"i = %d, now = %ld\n", i, now);
        } else {
            boolean needs_voice = TRUE;
            command = event_buff[i].n[0] & MIDI_CODE_MASK;
            voice = event_buff[i].n[0] & MIDI_CHN_MASK;

            if (command == MIDI_ON_NOTE && event_buff[i].n[2] != 0) {
                int velocity =	event_buff[i].n[2];
                write_pitch(fp, event_buff[i].n[1]);
                fprintf(fp, " U%ld", getdur(i, last, ped, now));
                if (last_velocity != velocity) {
                    fprintf(fp, " L%d", velocity);
                    last_velocity = velocity;
                }
            } else if (command == MIDI_CH_PROGRAM) {
                fprintf(fp, "Z%d", event_buff[i].n[1] + 1);
            } else if (command == MIDI_CTRL &&
                event_buff[i].n[1] == SUSTAIN) {
                ped = (event_buff[i].n[2] != 0);
                needs_voice = FALSE;
            } else if (command == MIDI_CTRL) {
                char c = map_ctrl(event_buff[i].n[1]);
                if (c != EOS) fprintf(fp, "%c%d", c, event_buff[i].n[2]);
                else fprintf(fp, "~%d(%d)", event_buff[i].n[1], event_buff[i].n[2]);
            } else if (command == MIDI_TOUCH) {
                fprintf(fp, "O%d", event_buff[i].n[1]);
            } else if (command == MIDI_BEND) {
                fprintf(fp, "Y%d", event_bend(&event_buff[i]));
            } else if (command == MIDI_ON_NOTE || command == MIDI_OFF_NOTE) {
                needs_voice = FALSE; /* ignore note-offs */
            } else {
                gprintf(ERROR, "Command 0x%x ignored\n", command);
                needs_voice = FALSE;
            }
            if (needs_voice) {
                if (last_voice != voice) {
                    fprintf(fp, " V%d", voice + 1);
                    last_voice = voice;
                }
                fprintf(fp, " N%d", (int)(getnext(i, last, now) - now));
                fprintf(fp, "\n");
            }
        }
    }
}


/****************************************************************************
*               write_pitch
* Inputs:
*    FILE *fp: an open file
*    int p: a pitch number
* Effect: write out the pitch name for a given number
****************************************************************************/

void write_pitch(FILE *fp, int p)
{
    static char *ptos[] = {
        "C", "CS", "D", "EF", "E", "F", "FS", "G",
        "GS", "A", "BF", "B"	};
    /* avoid negative numbers: adagio can't express lowest octave: */
    while (p < 12) {
        if (!fixed_octave) {
            gprintf(ERROR, "%s%s%s",
                    "A low note was transposed up an octave\n",
                    "(Adagio cannot express the lowest MIDI octave).\n",
                    "This message will appear only once.\n");
            fixed_octave = TRUE;
        }
        p += 12;
    }
    fprintf(fp, "%s%d", ptos[p % 12], (p / 12) - 1);
}

/**********************************************************************
*           rec_final
* Inputs:
*    boolean absflag: output absolute time of first note if TRUE
* Effect:
*    Write recorded data to a file
**********************************************************************/

void rec_final(FILE *fp, boolean absflag)
{
    next->when = gettime();
    last = next;
    if (debug_rec) gprintf(GDEBUG,"max_pile_up = %d, ", max_pile_up);
    gprintf(TRANS,"%ld times and events recorded.\n", 
                  (long) (last - event_buff));
    filter(last);
    output(fp, last, absflag);
    fclose(fp);
    FREE(event_buff);
    max_notes = -1;
}

/****************************************************************************
*               rec_init
* Inputs:
*    char *file:  pointer to file name from command line (if any)
*    boolean bender: TRUE if pitch bend should be enabled
* Outputs:
*    return TRUE if initialization succeeds
* Effect:
*    prepares module to record midi input
****************************************************************************/

/* ENOUGH_ROOM says if we have room for 10000 events + 10000 timestamps =
 * 20000 note_struct's, then that's "enough room" for recording a sequence.
 * If more ram is available, it won't be used.  If less is available, we'll
 * use as much as we can get, minus "SPACE_FOR_PLAY", which leaves a little
 * bit of spare ram in case Moxc or stdio need to allocate some space.
 *      For DOS, we limit recording space to 64K.
 */
#ifdef DOS
#define ENOUGH_ROOM 64000L
#else
#define ENOUGH_ROOM (20000L * sizeof(union note_struct))
#endif


boolean rec_init(boolean bender)
{
    size_t biggestChunk, spaceForRecord;

    debug_rec = cl_switch("debug");
    byteorder();
    pile_ups = 0;
    max_pile_up = 0;
    previous_time = (unsigned) -1L; /* this will force putting in initial timestamp */
    fixed_octave = FALSE;

    if (max_notes == -1) {    /* allocate space 1st time rec_init called */
        biggestChunk = AVAILMEM;
        if (biggestChunk <= SPACE_FOR_PLAY) {
            /* not enough memory; give up */
            return(FALSE);
        } 
        else {
            spaceForRecord =
                MIN((biggestChunk - SPACE_FOR_PLAY), ENOUGH_ROOM);
            /* leave SPACE_FOR_PLAY contiguous bytes of memory */
        }
        max_notes = (long) (spaceForRecord / sizeof(note_node));
        /*    gprintf(GDEBUG,"max_notes = %d\n", max_notes);*/
        event_buff = (note_type) MALLOC(spaceForRecord);
        if (event_buff == NULL) {
            /* should never happen */
            gprintf(FATAL, "Implementation error (record.c): getting memory.");
            return FALSE;
        }
    }
    next = event_buff;
    last = event_buff + max_notes - 2; /* it is critical that last not point
                                        * to the very last storage loc */
    midi_cont(bender);
    return((boolean)(max_notes > 10));
    /* it would be silly to record with only room enough for 10 notes! */
}

#ifdef NEED_REC_EVENT
/* 
This function was part of the CMU MIDI Toolkit. It provided a constant time
fast way to record midi data into a buffer. After recording, the buffered 
data could be transferred into an Adagio score structure, which involved 
linked list allocation and insertion that might have caused performance 
problems. This code uses the high-order bit of longs to distinguish timestamps
from data (MIDI messages), but the code seems to assume little endian, and 
I'm not sure how it worked on both Intel and 68000 processors. Rather than
look more closely or fix it, I'm commenting it out because Nyquist does not
have any MIDI I/O capability and does not need the function. It is here for
completeness, since this is probably the only archived version of the CMU
MIDI Toolkit. */

/****************************************************************************
*               rec_event
* Inputs:
*    long time: the current time
*    long data: midi data to record
* Outputs:
*    returns FALSE if there is no more memory
* Effect: reads and stores any input
* Implementation:
*    time stamps and midi events share the same buffer of 4-byte events
*    save time at most once per call to rec_poll
*    save time only if it changes
****************************************************************************/

boolean rec_event(long *data, time_type time)
{
    /* can't allow negative time because sign bit distinguishes 
     * data from time: */
    if (time < 0) time = 0;

    if (previous_time != time) {
        next++->when = previous_time = time;
        if (next >= last) goto overflow;
    }

    next->when = *data;
    next++->n[3] = MIDI_CMD_BIT;        /* set tag bit */
    if (next >= last) goto overflow;
    return TRUE;

overflow:
    next = last; /* last doesn't really point to last storage */
    gprintf(ERROR, "No more memory.\n");
    return FALSE;
}

#endif
