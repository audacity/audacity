/*
 * seqmread.c
 * 
 * Convert a MIDI file to a seq.
 */

/* Copyright 1989 Carnegie Mellon University */

/*****************************************************************************
*       Change Log
*       Date    | who : Change
*-----------+-----------------------------------------------------------------
* 17-Feb-92 | GWL : only one stdio.h
*                 : fix to satisfy compiler:
                    void returns, time_type giotime(), int filegetc()
*****************************************************************************/

#include "switches.h"
#include "stdio.h"
#include "cext.h"
#include "cmdline.h"
#include "midifns.h" /* to get time_type */
#include "timebase.h"
#include "moxc.h"    /* to get debug declared */
#include "seq.h"
#include "seqread.h" 	/* to get scale */
#include "seqmread.h"
#include "userio.h"
#include "ctype.h"

#include "midifile.h"
#include "tempomap.h"

int filegetc(void);
void initfuncs(void);
void prtime(void);
void snding_free(void);

typedef struct snding_struct {
    struct snding_struct *next;
    event_type event_ptr;
    int pitch;
    int channel;
} snding_node, *snding_type;

#define snding_alloc() (snding_type) memget(sizeof(snding_node))
#define snding_free(s) memfree(s, sizeof(snding_node))

snding_type snding_list = NULL;

tempomap_type the_tempomap;
event_type initial_clock;       /* remember the first clock event */
long prev_ticksize;             /* remember the previous ticksize */
int sysex_id = 0;


void smf_noteoff(int, int, int);
void smf_error(char *);
void smf_header(int, int, int);
void smf_trackstart(void);
void smf_trackend(void);
void smf_noteon(int, int, int);
void smf_pressure(int, int, int);
void smf_parameter(int, int, int);
void smf_pitchbend(int, int, int);
void smf_program(int, int);
void smf_chanpressure(int, int);
void smf_sysex(int, char *);
void smf_metamisc(int, int, char *);
void smf_metaseq(int);
void smf_metaeot(void);
void smf_timesig(int, int, int, int);
void smf_smpte(int, int, int, int, int);
void smf_tempo(int);
void smf_keysig(int, int);
void smf_metaspecial(int, int, char *);
void smf_metatext(int, int, char *);
void smf_arbitrary(int, char *);

private seq_type the_score;

static FILE *F;

int filegetc(void)
{
/*      int temp = getc(F);
        printf(" %x ", temp);*/
        return(int)(getc(F));
}

void seq_read_smf(seq_type seq, FILE *fp)
{
    F = fp;
    initfuncs();
    sysex_id = 0;       /* sysex in seq has to correspond to a symbol */
    the_score = seq;    /* current sequence is a global within this module */
    if (!seq) return;
    the_tempomap = tempomap_create();
    /* insert an initial clock to correspond to the default midifile tempo
       (tempomap_create creates a corresponding initial entry in the tempomap)
       (see smf_tempo for explanation of the scale() call)
     */
    initial_clock = insert_clock(the_score, 0L, 0, 500L << 16);
                  /*   scale(24 * 500000, 1 << 16, 24000) */
    if (!the_tempomap) return;
    Mf_getc = filegetc;
    midifile();
    /* fmac_close(F); -- do not close the file because the caller might try to 
     * close it (in fact XLISP insists on closing it as a side effect of
     * garbage collection.
     */
    gprintf(TRANS, "\nLoaded Midi file with %ld note(s), %ld ctrl(s).\n\n",
        seq_notecount(seq), seq_ctrlcount(seq));
    seq_reset(seq);
    while (snding_list) {
        snding_type snding = snding_list;
        snding_list = snding_list->next;
        gprintf(TRANS, "Note-on (key %d, chan %d) has no matching noteoff\n",
                snding->pitch, snding->channel + 1);
        snding_free(snding);
    }
    tempomap_free(the_tempomap);
}


/* gio_time -- get the time in millisec for Adagio */
/*
 * Since Adagio times are (in their precise form) 1/256 ms, we want
 * a similar time for midifiles, whose natural unit would be microseconds.
 * We'll shift the microsecond time by 2 to get 1/250 ms = 4 us units
 * and convert using the scale function when necessary.
 * Real time is the time of the last tempo change (last_tempo_time)
 * which is in 4us units + elapsed time.  
 * Elapsed time is the elapsed beats times the beat duration.
 * Elapsed beats is Mf_currtime - last_tempo_beat.
 * Beat duration is the specified tempo / division, where specified tempo
 *  is in microseconds, and division is parts per quarternote.
 */
unsigned long divisions = 24L;

time_type gio_time(void)
{
    return (tempomap_lookup(the_tempomap, Mf_currtime) + 125L) / 250L;
}


void smf_header(int format, int ntrks, int division)
{
/*      gprintf(TRANS, "Header format=%d ntrks=%d division=%d\n",
                format,ntrks,division); */
        if (format > 1) gprintf(TRANS, 
                          "Warning: format %d midi file may not work.\n",
                          format);
        divisions = division;
        /* adjust the initial tempochange */
        the_tempomap->entries->tempo = 500000L / division;
}


void smf_trackstart(void)
{
/*      gprintf(TRANS, "Track start\n"); */
}

void smf_trackend(void)
{
/*      gprintf(TRANS, "Track end\n"); */
}

void smf_noteon(int chan, int pitch, int vol)
{
        snding_type snding;
        if (vol == 0) {  /* convert to a noteoff */
            smf_noteoff(chan, pitch, 0);
            return;
        }
/*      prtime();
        gprintf(TRANS, "Note on, chan=%d pitch=%d vol=%d\n",chan+1,pitch,vol);
*/
        /* get ready to remember the sounding note */
        snding = snding_alloc();
        snding->next = snding_list;
        snding_list = snding;
        /* enter an event into score and remember it */
        snding->event_ptr = insert_note(the_score, gio_time(), 0, 
                                        chan + 1, pitch, 0L, vol);
        snding->pitch = pitch;
        snding->channel = chan;
}

void smf_noteoff(int chan, int pitch, int vol)
{
        snding_type *snding_ptr;
        register snding_type snding;
/*      prtime();
        gprintf(TRANS, "Note off, chan=%d pitch=%d vol=%d\n",chan+1,pitch,vol);
*/      /* search for the snding record */
        for (snding_ptr = &snding_list; 
               (snding = *snding_ptr) &&
                   ((snding->pitch != pitch) || (snding->channel != chan));
               snding_ptr = &(snding->next)) /* printf("* search *\n") */;
        if (!snding) {
            gprintf(TRANS, "Note off %d, channel %d ignored: no note on\n",
                        pitch, chan + 1);
        } else {
                event_type event = snding->event_ptr;
            event->u.note.ndur += (gio_time() - event->ntime) << 8;
            /* free the snding record */
            *snding_ptr = snding->next;
            snding_free(snding);
        }
}


void smf_pressure(int chan, int pitch, int press)
{
        prtime();
        gprintf(TRANS, "Pressure, chan=%d pitch=%d press=%d (IGNORED)\n",
                chan + 1, pitch, press);
}

void smf_parameter(int chan, int control, int value)
{
        int ctrl = 0;
/*      prtime();
        gprintf(TRANS, "Parameter, chan=%d c1=%d c2=%d\n",chan+1,control,value);
 */     /* see if the control is one of the standard Adagio controls that
           can be encoded in a special way.  If not, ctrl remains at zero.
         */
        switch (control) {
          case PORTASWITCH: ctrl = PSWITCH_CTRL; break;
          case MODWHEEL: ctrl = MODWHEEL_CTRL; break;
          case VOLUME: ctrl = VOLUME_CTRL; break;
        }
        if (ctrl) /* then do special ctrl insert and save storage */
            insert_ctrl(the_score, gio_time(), 0, ctrl, chan + 1, value);
        else insert_macctrl(the_score, gio_time(), 0, control, chan + 1, value);
}       


/* smf_pitchbend -- handle a pitch bend event */
/*
 * NOTE: the midifile code from Tim Thompson has the msb and lsb bytes swapped.
 *   Thus the parameter msb is really the low order byte and lsb is high order.
 */
void smf_pitchbend(int chan, int msb, int lsb)
{
/*      prtime();
        gprintf(TRANS, "Pitchbend, chan=%d msb=%d lsb=%d\n",chan+1,msb,lsb); */
        insert_ctrl(the_score, gio_time(), 0, BEND_CTRL, chan + 1,
                    ((lsb << 7) + msb) >> 6);
}

void smf_program(int chan, int program)
{
/*      prtime();
        gprintf(TRANS, "Program, chan=%d program=%d\n",chan+1,program); */
        insert_ctrl(the_score, gio_time(), 0, PROGRAM_CTRL, chan + 1, program);
}

void smf_chanpressure(int chan, int press)
{
/*      prtime();
        gprintf(TRANS, "Channel pressure, chan=%d pressure=%d\n",chan+1,press);
 */
        insert_ctrl(the_score, gio_time(), 0, TOUCH_CTRL, chan + 1, press);
}

void smf_sysex(int leng, char* mess)
{
        char symb[10];
        def_type defn;
        int i;
        sprintf(symb, "X%d", sysex_id++);
        if (leng > 255) {
            gprintf(TRANS, "sysex too long (%d bytes), ignored\n", leng - 2);
            return;
        }
        /* need to end up with a prefix of [0][length], so add 2 to length;
           note that this will copy past the end of the message -- this is
           slightly dangerous and definitely crufty: 
         */
        defn = insert_def(the_score, symb, (unsigned char *) mess, leng + 2);
        /* now fix up the definition by inserting the prefix bytes: */
        for (i = leng + 1; i > 1; i--) 
            defn->definition[i] = defn->definition[i - 2];
        defn->definition[0] = 0;
        defn->definition[1] = leng;
        insert_macro(the_score, gio_time(), 0, defn, 1, 0, NULL);
/*      prtime();
        gprintf(TRANS, "Sysex, leng=%d (IGNORED)\n",leng); */
}

void smf_metamisc(int type, int leng, char *mess)
{
        prtime();
        gprintf(TRANS,
                "Meta event, unrecognized, type=0x%02x leng=%d (IGNORED)\n",
                type, leng);
}

void smf_metaspecial(int type, int leng, char *mess)
{
        prtime();
        gprintf(TRANS, 
        "Meta event, sequencer-specific, type=0x%02x leng=%d (IGNORED)\n",
                type, leng);
}

void smf_metatext(int type, int leng, char *mess)
{
        static char *ttype[] = {
                NULL,
                "Text Event",           /* type=0x01 */
                "Copyright Notice",     /* type=0x02 */
                "Sequence/Track Name",
                "Instrument Name",      /* ...       */
                "Lyric",
                "Marker",
                "Cue Point",            /* type=0x07 */
                "Unrecognized"
        };
        int unrecognized = (sizeof(ttype)/sizeof(char *)) - 1;

        if ( type < 1 || type > unrecognized )
                type = unrecognized;
}

void smf_metaseq(int num)
{
        prtime();
        gprintf(TRANS, "Meta event, sequence number = %d (IGNORED)\n", num);
}

void smf_metaeot(void)
{
/*      prtime();
        gprintf(TRANS, "Meta event, end of track\n"); */
}

void smf_keysig(int sf, int mi)
{
/*      prtime();
        gprintf(TRANS, "Key signature, sharp/flats=%d  minor=%d\n", sf, mi); */
}

void smf_sqspecific(int id, char *msg)
{
    prtime();
    gprintf(TRANS, "Sequencer-specific event ID = %d (IGNORED)\n", id);
}

/* smf_tempo -- handle a midifile tempo change */
/*
 * NOTE: if divisions is positive, it gives time units per quarter, and
 * tempo is microsec per division.  The product is microsec per quarter.
 * To convert to ticksize (parameter to insert_clock), we divide by 24*1000
 * to get units of millisec and 24ths of quarter notes.  insert_clock
 * expects this to have a 16 bit fractional part.
 */
void smf_tempo(int tempo)
{
        time_type ctime = gio_time();
        long ticksize = scale(tempo, 1024L, 375L);
/*      (tempo / 24000) << 16;   microsec/clock converted to ms/quarter, shifted 16*/

/*      prtime();
        gprintf(TRANS, "Tempo, microseconds-per-MIDI-quarter-note = %ld\n",tempo);
*/ 
        tempomap_insert(the_tempomap, Mf_currtime, tempo / divisions);
        if (ctime == 0) {
            /* we already have a clock event at t=0 -> fix it */
            initial_clock->u.clock.ticksize = ticksize;
        } else { /* we need a new one */
            /* NOTE: after the first clock, insert clock events 1/2 tick early
                to make sure ticksize is set before clock_tick() wakes up and
                reads it.
             */
            insert_clock(the_score, ctime - (prev_ticksize >> 17), 0, ticksize);
            prev_ticksize = ticksize;
        }
}

void smf_timesig(int nn, int dd, int cc, int bb)
{
/*      int denom = 1;
        while ( dd-- > 0 )
                denom *= 2;
        prtime();
        gprintf(TRANS, 
"Time signature=%d/%d  MIDI-clocks/click=%d  32nd-notes/24-MIDI-clocks=%d\n",
                nn,denom,cc,bb); */
}

void smf_smpte(int hr, int mn, int se, int fr, int ff)
{
        prtime();
        gprintf(TRANS,
    "SMPTE, hour=%d minute=%d second=%d frame=%d fract-frame=%d (IGNORED)\n",
                hr, mn, se, fr, ff);
}

void smf_arbitrary(int leng, char *mess)
{
        prtime();
        gprintf(TRANS, "Arbitrary bytes, leng=%d (IGNORED)\n",leng);
}

void smf_error(char *msg)
{
    gprintf(ERROR, msg);
}


void prtime(void)
{
        gprintf(TRANS, "Time=%ld/%ld ",Mf_currtime, gio_time());
}

void initfuncs(void)
{
        Mf_error = smf_error;
        Mf_header =  smf_header;
        Mf_starttrack =  smf_trackstart;
        Mf_endtrack =  smf_trackend;
        Mf_on =  smf_noteon;
        Mf_off =  smf_noteoff;
        Mf_pressure =  smf_pressure;
        Mf_controller =  smf_parameter;
        Mf_pitchbend =  smf_pitchbend;
        Mf_program =  smf_program;
        Mf_chanpressure =  smf_chanpressure;
        Mf_sysex =  smf_sysex;
        Mf_metamisc =  smf_metamisc;
        Mf_seqnum =  smf_metaseq;
        Mf_eot =  smf_metaeot;
        Mf_timesig =  smf_timesig;
        Mf_smpte =  smf_smpte;
        Mf_tempo =  smf_tempo;
        Mf_keysig =  smf_keysig;
        Mf_sqspecific =  smf_sqspecific;
        Mf_text =  smf_metatext;
        Mf_arbitrary =  smf_arbitrary;
}
