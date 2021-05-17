/* created by DMH (damonhorowitz): write seq_type as standard midifile */
/***************************************************************************
*      Change Log
*  Date     | Change
*-----------+---------------------------------------------------------------
* 11-Mar-94 | Created Change Log
* 11-Mar-94 | PLu : Added private to function defs.
* 28-Apr-03 |  DM : Change #include's for portability
****************************************************************************/

#include "switches.h"

#include <stdio.h>

#include "cext.h"
#include "userio.h"
#include "midicode.h"
#include "mfmidi.h"
#include "midifns.h"
#include "timebase.h"
#include "moxc.h"
#include "seq.h"
#include "seqread.h" /* to get scale() */
#include "seqmwrite.h"

long chunk_size_marker;
int seti_counter;
extern time_type a_start_time;

long last_event;    /* time from last_clock_event to the last event */
time_type last_clock_event;
time_type last_tick_size;       /* millisec per tick shifted 16 bits */

struct smf_write_seq {
    seq_type seq;
    int track;
    FILE *outfile;
    } smfw_seq;
extern seq_type sequence; /* this is a global to be accessed by 
               * routines called from the sequence */
/* clock state: */
extern time_type clock_ticksize;  /* millisec per tick shifted 16 bits */
extern boolean clock_running;     /* TRUE if clock is running */
extern boolean use_midi_clock;

private void smfw_bend(seq_type seq, int voice, int value);
private void smfw_cause_noteoff(seq_type seq, time_type delay, int voice, 
                                int pitch);
private void smfw_ctrl(seq_type seq, int voice, int ctrl_name, int value);
private void smfw_deltatime(void);
private void smfw_dotrack(seq_type seq);
private void smfw_exclusive(int length, unsigned char *msg);
private void smfw_noteoff(call_args_type args);
private void smfw_noteon(seq_type seq, int voice, int pitch, int vel);
private void smfw_process_event(call_args_type args);
private void smfw_ramp_event(call_args_type args);
private void smfw_send_macro(unsigned char *ptr, int voice, short parameter[],
                             int parm_num, int value);
private void smfw_touch(seq_type seq, int voice, int value);
private void writevarlen(long value);


/* smfw_bend -- write a pitch bend to a midi file */
/**/
private void smfw_bend(seq_type seq, int voice, int value)
{
    if (debug)  gprintf(TRANS, "smfw_bend %d\n", value);
    smfw_deltatime();
    putc(MIDI_BEND | (voice - 1), smfw_seq.outfile);
    putc(0xFF & ((value & 0x1) << 6) , smfw_seq.outfile);
    putc(0xFF & (value >> 1), smfw_seq.outfile);
}


/* smfw_cause_noteoff -- schedule a noteoff for midi file */
/*
 * NOTE: this is called by smfw_process_event when it handles a note
 * event node in a seq_type's event list.  The ordinary moxc scheduler
 * is used to "schedule" the noteoff in the future.  In reality, the
 * output is done as fast as possible (by attempting an infinite rate),
 * so no real timing delays occur.  The effect is to sort events by their
 * specified time.
 */
private void smfw_cause_noteoff(seq_type seq, time_type delay, int voice, 
                                int pitch)
{
    call_args_node args;
    if (debug) gprintf(TRANS, "cause noteoff at %ld...", virttime + delay);
    pitch += seq->transpose;
    while (pitch <= 0) pitch += 12;
    while (pitch >= 127) pitch -= 12;
    seq->noteoff_count++;
    args.arg[0] = seq;
    args.arg[1] = (void *)(size_t)voice;
    args.arg[2] = (void *)(size_t)pitch;
    causepri((delay_type) delay, 10, seq->noteoff_fn, &args);
}

private void smfw_clock_event(call_args_type args)
{
    time_type old_ticksize = (time_type) ((intptr_t) args->arg[0]);
    time_type new_ticksize = (time_type)((intptr_t) args->arg[1]);
    time_type temp_ticksize = new_ticksize;
    new_ticksize = scale(new_ticksize, 375L, 1024L);
/* (new_ticksize >> 16) * 24000 ms/clock becomes us/midiquarter */

    if (debug) gprintf(TRANS, 
        "smfw_clock: write %ld (time:%ld) ->->->tempo %ld\n", 
        new_ticksize, virttime, 2500L / (new_ticksize / 24000));

    /* use old ticksize to write the delta for the clock event */
    last_tick_size = old_ticksize;
    smfw_deltatime();
    last_tick_size = temp_ticksize;/* reset to = new_tick_size */
       putc(0xFF, smfw_seq.outfile);
    putc(0x51, smfw_seq.outfile);
    putc(0x03, smfw_seq.outfile);
    putc((int) ((new_ticksize >> 16) & 0xFF), smfw_seq.outfile);
    putc((int) ((new_ticksize >> 8) & 0xFF), smfw_seq.outfile);
    putc((int) (new_ticksize & 0xFF), smfw_seq.outfile);

    last_clock_event = virttime;
    last_event = 0L;
    /* no time expired between last clockevent and last event 
     * (they are the same). Next clock event will be exactly the 
     * next this_event from last_clock_event
     */
}


/* smfw_ctrl -- write a control change to a midi file */
/**/
private void smfw_ctrl(seq_type seq, int voice, int ctrl_name, int value)
{
    if (debug)  gprintf(TRANS, "smfw_ctrl %d: %d\n", ctrl_name, value); 
    smfw_deltatime();
    putc(MIDI_CTRL | (voice - 1), smfw_seq.outfile);
    putc(ctrl_name, smfw_seq.outfile);
    putc(value, smfw_seq.outfile);
}


/* smfw_deltatime -- write the time difference between now an previous event */
/**/
private void smfw_deltatime()
{
    /* if last_ and clock_ are different, use last_ for clock deltatime*/
    time_type use_ticksize = (clock_ticksize != last_tick_size) ?
                last_tick_size : clock_ticksize;
    time_type this_event = virttime - last_clock_event;
    if (debug) gprintf(TRANS, 
        "delta! ticksize: %lu Lastev: %ld ThisevScaled: %lu Thisev: %lu ", 
        clock_ticksize, last_event,  
        (this_event * ((2500L << 16) / use_ticksize)) / 100,
        this_event);
    
    this_event = ((virttime - last_clock_event) * 
                      ((2500L << 16) / use_ticksize)) / 100;

    if (debug) gprintf(TRANS, "--- deltatime: %lu\n", this_event - last_event);
    writevarlen((long) (this_event - last_event));

    last_event = this_event; 
}


/* smfw_dotrack -- write the remainder of a track  */
private void smfw_dotrack(seq_type seq)
{
    long end_marker;
    timebase_type old_timebase = timebase;
    unsigned long chunk_size;

    if (seq->runflag) {
        seq_stop(seq);
    }
    timebase_use(seq->timebase);
    set_rate(seq->timebase, STOPRATE);
    set_virttime(seq->timebase, 0L);
    seq->current = seq_events(seq);
    seq->noteoff_count = 0L;
    seq->runflag = TRUE;
    seq->paused = TRUE;
    last_clock_event = 0L;
    last_event = 0L;
    if (debug)  gprintf(TRANS, "dotrack (reset) %d %ld (%lu) \n", 
        smfw_seq.track, last_event, virttime);

    if (seq->current) {
        call_args_node args;
        args.arg[0] = seq;
        cause((delay_type)(seq->current->ntime - virttime), smfw_process_event,
              &args);
    }
    set_virttime(timebase, MAXTIME);
    catchup();

    putc(0x00, smfw_seq.outfile);
    putc(0xFF, smfw_seq.outfile); /* end of track chunk */
    putc(0x2F, smfw_seq.outfile);
    putc(0x00, smfw_seq.outfile);
    end_marker = ftell(smfw_seq.outfile);
    /* go back to enter chunksize: */
    fseek(smfw_seq.outfile, chunk_size_marker, 0);
    /* - 4 for 4 size bytes: */ 
    chunk_size = (end_marker - chunk_size_marker) - 4;
    if (debug) gprintf(TRANS, "bytes written in previous track: %ld \n\n", 
                              chunk_size);
    putc((int) ((0xFF & (chunk_size >> 24))), smfw_seq.outfile);
    putc((int) ((0xFF & (chunk_size >> 16))), smfw_seq.outfile);
    putc((int) ((0xFF & (chunk_size >> 8))), smfw_seq.outfile);
    putc((int) ((0xFF & chunk_size)), smfw_seq.outfile);
    /* return file pointer to end of track */
    fseek(smfw_seq.outfile, end_marker, 0);
    timebase_use(old_timebase);
}


/* smfw_exclusive -- write a system excl. msg to midi file */
private void smfw_exclusive(int length, unsigned char *msg)
{
    int length_count = 0;

    if (debug)  gprintf(TRANS, "SYSEX (time:%ld)\n", virttime);

    smfw_deltatime();
   
    while (length > length_count){ /* *(msg-1) != MIDI_EOX) { */
        putc(*msg++, smfw_seq.outfile); 
        length_count++;
    }
    if (*(--msg) != MIDI_EOX) gprintf(TRANS, "ERROR: no end of sysex\n");
}

private void smfw_msg_write(n,c1,c2,c3)
  int n;
  unsigned char c1,c2,c3;
{
    if (debug) gprintf(TRANS, "MSGWRITE %d bytes (time:%ld)\n", n, virttime);
    smfw_deltatime();
    switch(n) {
    case 1: putc(c1, smfw_seq.outfile);
        break;
    case 2: putc(c1, smfw_seq.outfile);
        putc(c2, smfw_seq.outfile);
        break;
    case 3: putc(c1, smfw_seq.outfile);
        putc(c2, smfw_seq.outfile);
        putc(c3, smfw_seq.outfile);
        break;
    }
}


/* smfw_noteoff -- write noteoff to midi file */
/**/
private void smfw_noteoff(call_args_type args)
{
    /* seq_type seq = args->arg[0]; -- unused */
    int voice = (int)(size_t)args->arg[1];
    int pitch = (int)(size_t)args->arg[2];
    if (debug) gprintf(TRANS, 
        "smfw_noteoff %d: %d (time:%ld)\n", voice, pitch, virttime);
    smfw_deltatime();
    putc(NOTEOFF | (voice - 1), smfw_seq.outfile);
    putc(pitch, smfw_seq.outfile);
    putc(0x40, smfw_seq.outfile);
} 


/* smfw_noteon -- write noteon to midi file */
/*
 * NOTE: the seq parameter is not used here, but is passed in by the
 * seq_noteon macro, so we have to have a placeholder for it.
 */
private void smfw_noteon(seq_type seq, int voice, int pitch, int vel)
{
    if (debug) gprintf(TRANS, 
        "smfw_noteon %d: %d %d(time:%ld)\n", voice, pitch, vel, virttime);
    smfw_deltatime();
    putc(NOTEON | (voice - 1), smfw_seq.outfile);
    putc(pitch, smfw_seq.outfile);
    putc(vel, smfw_seq.outfile);
}


/* smfw_process_event -- write a seq event to a midi file */
/**/
private void smfw_process_event(call_args_type args)
{
    seq_type seq = args->arg[0];
    register event_type event;
    if (!seq->runflag) return;
    while ((event = seq->current) && (event->ntime <= virttime)) {
        unsigned int voice;
        if ((vc_voice(event->nvoice) == smfw_seq.track) ||
            /* if on current track */
            (((vc_voice(event->nvoice) - 16) == smfw_seq.track)
             && (smfw_seq.track > 0)) ||
            /* acknowledge clock change on all tracks*/
            (event->value == CLOCK_VALUE && 
             vc_ctrl(event->nvoice) == ESC_CTRL)) {

            /* process all current (and earlier) events */
            if (is_note(event)) {   /*** play a note or rest ***/
                /* if this note is not a rest, play it and schedule an 
                 * off event */
                if (event->value != NO_PITCH &&
                    (seq_channel_mask(seq) &
                     (1 << ((voice = vc_voice(event->nvoice)) - 1)))) {
                    seq_noteon(seq, voice, (0xFF & event->value), 
                               (int) (event->u.note.ndur & 0xFF));
        
                    seq_cause_noteoff(seq, (event->u.note.ndur) >> 8, 
                                      voice, (0xFF & event->value)); 
                }
            } else {                    /*** send a control command ***/
                int n;
                time_type step;
                int delta;
                int increment;
                int voice = vc_voice(event->nvoice);
                ulong enabled = seq_channel_mask(seq) & (1 << (voice - 1));

                switch (vc_ctrl(event->nvoice)) {
                  case PSWITCH_CTRL:
                    if (!enabled) break;
                    if (debug) gprintf(TRANS, 
                        "porta %d (time:%ld)... ", event->value, virttime);
                    seq_midi_ctrl(seq, voice, PORTASWITCH, 0xFF & event->value);
                    break;
                  case MODWHEEL_CTRL:
                    if (!enabled) break;
                    if (debug) gprintf(TRANS, 
                        "modw %d (time:%ld)...", event->value, virttime);
                    seq_midi_ctrl(seq, voice, MODWHEEL, 0xFF & event->value);
                    break;
                  case TOUCH_CTRL:
                    if (!enabled) break;
                    if (debug) gprintf(TRANS, 
                        "touch %d (time:%ld)... ", event->value, virttime);
                    seq_midi_touch(seq, voice, 0xFF & event->value);
                    break;
                  case VOLUME_CTRL:
                    if (!enabled) break;
                    if (debug) gprintf(TRANS, 
                        "ftvol %d (time:%ld)...", event->value, virttime);
                    seq_midi_ctrl(seq, voice, VOLUME, 0xFF & event->value);
                    break;
                  case BEND_CTRL:
                    if (!enabled) break;
                    if (debug) gprintf(TRANS, 
                        "bend %d (time:%ld)... ", event->value, virttime);
                    seq_midi_bend(seq, voice, event->value);
                    break;
                  case PROGRAM_CTRL:
                    if (!enabled) break;
                    if (debug) gprintf(TRANS, 
                        "prog %d (time:%ld)\n", event->value, virttime);
                    smfw_deltatime();
                    putc(MIDI_CH_PROGRAM | (voice - 1), smfw_seq.outfile);
                    putc(0xFF & event->value, smfw_seq.outfile);
                    break;
                  case ESC_CTRL:
                    switch (event->value) {
                            
                      /* called routine will write to midifile in execution: */
                      case CALL_VALUE: 
                        sequence = seq;
                        (*(event->u.call.routine))(event->u.call.args);
                        break;
                      case CLOCK_VALUE:
                        clock_ticksize = event->u.clock.ticksize;
                        if (debug) gprintf(TRANS, 
                            "clockevent! ticksize: %lu (time:%ld)\n",
                            clock_ticksize, virttime);

                        if (virttime > 0) { /* any clock before this is 
                                             * already recorded in the header */
                            if (smfw_seq.track == 0) {  
                                /* record clock event on tempo track = 0 */
                                /* cause clock write in half a newtick, 
                                 * because it was written .5 tick early */
                                call_args_node args;
                                args.arg[0] = (void *)(size_t)last_tick_size;
                                args.arg[1] = (void *)(size_t)clock_ticksize;
                                cause((delay_type) (clock_ticksize >> 17), 
                                      smfw_clock_event, &args);
                                /* set new ticksize: */
                                last_tick_size = clock_ticksize; 
                            } else { /*not on tempo track*/
                                long this_event = ((virttime - last_clock_event) *
                                        ((2500L << 16) / last_tick_size)) / 100;
                                if (debug) gprintf(TRANS,
                                    "track != 0: Lastev: %ld Thisev: %ld NewLast: %ld\n",
                                    last_event, this_event, 
                                    this_event - last_event);
                                last_event = 0L - (this_event - last_event);
                                last_clock_event = virttime;
                                /* last_event is negative, so will be ADDED 
                                 * to next this_event */
                                last_tick_size = clock_ticksize;
                            }               
                        } else /* if virttime <= 0 */
                            if (debug) gprintf(TRANS, "IGNORED\n");
                        break;
                      case MACCTRL_VALUE:
                        if (!enabled) break;
                        if (debug) gprintf(TRANS, 
                            "MACCTRL %d: %d (time:%ld)\n", 
                            event->u.macctrl.ctrl_number, 
                            event->u.macctrl.value, virttime);
                        smfw_deltatime();
                        putc(MIDI_CTRL | (voice - 1), smfw_seq.outfile);
                        putc(0xFF & event->u.macctrl.ctrl_number, 
                             smfw_seq.outfile);
                        putc(0xFF & event->u.macctrl.value, smfw_seq.outfile);
                        break;
                      case MACRO_VALUE:
                        if (!enabled) break;
                        if (debug) gprintf(TRANS, "MACRO sent to...\n");
                        smfw_send_macro(event->u.macro.definition,
                                        voice, event->u.macro.parameter, -1, 0);
                        break;
                      case CTRLRAMP_VALUE:
                      case DEFRAMP_VALUE: {
                        int from, to;
                        call_args_node re_args;
                        if (!enabled) break;
                        step = event->u.ramp.step;
                        if (event->value == CTRLRAMP_VALUE) {
                            if (debug) gprintf(TRANS, 
                                "CTRLRAMP (time:%ld)...", virttime);
                            from = event->u.ramp.u.ctrl.from_value;
                            to = event->u.ramp.u.ctrl.to_value;
                        } else {
                            if (debug) gprintf(TRANS, 
                                "DEFRAMP (time:%ld)...", virttime);
                            from = event->u.ramp.u.def.parameter[
                                event->u.ramp.u.def.parm_num];
                            to = event->u.ramp.u.def.to_value;
                        }
                        delta = to - from;
                        increment = delta;
                        if (delta < 0) delta = -delta;
                        /* RBD - Note: Step is always non-zero */
                        n = event->u.ramp.dur / step;
                        increment = (increment << 8) / n;
                        re_args.arg[0] = seq;
                        re_args.arg[1] = event;
                        re_args.arg[2] = (void *)(size_t)(from << 8);
                        re_args.arg[3] = (void *)(size_t)(to << 8);
                        re_args.arg[4] = (void *)(size_t)increment;
                        re_args.arg[5] = (void *)(size_t)step;
                        re_args.arg[6] = (void *)(size_t)n;
                        smfw_ramp_event(&re_args);
                        seq->noteoff_count++;
                        break;
                      }
                      case SETI_VALUE:
                        /* will be printed after writing is completed: */
                        seti_counter++; 
                        *(event->u.seti.int_to_set) = event->u.seti.value;
                        break;
                      default:
                        gprintf(TRANS, "unexpected ESC_CTRL value\n");
                        break;
                    }
                    break;
                  default:
                    gprintf(TRANS, "unexpected seq data\n");
                    break;
                }
            }
        }
        seq->current = event->next;
    }
    if (seq->current) { /* if there is an event: delay, then process again */
        cause((delay_type)(event->ntime - virttime), smfw_process_event, args);
    }
}

/* smfw_ramp_event -- generate a ramp to write */
private void smfw_ramp_event(call_args_type args)
{
    seq_type seq = (seq_type) args->arg[0];
    event_type event = (event_type) args->arg[1];
    unsigned int value = (unsigned int)(size_t)args->arg[2];
    unsigned int to_value = (unsigned int)(size_t)args->arg[3];
    int increment = (int)(size_t)args->arg[4];
    time_type step = (time_type)(size_t)args->arg[5];
    int n = (int)(size_t)args->arg[6];
    
    if (debug) gprintf(TRANS, "ramp of %d: %d to %d\n", event->u.ramp.ctrl, 
                              value >> 8, to_value >> 8);     
    if (seq->runflag) {
    int voice = vc_voice(event->nvoice);
    if (n == 0) value = to_value;
    else {
        args->arg[2] = (void *)(size_t)(value + increment); /* update value */
        args->arg[6] = (void *)(size_t)(n - 1);             /* update n */
        cause((delay_type)step, smfw_ramp_event, args);
    }
    if (event->value == CTRLRAMP_VALUE) {
        int ctrl = event->u.ramp.ctrl;
        if (ctrl == -TOUCH_CTRL) smfw_touch(seq, voice, value >> 8);
        else if (ctrl == -BEND_CTRL) smfw_bend(seq, voice, value >> 8);
        else smfw_ctrl(seq, voice, ctrl, value >> 8);
    } else { /* must be DEFRAMP_VALUE */
        smfw_send_macro(event->u.ramp.u.def.definition,
        vc_voice(event->nvoice),
        event->u.ramp.u.def.parameter,
        event->u.ramp.u.def.parm_num, value >> 8);
    }
    /* really passing seq, but it's already in args: */
    if (n == 0) seq_end_event(args); 
    }
}


/* smfw_send_macro -- write msg to midi file from a seq "macro" event */
/**/
private void smfw_send_macro(unsigned char *ptr, int voice, short parameter[],
                             int parm_num, int value)
{
    unsigned char code, *loc;
    while ((code = *ptr++)) {
        loc = ptr + *ptr;
        ptr++;
        if (code <= nmacroparms) {
            code--;
            *loc = (code == parm_num ? value : parameter[code]) & 0x7f;
        }
        else if (code == nmacroparms + 1) {
            *loc = ((voice - 1) & 0xF) | *loc;
        }
        else {
            code -= (nmacroparms + 2);
            *loc = ((code == parm_num ? value : parameter[code]) >> 7) & 0x7F;
        }
    }
    if (ptr[1] == MIDI_SYSEX)
        smfw_exclusive(*ptr, ptr + 1);
    else
        smfw_msg_write(*ptr, ptr[1], ptr[2], ptr[3]);
}


/* smfw_touch -- write aftertouch msg to midi file */
/**/
private void smfw_touch(seq_type seq, int voice, int value)
{
    if (debug)  gprintf(TRANS, "smfw_touch %d\n", value);
    smfw_deltatime();
    putc(MIDI_TOUCH | (voice - 1), smfw_seq.outfile);
    putc(value, smfw_seq.outfile);
}


void seq_write_smf(seq, outfile)
  seq_type seq;
  FILE *outfile;
{
    time_type put_tick_size;
    int i;
    /* ticksize is milliseconds << 16, and tickrate = 24*tempo
       60000ms/min / (24 * tempo) = 2500/tempo = 25
       25 << 16 = 1638400 */
    time_type starting_ticksize = 1638400L; /*default midifile tempo 100*/
    int track_count = 0;
    long track_count_marker;
    register event_type event;

    seti_counter = 0;
    
    /*initialize the smfw_seq struct*/
    smfw_seq.outfile = outfile;
    smfw_seq.seq = seq_copy(seq);

    smfw_seq.seq->cause_noteoff_fn = smfw_cause_noteoff;
    smfw_seq.seq->midi_bend_fn    = smfw_bend;
    smfw_seq.seq->midi_ctrl_fn    = smfw_ctrl;
    smfw_seq.seq->midi_touch_fn   = smfw_touch;
    smfw_seq.seq->noteoff_fn      = smfw_noteoff;
    smfw_seq.seq->noteon_fn       = smfw_noteon;

    event = seq_events(smfw_seq.seq);

    /* search for clock events up till start of score */
    /* careful: there may be no events at all */
    while(event && event->ntime <= 0){
        if (debug)  gprintf(TRANS, "event (time:%ld)\n", event->ntime); 
        if (vc_ctrl(event->nvoice) == ESC_CTRL && event->value == CLOCK_VALUE) {
            if (debug) gprintf(TRANS, 
                               "clock %lu at 0\n", event->u.clock.ticksize);
            starting_ticksize = event->u.clock.ticksize;
            break;
        }
        event = event->next;
    }

    putc(0x4D, smfw_seq.outfile); /*header:  MThd*/
    putc(0x54, smfw_seq.outfile);
    putc(0x68, smfw_seq.outfile);
    putc(0x64, smfw_seq.outfile);
    putc(0x00, smfw_seq.outfile);
    putc(0x00, smfw_seq.outfile);
    putc(0x00, smfw_seq.outfile);
    putc(0x06, smfw_seq.outfile);
    putc(0x00, smfw_seq.outfile);
    putc(0x01, smfw_seq.outfile);       /*format 1 */
    putc(0x00, smfw_seq.outfile);

    /* number of tracks will be written later: */
    track_count_marker = ftell(smfw_seq.outfile);
    putc(0x00, smfw_seq.outfile); /* will be filled by track_count_marker */
    putc(0x02, smfw_seq.outfile); /* division resolution of 600 */
    putc(0x58, smfw_seq.outfile);

    for (i = 0; i < 17; i++) { /* for each track... */
        if (((seq_used_mask(smfw_seq.seq) >> (i - 1)) & 0x1) || (i == 0)) {
            if (debug) gprintf(TRANS, "write track %d \n", i);
            track_count++;
            clock_ticksize = starting_ticksize;
            last_tick_size = starting_ticksize;
            putc(0x4D, smfw_seq.outfile);/*track header: MTrk*/
            putc(0x54, smfw_seq.outfile);
            putc(0x72, smfw_seq.outfile);
            putc(0x6B, smfw_seq.outfile);
       
            chunk_size_marker = ftell(smfw_seq.outfile); 
            /* size of chunk will be written later */
            /* will be filled by chunk_size_marker */
            putc(0x00, smfw_seq.outfile);
            putc(0x00, smfw_seq.outfile);
            putc(0x00, smfw_seq.outfile);
            putc(0x00, smfw_seq.outfile);

            if (i == 0) { /* tempo and time signature track */
                putc(0x00, smfw_seq.outfile);/* default time sig stuff*/
                putc(0xFF, smfw_seq.outfile);
                putc(0x58, smfw_seq.outfile);
                putc(0x04, smfw_seq.outfile);
                putc(0x04, smfw_seq.outfile);
                putc(0x02, smfw_seq.outfile);
                putc(0x18, smfw_seq.outfile);
                putc(0x08, smfw_seq.outfile);
                putc(0x00, smfw_seq.outfile);
            
                /* TEMPO: inserted here in case default is used */
                putc(0xFF, smfw_seq.outfile);
                putc(0x51, smfw_seq.outfile);
                putc(0x03, smfw_seq.outfile);
                /* ticksize is in ms<<16, so to get milliseconds per tick, it's
                   ticksize / 65536. To get beat durations, multiply by 24 
                   to get ticksize * 24 / 65536. To get microseconds, 
                   multiply by 1000: ticksize * 24000 / 65536. Divide both
                   constants by 64 to get ticksize * 375 / 1024 = 
                   microseconds per quarter note.
                */
                put_tick_size = scale(clock_ticksize, 375L, 1024L);
                putc((int) ((put_tick_size >> 16) & 0xFF), smfw_seq.outfile);
                putc((int) ((put_tick_size >> 8) & 0xFF), smfw_seq.outfile);
                putc((int) (put_tick_size & 0xFF), smfw_seq.outfile);
            }
            smfw_seq.track = i;
            smfw_dotrack(smfw_seq.seq);
        }
    }
    if (seti_counter) 
        gprintf(TRANS, "%d SETI events IGNORED!\n", seti_counter);
    seq_stop(smfw_seq.seq);
    /* go back and insert number of tracks */
    fseek(smfw_seq.outfile, track_count_marker, 0);
    putc(0xFF & track_count, smfw_seq.outfile);
    fclose(smfw_seq.outfile);
}


/* writevarlen -- write a variable length integer to midi file */
/**/
private void writevarlen(long value)
{
    register ulong buffer;

    if (debug) gprintf(TRANS, "variable length quantity...");

    buffer = value & 0x7f;

    while((value >>= 7) > 0) {
    buffer <<= 8;
    buffer |= 0x80;
    buffer += (value & 0x7f);
    }

    for(;;) {
    if (debug) gprintf(TRANS, " byte ");
    putc((int) (buffer & 0xFF), smfw_seq.outfile);
    if (buffer & 0x80)  buffer >>= 8;
    else break;
    }
    if (debug) gprintf(TRANS, "written!\n");
}
