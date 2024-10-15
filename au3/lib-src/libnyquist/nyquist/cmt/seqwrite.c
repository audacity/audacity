/* seqwrite -- write a seq in Adagio format to a file */
/***************************************************************************
*       Change Log
*  Date | Change
*-----------+--------------------------------------------------------------
* 11-Mar-94 | Created Change Log
* 11-Mar-94 | PLu : Added private to function defs.
* 28-Apr-03 |  DM : changed for portability, true->TRUE, false->FALSE
****************************************************************************/

#include "switches.h"

#include <stdio.h>

#include "cext.h"
#include "midifns.h"
#include "timebase.h"
#include "seq.h"
#include "seqwrite.h"
#include "userio.h"
#include "record.h"

private boolean next_event_time(event_type event, time_type *next_time);
private void write_event(seq_type seq, event_type event, FILE *f,
                         boolean abs_flag);
private void write_velocity(FILE *f, int velocity);
private void write_voice(FILE *f, int voice);
private void write_rest(FILE *f, event_type ev, boolean abs_flag);
private void write_time(FILE *f, event_type event, boolean abs_flag);

private boolean clock_started;
private long clock_half_tick;
private int new_tempo;
/* non-zero indicates a pending tempo change... */
private time_type tempo_change_at;

private int macro_count;
private int call_count;
private int deframp_count;
private int seti_count;

private int last_velocity;
private int last_voice = seq_dflt_voice;


/* next_event_time -- get the time of the next event */
/*
 * NOTE: clock events are ignored (unless this is the first clock event)
 */
private boolean next_event_time(event_type event, time_type *next_time)
{
    while (event) {
        if (vc_ctrl(event->nvoice) == ESC_CTRL &&
            event->value == CLOCK_VALUE && 
            clock_started) {
            /* this is a clock event, ignore it: */
            event = event->next;
        } else {
            *next_time = event->ntime;
            return TRUE;
        }
    }
    return FALSE;
}


/* seq_write -- write a seq to a file */
/*
 * NOTE: if abs_flag, then write using absolute times ('T'), otherwise use
 *  relative timing ('N').  Also, only selected channels are written.
 */
void seq_write(seq_type seq, FILE *f, boolean abs_flag)
{
    event_type ev = seq_events(seq);

    last_velocity = seq_dflt_loud;
    last_voice = seq_dflt_voice;

    fprintf(f, "!MSEC\n");
    clock_started = FALSE;
    tempo_change_at = 0;

    macro_count = 0;
    call_count = 0;
    deframp_count = 0;
    seti_count = 0;

    while (ev) {
        write_event(seq, ev, f, abs_flag);
        ev = ev->next;
    }
    if (macro_count) gprintf(TRANS, "%d macros ignored.\n", macro_count);
    if (call_count) gprintf(TRANS, "%d calls ignored.\n", call_count);
    if (deframp_count) gprintf(TRANS, "%d deframps ignored.\n", deframp_count);
    if (seti_count) gprintf(TRANS, "%d setis ignored.\n", seti_count);
}


private char ctrl_letter[] = "?KMOXYZ";

/* write_event -- write a single event to a file */
/**/
private void write_event(seq_type seq, event_type event, FILE *f,
                         boolean abs_flag)
{
    int voice = vc_voice(event->nvoice);

    /* process all current (and earlier) events */
    if (is_note(event)) {       /*** a note or rest ***/
        /* if this note is not a rest, write it */
        if (event->value != NO_PITCH &&
                  (seq_channel_mask(seq) &
                   (1 << (voice - 1)))) {
            write_pitch(f, event->value);
            fprintf(f, " U%ld ", event->u.note.ndur >> 8);
            write_velocity(f, (int) (event->u.note.ndur & 0xFF));
            write_voice(f, voice);
            write_time(f, event, abs_flag);
        }
    } else {            /*** a control command ***/
        switch (vc_ctrl(event->nvoice)) {
          case PSWITCH_CTRL:
          case MODWHEEL_CTRL:
          case TOUCH_CTRL:
          case VOLUME_CTRL:
          case BEND_CTRL:
            fprintf(f, "%c%d ", ctrl_letter[vc_ctrl(event->nvoice)],
                                    event->value);
            write_voice(f, voice);
            write_time(f, event, abs_flag);
            break;
          case PROGRAM_CTRL:
            fprintf(f, "Z%d ", event->value + 1);
            write_voice(f, voice);
            write_time(f, event, abs_flag);
            break;
          case ESC_CTRL:
            switch (event->value) {
              case CALL_VALUE:
                call_count++;
                write_rest(f, event, abs_flag);
                /* !call's are not written because there isn't enough
                 * information.  A future version of seq should store
                 * the hash table entry instead of the address of the
                 * routine (then we could get string name of the call)
                 * and the number of arguments.
                 */
                break;
              case CLOCK_VALUE:
                /* design for !clock: for absolute (absflag) files, put the
                   clocks (if any) at the end where they can be edited out
                   easily.  For relative files, keep the clocks in-line.
                   On each clock, write a !tempo command inferred by the 
                   clock and followed by the !clock.  Because the clock
                   event comes before the actual tempo change, the chnage
                   must be delayed by a half tick except for the first one.
                 */
                if (abs_flag) break;
                new_tempo = (2500L << 16) / event->u.clock.ticksize;
                if (clock_started) {
                    tempo_change_at = event->ntime + clock_half_tick;
                } else {
                    fprintf(f, "!tempo %d\n", new_tempo);
                    fprintf(f, "!clock\n");
                    clock_started = TRUE;
                }
                clock_half_tick = (event->u.clock.ticksize) >> 17;
                break;
              case MACCTRL_VALUE:
                fprintf(f, "~%d(%d) ", event->u.macctrl.ctrl_number,
                                       event->u.macctrl.value);
                write_voice(f, voice);
                write_time(f, event, abs_flag);
                break;
              case MACRO_VALUE: 
                macro_count++;
                write_rest(f, event, abs_flag);
                /* macros are not written because there isn't enough
                 * information.  A future version of seq should store
                 * the number of arguments in the event, or better yet,
                 * in the definition.  Send complaints to RBD!
                 */
                break;
              case CTRLRAMP_VALUE:
                fprintf(f, "!ramp ~%d(%d) ~%d(%d) U%d U%d ",
                        event->u.ramp.ctrl, event->u.ramp.u.ctrl.from_value,
                        event->u.ramp.ctrl, event->u.ramp.u.ctrl.to_value,
                        (int)event->u.ramp.step,
                        (int)event->u.ramp.dur);
                write_voice(f, voice);
                write_time(f, event, abs_flag);
                break;
              case DEFRAMP_VALUE:
                deframp_count++;
                write_rest(f, event, abs_flag);
                /* See MACRO_VALUE above for why this isn't implemented. */
                break;
              case SETI_VALUE:
                seti_count++;
                write_rest(f, event, abs_flag);
                /* !seti and !setv are not implemented -- a future version
                 * of seq should save more information so we can reconstruct
                 * the Adagio source command.
                 */
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


/* write_rest -- write a rest (in place of an event) */
/**/
private void write_rest(FILE *f, event_type ev, boolean abs_flag)
{
    fprintf(f, "R ");
    write_time(f, ev, abs_flag);
}


/* write_time -- write the final field on the line with N or T command */
/**/
private void write_time(FILE *f, event_type event, boolean abs_flag)
{
    time_type next_time;

    if (abs_flag) {
        fprintf(f, "T%ld\n", event->ntime);
        return;
    }

    if (next_event_time(event->next, &next_time)) {
        if (tempo_change_at && (next_time >= tempo_change_at)) {
            fprintf(f, "N%ld\n!TEMPO %d\n!CLOCK\nR U%ld\n", 
                        tempo_change_at - event->ntime,
                        new_tempo, next_time - tempo_change_at);
            tempo_change_at = 0;
        } else {
            fprintf(f, "N%ld\n", next_time - event->ntime);
        }
    } else {
        fprintf(f, "\n");
    }
}


/* write_velocity -- write the velocity field */
/**/
private void write_velocity(FILE *f, int velocity)
{
    if (last_velocity != velocity) {
        last_velocity = velocity;
        fprintf(f, "L%d ", velocity);
    }
}


/* write_voice -- write the voice field */
/**/
private void write_voice(FILE *f, int voice)
{
    if (last_voice != voice) {
        last_voice = voice;
        fprintf(f, "V%d ", voice);
    }
}
