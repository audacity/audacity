/* seq.c -- implement adagio scores as abstract data type */

/*****************************************************************************
*       Change Log
*  Date | Change
*-----------+-----------------------------------------------------------------
*  2-Apr-91 | JDW : further changes
* 16-Feb-92 | GWL : use reg_timebase in seq_play()
* 28-Apr-03 |  DM : false->FALSE, true->TRUE, portability changes
* 19-May-03 | RBD : no longer assume seq->current remains untouched between 
*           |       note inserts
*****************************************************************************/

#include "stdio.h"
#include "cext.h"
#include "userio.h"
#include "midicode.h"
#include "midifns.h"
#include "timebase.h"
#include "moxc.h"
#include "seq.h"
#include "string.h"

extern int moxcdebug;
extern timebase_type default_base;

boolean seq_print = FALSE;      /* debugging print switch */

seq_type sequence;      /* this is a global to be accessed by routines called
                         * from the sequence */

/* clock state: */
time_type clock_ticksize;       /* millisec per tick shifted 16 bits */
boolean clock_running = FALSE;  /* TRUE if clock is running */
boolean external_midi_clock = FALSE;
boolean suppress_midi_clock = FALSE;

private void insert_event(seq_type, register event_type);
private void process_event(call_args_type args);

private char *chunk_alloc(seq_type seq, int size);
private void clock_tick(call_args_type args);
private void ramp_event(call_args_type args);
/*private*/ void send_macro(register unsigned char *ptr, int voice,
    short parameter[], int parm_num, int value, int nline);

/* chunk_alloc -- allocate data for a sequence */
/*
 * NOTE: This assumes one chunk is already allocated.
 * The first chunk holds shared sequence information in 
 * the info struct, and by convention this is always in
 * the first chunk.
 */
private char *chunk_alloc(seq_type seq, int size)
{
    chunk_type chunk = seq->chunklist->u.info.last_chunk;
    /* gprintf(TRANS, "chunk_alloc: seq %lx size %d\n", seq, size); */
    if (size & 1) size++;	/* make it even */
    if (chunk->free + size >= CHUNK_SIZE) {
        chunk_type new_chunk = chunk_create(FALSE);
        if (!new_chunk) 
        {
            gprintf(FATAL, "Out of memory while reading seq\n");
            return NULL;
        }
        /* add new_chunk to chunk chain */
        seq->chunklist->u.info.last_chunk = new_chunk;
        chunk->next = new_chunk;
        chunk = new_chunk;
    }
    chunk->free += size;
    return &(chunk->u.data[chunk->free - size]);
}


/* chunk_create -- create a new chunk for seq data */
/*
 * If this is the first chunk, set first_flag to reserve
 * space for the info structure.
 */
chunk_type chunk_create(boolean first_flag)
{
    chunk_type result = (chunk_type) memget(sizeof(chunk_node));
    if (result) {
        result->next = NULL;
        result->u.info.refcount = 1;  /* pre-initialize for caller */
        result->free = 0;
        if (first_flag) {
            result->free = sizeof(struct info_struct);
            result->u.info.last_chunk = result;
            result->u.info.dictionary = NULL;
            result->u.info.eventlist = NULL; 
            result->u.info.ctrlcount = 0;
            result->u.info.notecount = 0;
            result->u.info.duration = 0;
            result->u.info.used_mask = 0;
        }
    }
    /* gprintf(TRANS, "chunk_create: got %lx (size %d)\n", */
                   /* result, sizeof(chunk_node)); */
    return result;
}

/* clock_tick -- advance the clock and send a tick */
/**/
private void clock_tick(call_args_type args)
{
    seq_type seq = (seq_type) args->arg[0];
	// double cast to avoid pointer-to-long truncation on WIN64
    time_type fraction = (time_type) ((intptr_t) args->arg[1]);
    int delay;
    fraction += clock_ticksize;
    delay = fraction >> 16;
    fraction &= 0xFFFF;
    if (seq->runflag && clock_ticksize && seq->note_enable) {
        midi_clock();
        args->arg[0] = seq;
        args->arg[1] = (void *)(size_t)fraction;
        cause((delay_type)delay, clock_tick, args);
    } else {
        clock_running = FALSE;
        midi_stop();
        midi_clock(); /* stop takes effect on next clock, so provide one */
    }
}

private void cycle(call_args_type args)
{
    seq_type seq = (seq_type) args->arg[0];
    seq_reset(seq);
    seq_play(seq);
}



/****************************************************************************
*               event_create
* Inputs:
*    seq_type seq: the seq to hold the event
*    int size: the size of the event in bytes
*    time_type etime: the time of the event
*    int eline: the line number of the event
* Returns:
*    event_type: a new event structure or
*               NULL if there is not enough memory left
* Effect:
*    allocates memory from the chunk, then heap as needed
* Implementation:
*    to reduce the per block storage overhead, we allocate memory in
*    large chunks and do our own allocation.  Allocate from first
*    chunk first.  If full, allocate a new chunk.
* WARNING: this implementation assumes that individual events are never freed!!
****************************************************************************/

private event_type event_create(seq, size, etime, eline)
  seq_type seq;
  int size;
  time_type etime;
  int eline;
{
    event_type result = (event_type) chunk_alloc(seq, size);
    if (result) {
        result->ntime = etime;
        result->nline = eline;
        /* since we know the time, we can insert now: */
        insert_event(seq, result);
        seq_duration(seq) = MAX(seq_duration(seq), etime);
    }
    return result;
}


/* insert_call -- add a call event to the seq */
/**/
event_type insert_call(seq_type seq, time_type ctime, int cline, int voice,
        int (*addr)(seq_arg_t args), long value[SEQ_MAX_PARMS], int n)
{
    int i;
    register event_type event = event_create(seq, callsize, ctime, cline);
    if (seq_print) {
        gprintf(TRANS, 
            "call(%lx): time %ld, line %d, voice %d, fn %lx,\n\tvalues:",
            event, ctime, cline, voice, addr);
        for (i = 0; i < n; i++) gprintf(TRANS, " %ld", value[i]);
        gprintf(TRANS, "\n");
    }
    if (event) {
        seq_used_mask(seq) |= 1 << (voice - 1);
        event->nvoice = ctrl_voice(ESC_CTRL, voice);
        event->value = CALL_VALUE;
        event->u.call.routine = addr;
        /* save the arguments */
        for (i = 0; i < n; i++) event->u.call.args.a[i] = value[i];
        seq_ctrlcount(seq)++;
    }
    return event;       
}

  
/* insert_clock -- add a clock cmd to the seq */
/**/
event_type insert_clock(seq, ctime, cline, ticksize)
  seq_type seq;
  time_type ctime;
  int cline;
  time_type ticksize;
{
    register event_type event = event_create(seq, clocksize, ctime, cline);

    if (seq_print) {
        gprintf(TRANS, "clock(%lx): time %ld, line %d\n", event, ctime, cline);
    }
    if (event) {
        event->nvoice = ctrl_voice(ESC_CTRL, 1);
        event->value = CLOCK_VALUE;
        event->u.clock.ticksize = ticksize;
        seq_ctrlcount(seq)++;
    }
    return event;
}


/* insert_ctrl -- add a control to the seq */
/**/
event_type insert_ctrl(seq_type seq, time_type ctime,
                       int cline, int ctrl, int voice, int value)
{
    register event_type event = event_create(seq, ctrlsize, ctime, cline);
    if (seq_print) {
        gprintf(TRANS,
            "ctrl(%lx): time %ld, line %d, ctrl %d, voice %d, value %d\n",
            event, ctime, cline, ctrl, voice, value);
    }
    if (event) {
        seq_used_mask(seq) |= 1 << (voice - 1);
        event->nvoice = ctrl_voice(ctrl, voice);
        event->value = value;
        seq_ctrlcount(seq)++;
    }
    return event;
}


/* insert_ctrlramp -- add a control ramp event to the seq */
/**/
event_type insert_ctrlramp(seq, rtime, rline, voice, step, dur, ctrl, v1, v2)
  seq_type seq;
  time_type rtime;
  int rline;
  int voice;
  time_type step;
  time_type dur;
  int ctrl;
  int v1, v2;
{
    register event_type event = event_create(seq, ctrlrampsize, rtime, rline);
    if (seq_print) {
        gprintf(TRANS, 
            "ctrlramp(%lx): time %ld, line %d, step %ld, dur %ld, ctrl %d, voice %d\n",
            event, rtime, rline, step, dur, ctrl, voice);
        gprintf(TRANS, "\tfrom %d to %d\n", v1, v2);
    }

    if (event) {
        seq_used_mask(seq) |= 1 << (voice - 1);
        event->nvoice = ctrl_voice(ESC_CTRL, voice);
        event->value = CTRLRAMP_VALUE;
        if (dur <= 0) dur = 1L; /* don't allow zero duration */
        event->u.ramp.dur = dur;
        event->u.ramp.ctrl = ctrl;
        if (step <= 0) step = 1; /* don't allow zero step size */
        event->u.ramp.step = (short) step;
        event->u.ramp.u.ctrl.from_value = v1;
        event->u.ramp.u.ctrl.to_value = v2;
        seq_ctrlcount(seq)++;
        seq_duration(seq) = MAX(seq_duration(seq), rtime + dur);
    }
    return event;
}


/* insert_def -- add a definition to the dictionary */
/**/
def_type insert_def(seq, symbol, definition, deflen)
  seq_type seq;
  char *symbol;
  unsigned char *definition;
  int deflen;
{
    int i;
    def_type defn = (def_type) chunk_alloc(seq, sizeof(def_node));
    defn->symbol = chunk_alloc(seq, (int) strlen(symbol) + 1);
    defn->definition = (unsigned char *) chunk_alloc(seq, deflen);
    strcpy(defn->symbol, symbol);
    for (i = 0; i < deflen; i++) {
        defn->definition[i] = definition[i];
    }
    defn->next = seq_dictionary(seq);
    seq_dictionary(seq) = defn;
    if (seq_print) {
        gprintf(TRANS, "def(%ld): symbol %s defn \n", defn, symbol);
        for (i = 0; i < deflen; i++) gprintf(TRANS, "%x", definition[i]);
        gprintf(TRANS, "\n");
    }
    return defn;
}


/* insert_deframp -- add a def ramp event to the seq */
/**/
event_type insert_deframp(seq, rtime, rline, voice, step, dur,
                          def, nparms, parms, parm_num, to_value)
  seq_type seq;
  time_type rtime;
  int rline;
  int voice;
  time_type step;
  time_type dur;
  def_type def;
  int nparms;           /* number of parameters for macro */
  short parms[];        /* actual parameter vector */
  int parm_num;         /* which of the actual parameters to ramp */
  int to_value;         /* final destination of ramp */
{
    register event_type event = event_create(seq, deframpsize, rtime, rline);
    if (seq_print) {
        int i;
        gprintf(TRANS, 
            "deframp(%ld): time %ld, line %d, voice %d, step %ld, dur %ld\n",
            event, rtime, rline, voice, step, dur);
        gprintf(TRANS, "def %ld, parms");
        for (i = 0; i < nparms; i++) gprintf(TRANS, " %d", parms[i]);
        gprintf(TRANS, "parm_num %d to %d\n", parm_num, to_value);
    }
    if (event) {
        int i;
        seq_used_mask(seq) |= 1 << (voice - 1);
        event->nvoice = ctrl_voice(ESC_CTRL, voice);
        event->value = DEFRAMP_VALUE;
        if (dur <= 0) dur = 1L; /* don't allow zero duration */
        event->u.ramp.dur = dur;
        event->u.ramp.ctrl = 0;
        if (step <= 0) step = 1; /* don't allow zero step size */
        event->u.ramp.step = (short) step;
        event->u.ramp.u.def.definition = def->definition;
         for (i = 0; i < nmacroparms; i++) {
            event->u.ramp.u.def.parameter[i] = (i < nparms ? parms[i] : 0);
        }
        event->u.ramp.u.def.parm_num = parm_num;
        event->u.ramp.u.def.to_value = to_value;
        seq_ctrlcount(seq)++;
        seq_duration(seq) = MAX(seq_duration(seq), rtime + dur);
    }
    return event;
}


/****************************************************************************
*               insert_event
* Inputs:
*    seq_type seq: where to put the event
*    event_type event: the event to insert
* Effect:
*    inserts event into the event list
*    NOTE: it is inserted *after* previously inserted events with the same time
* Implementation:
*    adagio files often contain many independent voices.  Although each voice
*    consists of events in sequence, the voices need not be inter-twined in
*    the input file.  Rather, all the events of voice 1 appear followed by all
*    the events of voice 2, and so forth.  As phase one merges these event
*    sequences, it must make many passes over an increasingly long list of
*    events: expensive if we always start from the beginning of the list!
*    we can exploit the fact that each voice is sequential by starting the
*    search for the proper point of insertion at the last event inserted.
*    the variable "last_event" is used to remember this hint.  We could
*    also snapshot "last_event" in "ref_event" when a !tempo or !rate
*    command occurs as another hint, but we don't.
****************************************************************************/

private void insert_event(seq, event)
  seq_type seq;
  register event_type event;
{
    event_type *evlptr = &(seq_eventlist(seq));
    if ((*evlptr == NULL) ||
        (event->ntime < (*evlptr)->ntime)) {
        /* insert at the head of the list */
        event->next = *evlptr;
        *evlptr = event;
        seq->current = event;
    } else {
        /* insert somewhere after the head of the list
         * do not assume: current is not NULL.  Although we always leave
		 * it set, the client may access the sequence before the next
		 * insert.
         */
        register event_type previous;
        register event_type insert_before;

		if (!seq->current) {
			seq->current = seq_eventlist(seq);
		}
        if (event->ntime >= seq->current->ntime) {
            /* insertion point is after current */
            previous = seq->current;
            insert_before = previous->next;
        } else {
            /* insertion point is before current; start at beginning */
            /* assume: not inserting at very head of list; that would
             * have been taken care of above */
            previous = seq_events(seq);
            insert_before = previous->next;
        }

        while ((insert_before != NULL) &&
            (event->ntime >= insert_before->ntime)) {
            previous = insert_before;
            insert_before = insert_before->next;
        }
        previous->next = event;
        event->next = insert_before;
        seq->current = event;
    }
}


/* insert_macctrl -- add a control to the seq */
/**/
event_type insert_macctrl(seq, ctime, cline, ctrl, voice, value)
  seq_type seq;
  time_type ctime;
  int cline;
  int ctrl;
  int voice;
  int value;
{
    register event_type event = event_create(seq, macctrlsize, ctime, cline);
    if (seq_print) {
        gprintf(TRANS, 
            "macctrl(%lx): time %ld, line %d, ctrl %d, voice %d, value %d\n",
            event, ctime, cline, ctrl, voice, value);
    }
    if (event) {
        seq_used_mask(seq) |= 1 << (voice - 1);
        event->nvoice = ctrl_voice(ESC_CTRL, voice);
        event->value = MACCTRL_VALUE;
        event->u.macctrl.ctrl_number = ctrl;
        event->u.macctrl.value = value;
        seq_ctrlcount(seq)++;
    }
    return event;
}


/* insert_macro -- insert a macro call seq */
/**/
event_type insert_macro(seq, ctime, cline, def, voice, nparms, parms)
  seq_type seq;
  time_type ctime;
  int cline;
  def_type def;
  int voice;
  int nparms;
  short *parms;
{
    register event_type event = event_create(seq, macrosize, ctime, cline);
    if (seq_print) {
        int i;
        gprintf(TRANS, 
            "macro(%lx): time %ld, line %d, def %ld, voice %d, parms",
            event, ctime, cline, def, voice);
        for (i = 0; i < nparms; i++) gprintf(TRANS, " %d", parms[i]);
        gprintf(TRANS, "\n");
    }
    if (event) {
        seq_used_mask(seq) |= 1 << (voice - 1);
        event->nvoice = ctrl_voice(ESC_CTRL, voice);
        event->value = MACRO_VALUE;
        event->u.macro.definition = def->definition;
        while (nparms-- > 0) {
            event->u.macro.parameter[nparms] = parms[nparms];
        }
        seq_ctrlcount(seq)++;
    }
    return event;
}


/* insert_note -- add a note to the seq */
/**/
event_type insert_note(seq, ntime, nline, voice, pitch, dur, loud)
  seq_type seq;
  time_type ntime;
  int nline;
  int voice;
  int pitch;
  time_type dur;
  int loud;
{
    register event_type event = event_create(seq, notesize, ntime, nline);

    if (seq_print) {
        gprintf(TRANS,
  "note(%lx): time %ld, line %d, dur %ld, pitch %d, voice %d, loudness %d\n",
                event, ntime, nline, dur, pitch, voice, loud);
    }
    
    if (event) {
        seq_used_mask(seq) |= 1 << (voice - 1);
        event->nvoice = voice - 1;
        event->value = pitch;
        event->u.note.ndur = (dur << 8) + loud;
        seq_notecount(seq)++;
        seq_duration(seq) = MAX(seq_duration(seq), ntime + dur);
    }
    return event;
}


/* insert_seti -- add a seti event to the seq */
/**/
event_type insert_seti(seq, stime, sline, voice, addr, value)
  seq_type seq;
  time_type stime;
  int sline;
  int voice;
  int *addr;
  int value;
{
    register event_type event = event_create(seq, setisize, stime, sline);
    if (seq_print) {
        gprintf(TRANS, 
            "seti(%ld): time %ld, line %d, voice %d, addr %ld, value %d\n",
            event, stime, sline, voice, addr, value);
    }
    if (event) {
        event->nvoice = ctrl_voice(ESC_CTRL, voice);
        event->value = SETI_VALUE;
        event->u.seti.int_to_set = addr;
        event->u.seti.value = value;
        seq_ctrlcount(seq)++;
    }
    return event;       
}


/* noop -- just returns, the default stopfunc for sequences */
/**/
void noop(seq_type seq) {}


private void process_event(call_args_type args)
{
    seq_type seq = (seq_type) args->arg[0];
    register event_type event;
    if (!seq->runflag) return;
    while ((event = seq->current) && (event->ntime <= virttime)) {
        int voice;
        /* process all current (and earlier) events */
        if (is_note(event)) {   /*** play a note or rest ***/
            /* if this note is not a rest, play it and schedule an off event */
            if (event->value != NO_PITCH &&
                  (seq_channel_mask(seq) &
                   (1 << ((voice = vc_voice(event->nvoice)) - 1)))) {
                seq_noteon(seq, voice, event->value,
                         (int) event->u.note.ndur & 0xFF);
                if (debug) {
                    gprintf(TRANS, "play pitch %d at %ld\n",
                                event->value, event->ntime);
                }
                seq_cause_noteoff(seq, (event->u.note.ndur) >> 8,
                    voice, event->value);

            }
        } else {                    /*** send a control command ***/
            int n;
            time_type step;
            int delta;
            long increment;
            int voice = vc_voice(event->nvoice);
            ulong enabled = seq_channel_mask(seq) & (1 << (voice - 1));

            switch (vc_ctrl(event->nvoice)) {
              case PSWITCH_CTRL:
                if (!enabled) break;
                seq_midi_ctrl(seq, voice, PORTASWITCH, event->value);
                break;
              case MODWHEEL_CTRL:
                if (!enabled) break;
                seq_midi_ctrl(seq, voice, MODWHEEL, event->value);
                break;
              case TOUCH_CTRL:
                if (!enabled) break;
                seq_midi_touch(seq, voice, event->value);
                break;
              case VOLUME_CTRL:
                if (!enabled) break;
                seq_midi_ctrl(seq, voice, VOLUME, event->value);
                break;
              case BEND_CTRL:
                if (!enabled) break;
                seq_midi_bend(seq, voice, (event->value << 6));
                break;
              case PROGRAM_CTRL:
                if (!enabled) break;
                seq_midi_program(seq, voice, event->value + 1);
                break;
              case ESC_CTRL:
                switch (event->value) {
                  case CALL_VALUE:
                    sequence = seq;
                    (*(event->u.call.routine))(event->u.call.args);
                    break;
                  case CLOCK_VALUE:
                    clock_ticksize = event->u.clock.ticksize;
                    if (!clock_running && !suppress_midi_clock && 
                        !external_midi_clock) {
                        call_args_node args;
                        clock_running = TRUE;
                        midi_start();
                        args.arg[0] = seq;
                        args.arg[1] = (void *)(size_t)0;
                        clock_tick(&args);
                    }
                    break;
                  case MACCTRL_VALUE:
                    if (!enabled) break;
                    seq_midi_ctrl(seq, voice, event->u.macctrl.ctrl_number,
                                              event->u.macctrl.value);
                    break;
                  case MACRO_VALUE: {
                    if (!enabled) break;
                    send_macro(event->u.macro.definition, voice,
                               event->u.macro.parameter, -1, 0,
                               event->nline);
                    break;
                  }
                  case CTRLRAMP_VALUE:
                  case DEFRAMP_VALUE: {
                    int from, to;
                    call_args_node re_args;
                    if (!enabled) break;

                    step = event->u.ramp.step;
                    if (event->value == CTRLRAMP_VALUE) {
                        from = event->u.ramp.u.ctrl.from_value;
                        to = event->u.ramp.u.ctrl.to_value;
                    } else {
                        from = event->u.ramp.u.def.parameter[
                                    event->u.ramp.u.def.parm_num];
                        to = event->u.ramp.u.def.to_value;
                    }
                    delta = to - from;
                    increment = delta;
                    if (delta < 0) delta = -delta;
                    /* Note: Step is always non-zero */
                    n = event->u.ramp.dur / step;
                    increment = (increment << 8) / n;
                    re_args.arg[0] = seq;
                    re_args.arg[1] = event;
                    re_args.arg[2] = (void *)(size_t)(from << 8);
                    re_args.arg[3] = (void *)(size_t)(to << 8);
                    re_args.arg[4] = (void *)(size_t)increment;
                    re_args.arg[5] = (void *)(size_t)step;
                    re_args.arg[6] = (void *)(size_t)n;
                    ramp_event(&re_args);
                    seq->noteoff_count++;
                    break;
                  }
                  case SETI_VALUE:
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
        seq->current = event->next;
    }
    if (seq->current) {
        cause((delay_type)(event->ntime - virttime), process_event, args);
    } else if (seq->noteoff_count == 0 && seq->note_enable) {
        /* if we're just advancing to a start point, note_enable will be
         * FALSE and this won't get called:
         */
        if (seq->stopfunc) {
            (*(seq->stopfunc))(seq);
        }
    }
}


/* ramp_event -- generate a ramp */
/**/
private void ramp_event(call_args_type args)
{
    seq_type seq = (seq_type) args->arg[0];
    event_type event = (event_type) args->arg[1];
    /* these 2 casts are ok because value (the starting point of the ramp)
     * and to_value (the ending point) are unsigned int's representing a 
     * fractional MIDI data value with an 8-bit fractional part. */
    unsigned int value = (unsigned int) ((size_t) args->arg[2]);
    unsigned int to_value = (unsigned int) ((size_t) args->arg[3]);
    /* increment is also a fixed-point fraction, so int is fine */
    int increment = (int) ((size_t) args->arg[4]);
	/* double cast to avoid pointer to long truncation warning in WIN64: */
    time_type step = (time_type) ((intptr_t) args->arg[5]);
    /* n is the number of steps remaining. int is big enough. */
    int n = (int) ((size_t) args->arg[6]);
    
    if (seq->runflag) {
        int voice = vc_voice(event->nvoice);
/*      printf("ramp_event: value %d to_value %d increment %d step %d n %d time %d\n",
               value, to_value, increment, step, n, virttime); */
        if (n == 0) value = to_value;
        else {/* update value */
            args->arg[2] = (void *)(size_t)(value + increment); 
            args->arg[6] = (void *)(size_t)(n - 1); /* update n */
            causepri((delay_type)step, 5, ramp_event, args);
        }
        if (event->value == CTRLRAMP_VALUE) {
            int ctrl = event->u.ramp.ctrl;
            if (ctrl == -TOUCH_CTRL) midi_touch(voice, value >> 8);
            else if (ctrl == -BEND_CTRL) midi_bend(voice, value >> 2);
            else midi_ctrl(voice, ctrl, value >> 8);
        } else { /* must be DEFRAMP_VALUE */
            send_macro(event->u.ramp.u.def.definition,
                       vc_voice(event->nvoice),
                       event->u.ramp.u.def.parameter,
                       event->u.ramp.u.def.parm_num, value >> 8,
                       event->nline);
        }
        /* really passing seq, but it's already in args: */
        if (n == 0) seq_end_event(args);
    }
}


/* report_enabled_channels -- print out concise listing of channels */
/*
 * to fit on one line, write out ranges, e.g. 1-5 9-11 
 */
void report_enabled_channels(seq)
  seq_type seq;
{
    ulong mask = seq_channel_mask(seq);
    int i, range_open_at = 0;

    for (i = 1; i <= MAX_CHANNELS; i++) {
        if (!range_open_at && (mask & 1)) {
            gprintf(TRANS, " %d", i);
            range_open_at = i;
        } else if (range_open_at && !(mask & 1)) {
            if (i > (range_open_at + 1)) {
                gprintf(TRANS, "-%d", i - 1);
            }
            range_open_at = 0; /* FALSE */
        }
        mask = mask >> 1;
    }
    if (range_open_at) gprintf(TRANS, "-%d", MAX_CHANNELS);
}


/* send_macro -- instantiate macro and send it */
/*
 * note: to support ramping, "value" is used in place of
 *       parameter["parm_num"]
 */
/*private*/
void send_macro(ptr, voice, parameter, parm_num, value, nline)
  register unsigned char *ptr;
  int voice;
  short parameter[];
  int parm_num;
  int value;    
  int nline;
{
    register unsigned char code, *loc;
    while ((code = *ptr++)) {
        loc = ptr + *ptr;
        ptr++;
        if (code <= nmacroparms) {
            code--;
            *loc = (code == parm_num ? value : parameter[code]) & 0x7f;
        } else if (code == nmacroparms + 1) {
            /* take old high order bits and OR in 4 voice bits */
            *loc = (*loc & 0xF0) | ((voice - 1) & 0xF);
        } else {
            code -= (nmacroparms + 2);
            *loc = ((code == parm_num ? value : parameter[code]) >> 7) & 0x7F;
        }
    }
    if (ptr[1] == MIDI_SYSEX) {
        midi_exclusive(ptr + 1);
    } else {
        /* make sure user didn't try to send more than 3 bytes.  This test
         * could be done at sequence read time, but it's tricky because the
         * first byte could be a parameter, so in general you need to 
         * plug the actual parameters into the message and then do the test.
         * Currently, this is the only place parameters are plugged in.
         */
        if (*ptr > 3) {
            gprintf(ERROR, 
                    "Non-sysex macro longer than 3 bytes ignored, line %d.\n",
                     nline);
        } else {
            midi_write((int) *ptr, MIDI_PORT(voice), ptr[1], ptr[2], ptr[3]);
        }
    }
}


/* seq_alloc -- a utility function to allocate a seq struct */
/**/
seq_type seq_alloc()
{
    seq_type seq;
    seq = (seq_type) memget(sizeof(seq_node));
    return seq;
}    


/* seq_at_end -- set the function to be called at sequence end */
/**/
void seq_at_end(seq, fn)
  seq_type seq;
  void (*fn)(seq_type);
{
    if (!fn) fn = noop;
    seq->stopfunc = fn;
}


/* seq_cause_noteoff_meth -- turn off a note in the future */
/**/
void seq_cause_noteoff_meth(seq_type seq, time_type delay, int voice, int pitch)
{
    if (seq->note_enable) {
        call_args_node args;
        pitch += seq->transpose;
        while (pitch < 0) pitch += 12;
        while (pitch > 127) pitch -= 12;
        seq->noteoff_count++;
        args.arg[0] = seq;
        args.arg[1] = (void *)(size_t)voice;
        args.arg[2] = (void *)(size_t)pitch;
        causepri((delay_type) delay, 10, seq->noteoff_fn, &args);
    }
}

/* seq_copy -- copy a sequence, share the eventlist */
/**/
seq_type seq_copy(from_seq)
  seq_type from_seq;
{
    register seq_type seq = seq_init(seq_alloc(), FALSE);
    if (!seq) return NULL;
    seq->chunklist = from_seq->chunklist;
    seq->current = seq_events(seq);
    seq->chunklist->u.info.refcount++;
    seq->transpose = from_seq->transpose;
    seq->loudness = from_seq->loudness;
    seq->rate = from_seq->rate;
    seq->paused = from_seq->paused;
    seq->noteoff_count = 0;
    return seq;
}


/* seq_create -- create a seq structure and an initial event chunk */
/**/
seq_type seq_create()
{
    return seq_init(seq_alloc(), TRUE);
}


/* seq_cycle -- set parameters for cycling a sequence */
/**/
void seq_cycle(seq_type seq, boolean flag, time_type dur)
{
    seq->cycleflag = flag;
    seq->cycledur = dur;
}


/* seq_end_event -- call this when an score-generated event ends */
/*
 * Assumes that noteoff_count was incremented when event started.
 */
void seq_end_event(call_args_type args)
{
    seq_type seq = (seq_type) args->arg[0];
    /*gprintf(TRANS, "nd");*/
    seq->noteoff_count--;
    if (seq->current == NULL /* finished seq */ &&
        seq->noteoff_count == 0 /* finished noteoff's */ &&
        seq->runflag /* we've not been stopped */) {
        if (seq->cycleflag) {
            cause((delay_type) (seq->cycledur - virttime), cycle, args);
        } else if (seq->stopfunc) {
            (*(seq->stopfunc))(seq);
        }
    }
}



/****************************************************************************
*               seq_free_meth
* Input: a seq_type
* Effect:
*    frees storage occupied by a seq
****************************************************************************/

private void seq_free_meth(seq_type seq)
{
    seq_free_chunks(seq);
    if (seq->timebase) timebase_free(seq->timebase);
    memfree((void *) seq, sizeof(seq_node));
}


/* seq_free_chunks -- free storage for note list */
/*
 * NOTE: in its original form, this routine was perhaps more readable,
 * but would not compile under Microsoft C V7.00 due to a compiler bug.
 * I rewrote the code until the bug disappeared, hopefully without
 * changing the semantics!  If you change this code, make sure it still
 * compiles under Microsoft C.
 *
 * This module frees chunks from a seq_type in preparation for freeing
 * the seq_type itself.  Reference counts are checked and chunks are
 * only freed when the last reference is removed.
 */
public void seq_free_chunks(seq)
  seq_type seq;
{
    chunk_type tail;
    chunk_type head;

    head = seq->chunklist;
    if (((head->u.info.refcount)--) != 0) return;

    while (head != NULL) {
        tail = head->next;
        memfree((void *) head, sizeof(chunk_node));
        head = tail;
        seq->chunklist = head;
    }
}


seq_type seq_init(seq, create_chunk)
    seq_type seq;
    int create_chunk;
{
    if (!seq || !(seq->timebase = timebase_create(50))) {
        return NULL;
    }
    seq->chunklist = NULL;
    if (create_chunk) {
        seq->chunklist = chunk_create(TRUE);
        if (!seq->chunklist) {
            seq_free(seq);
            return NULL;
        }
    }
    seq->cause_noteoff_fn = seq_cause_noteoff_meth;
    seq->midi_bend_fn     = seq_midi_bend_meth;
    seq->midi_ctrl_fn     = seq_midi_ctrl_meth;
    seq->midi_program_fn  = seq_midi_program_meth;
    seq->midi_touch_fn    = seq_midi_touch_meth;
    seq->noteoff_fn       = seq_noteoff_meth;
    seq->noteon_fn        = seq_noteon_meth;
    seq->free_fn          = seq_free_meth;
    seq->reset_fn         = seq_reset_meth;

    seq->current = NULL;
    seq->transpose = 0;
    seq->loudness = 0;
    seq->cycleflag = FALSE;
    seq->cycledur = 0L;
    seq->rate = 256L;
    seq->paused = FALSE;
    seq->stopfunc = noop;
    seq->channel_mask = 0xFFFFFFFFL;
    seq->runflag = seq->note_enable = FALSE;
    return seq;
}


/* seq_midi_bend_meth -- send a midi bend */
/**/
void seq_midi_bend_meth(seq_type seq, int voice, int value)
{
    midi_bend(voice, value);
}


/* seq_midi_ctrl_meth -- send a midi ctrl change */
/**/
void seq_midi_ctrl_meth(seq_type seq, int voice, int ctrl, int value)
{
    midi_ctrl(voice, ctrl, value);
}


/* seq_midi_program_meth -- send a midi program change */
/**/
void seq_midi_program_meth(seq_type seq, int voice, int prog)
{
    midi_bend(voice, prog);
}


/* seq_midi_touch_meth -- send a midi touch */
/**/
void seq_midi_touch_meth(seq_type seq, int voice, int value)
{
    midi_touch(voice, value);
}


/* seq_noteoff_meth -- turn a seq note off */
/**/
void seq_noteoff_meth(call_args_type args)
{
    /* seq_type seq = args->arg[0]; -- unused */
    /* these are 8 bit values stored in pointers; coerce
     * in 2 steps to avoid compiler warnings */
    int voice = (int) ((size_t) args->arg[1]);
    int pitch = (int) ((size_t) args->arg[2]);
    midi_note(voice, pitch, 0);
        /*gprintf(TRANS, "_e");*/
    seq_end_event(args);
}


/* seq_noteon_meth -- play a note with transformations */
/**/
void seq_noteon_meth(seq_type seq, int chan, int pitch, int vel)
{
    if (seq->note_enable) {
        pitch += seq->transpose;
        while (pitch < 0) pitch += 12;
        while (pitch > 127) pitch -= 12;

        vel += seq->loudness;
        if (vel <= 0) vel = 1;
        else if (vel > 127) vel = 127;

        midi_note(chan, pitch, vel);
    }
}


/* seq_pause -- stop playing momentarily or resume playing */
/**/
time_type seq_pause(seq_type seq, boolean flag)
{
    if (!seq->paused && flag) {
        seq->paused = TRUE;
        seq->rate = seq->timebase->rate;
        set_rate(seq->timebase, STOPRATE);
    } else if (seq->paused && !flag) {
        seq_play(seq);
    }
    return (time_type) seq->timebase->virt_base;
}


/* seq_play -- play a sequence from the current event forward */
/**/
void seq_play(seq)
  seq_type seq;
{
    timebase_type prev_timebase = timebase;
    register timebase_type reg_timebase = seq->timebase;

    if (!seq->runflag) {
        seq_reset(seq);
    }
    if (!seq->paused) return;
    eventtime = gettime();

    /* assume that virt_base is correct virtual time as the result
        of seq_start_time or seq_reset
     */
    timebase = reg_timebase;
    virttime = reg_timebase->virt_base;
    /* note that set_rate will set reg_timebase->real_base to eventtime */
    set_rate(reg_timebase, seq->rate);
    seq->paused = FALSE; /* in case the score had been paused; note that
                            seq_pause() has no effect if paused is TRUE */
    seq->runflag = TRUE;
    seq->note_enable = TRUE;

    /* restore previous timebase */
    timebase_use(prev_timebase);
}


/* seq_reset_meth -- reset a sequence to start back at the first event */
/**/
void seq_reset_meth(seq_type seq)
{
    timebase_type old_timebase = timebase;

    if (seq->runflag) {
        /* maybe this seq is already reset, and process_event is
         * already scheduled.  If so, don't schedule another one.
         */
        if ((seq->timebase->virt_base == 0) &&
            (seq->timebase->rate == STOPRATE)) {
            /* in case the reader just iterated through the list without
             * cause'ing events, reset the event list
             */
            seq->current = seq_events(seq);
            return;
        }
        /* Otherwise, the seq is running, so stop it. */
        seq_stop(seq);
    }
    
    timebase_use(seq->timebase);
    set_rate(seq->timebase, STOPRATE);
    set_virttime(seq->timebase, 0L);
    seq->current = seq_events(seq);
    seq->noteoff_count = 0L;
    seq->runflag = TRUE;
    seq->paused = TRUE;
    if (seq->current) {
        call_args_node args;
        args.arg[0] = seq;
        cause((delay_type)(seq->current->ntime - virttime),
              process_event, &args);
    }
    timebase_use(old_timebase);
}


/* seq_set_loudness -- set the loudness offset of a sequence */
/**/
void seq_set_loudness(seq, loud)
  seq_type seq;
  int loud;
{
    seq->loudness = loud;
}

/* seq_set_rate -- set the rate of a sequence */
/**/
void seq_set_rate(seq, rate)
  seq_type seq;
  time_type rate;
{
    seq->rate = rate;
    if (!seq->paused) set_rate(seq->timebase, rate);
}


/* seq_set_transpose -- set the sequence transposition */
/**/
void seq_set_transpose(seq, trans)
  seq_type seq;
  int trans;
{
    seq->transpose = trans;
}


/* seq_start_time -- set the current pointer so the sequence starts here */
/**/
void seq_start_time(seq, start_time)
  seq_type seq;
  time_type start_time;
{
    timebase_type prev_timebase = timebase;
    if (!seq->runflag) {
        seq_reset(seq);
    }
    if (real_to_virt(seq->timebase, eventtime) > start_time) {
        seq_reset(seq);
    }
    timebase_use(seq->timebase);
    seq->note_enable = FALSE;
    /* prime the pump */
    set_rate(timebase, STOPRATE);
    set_virttime(timebase, start_time);
    catchup();
    seq->note_enable = TRUE;
    seq->paused = TRUE;
    /* restore previous timebase */
    timebase_use(prev_timebase);
}


/* seq_stop -- stop a sequence, clear out all pending events */
/**/
void seq_stop(seq)
  seq_type seq;
{
    timebase_type prev_timebase = timebase;

    if (seq->runflag) {
        if (moxcdebug)
            gprintf(TRANS, "seq_reset swap from timebase 0x%x to 0x%x\n",
                    timebase, seq->timebase);
        timebase = seq->timebase;
        seq->runflag = FALSE;
        set_rate(timebase, STOPRATE);
        set_virttime(timebase, MAXTIME);
        catchup();
    }
    timebase_use(prev_timebase);
}
