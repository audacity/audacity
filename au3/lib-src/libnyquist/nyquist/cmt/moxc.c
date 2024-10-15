/* MOXC -- a C version of Collinge's MOXIE language */
/* Copyright 1989 Carnegie Mellon University */

/*****************************************************************************
*       Change Log
*  Date     | Change
*-----------+-----------------------------------------------------------------
* 31-Dec-85 | Modified for use with midi
*  5-Feb-86 | Added m_rest and m_restuntil allowing rests at top level
* 28-May-86 | Added command line parsing
*  4-Jun-86 | changed keyevent to separate calls for each event type
* 10-Jul-86 | put loop in mainscore with prompt to play and replay
* 03-Jun-88 | modified for portability (AMIGA) -JCD
* 07-Jul-89 | time bases -RBD
* 31-Jan-90 | GWL : cleaned up for LATTICE
* 30-Jun-90 | RBD : further changes
*  2-Apr-91 | JDW : further changes
*  4-Mar-91 | GWL : DOS allows odd inst addrs
* 10-Oct-94 | nix : posicionador tridimensionale interface
* 28-Apr-03 |  DM : true->TRUE, false->FALSE
* 18-May-15 | RBD : The varargs hack to allow arbitrary parameters (only
*           | limited by total size) passed to cause() breaks down on ia64
*           | where args are not passed uniformly on the stack. To fix this
*           | the beautiful cause() that survived 30 years of ports, will 
*           | finally have to go. The new cause() will take only one
*           | parameter to pass along to callees. It will be of type
*           | call_args_node (the same type used by the scheduler to store
*           | the args.) It will now be up to the caller to pack the real
*           | args into call_args_node, and up to the callee to unpack.
*           | (packing/unpacking were previously hidden in moxc.c)
*****************************************************************************/

#include "switches.h"

#ifdef AMIGA
#ifdef AZTEC
#include "functions.h"
#else
#include "amiga.h"
#endif
#include "exec/exec.h"
#include "cmtcmd.h"

extern long event_mask; /* imported from midifns.c */
#endif
extern int abort_flag;  /*DMH: taken out of ifdef AMIGA for moxcrun*/

#include "stdio.h"
#include "cext.h"
#include "userio.h"
#include "midifns.h"

#include "cmdline.h"
#include "midicode.h"
#include "timebase.h"
#include "moxc.h"

#ifdef AMIGA /*DMH: only AMIGA cares about AMIGA's "proportional controllers"*/
#include "prop1.h"
#endif
#ifdef POSICIONADOR_3D
#include "pos3d.h"
#include "pos3dbuf.h"
#endif /* POSICIONADOR_3D */

extern char *app_syntax;

/***************************************************************************
*
*  IMPORTS:
*       asciievent(k)           user-defined action for terminal input
*       bendchange(ch, val)     user-defined pitch bend handler
*       ctrlchange(ch, c, val)  user-defined control change handler
*       keydown(ch, p, v)       user-defined MIDI note on handler
*       keyup(ch, p)            user-defined MIDI note off handler
*       mainscore()             user-defined first action(s)
*       musicfns                lots of time and io functions
*       peddown(ch)             user-defined pedal down handler
*       pedup(ch)               user-defined pedal up handler
*       touchchange(ch, val)    user-defined aftertouch handler
*       app_syntax              string defining extra command line options
*
*  EXPORTS:
*       
*       cause(delay, routine, packed_args)
*       moxcdone -- set to TRUE to quit
*       eventtime -- ideallized current time
*
*****************************************************************************/

#define SAFEMOXC TRUE
#define BREAKKEY 0x03

int moxcdone;   /* flag to halt execution */
time_type eventtime;    /* time of current call -- used to avoid        */
                        /* timing errors due to finite execution speed  */
time_type virttime;     /* virtual time of current call */
timebase_type timebase; /* time base of current call */
int mididecode = TRUE;  /* whether to decode messages or just call midievent */

int debug = FALSE;
int moxcdebug = FALSE;
time_type next_wakeup;
timebase_type default_base;

#ifdef AMIGA
int pub_port_signal;
struct MsgPort pub_port;
#endif

/*****************************************************************************
*       Routines local to this module
*****************************************************************************/

private void callrun(void);
private void decode(void);
private void moxcterm(void);

/****************************************************************************
*                           callallcancel
* Inputs:
*       timebase_queue
* Effect: 
*       return all calls to free list
* Implementation:
*             If timebase_queue is not empty, there's a pending call.  Remove the call
*             (not necessarily the timebase) and repeat.
****************************************************************************/

void callallcancel(void)
{
    if (moxcdebug) gprintf(GDEBUG, "cancel all calls\n");
    while (timebase_queue) {
        timebase = timebase_queue;
        timebase_queue = timebase->next;
        while (timebase->heap_size > 0) {
            call_free(remove_call(timebase));
        }
        insert_base(timebase);
    }
}

/* catchup -- bring current timebase up to date by running its calls */
/**/
void catchup(void)
{
    register call_type call;
    /* Remember where we're going in virtual time because setting the
     * rate will also modify timebase->virt_base.  We don't want catchup
     * to stop short:
     */
    time_type target_time = timebase->virt_base;
    /* remember timebase here because it's possible that a call will do
     * a timebase_use() and change it:
     */
    register timebase_type my_base = timebase;

    while (my_base->heap_size != 0 &&
           (my_base->heap[1]->u.e.time < target_time)) {
        /* eventtime is the real time at which something was scheduled */
        eventtime = (my_base->next_time) >> 8;
        call = remove_call(my_base);
        virttime = call->u.e.time;
        (*(call->u.e.routine))(&(call->u.e.p));
        call_free(call);
    }
    /* now that we've possibly pulled events out of the timebase, adjust
     * the position in the timebase queue (and possibly remove it).
     */
    remove_base(my_base);
    insert_base(my_base);
}



/****************************************************************************
*                               cause
* Inputs:
*       delay_type (long) delay: time before this call should occur
*       int (*routine)(): routine that implements the call
*       int p1 through p8: parameters to pass to routine
* Effect: 
*       builds a call and puts it in pending queue for later scheduling
****************************************************************************/

#ifndef DOTS_FOR_ARGS
THIS CODE IS OBSOLETE
void cause(delay, routine, p)
    delay_type  delay;
    int (*routine)();
    call_args_node p;
#else
#include <stdarg.h>

void cause(delay_type delay, void (*routine)(call_args_type args), call_args_type p)
#endif
{
    register call_type call = call_alloc();

    if (!call) {
        gprintf(ERROR, "cause: out of memory\n");
        EXIT(1);
    }

#ifdef DOTS_FOR_ARGS
    call->u.e.time = virttime + delay;
    call->u.e.priority = 128; /* default priority */
    call->u.e.routine = routine;
    call->u.e.p = *p;
#else
    THIS CODE IS OBSOLETE
    call->u.e.time = virttime + delay;
    call->u.e.priority = 128; /* default priority */
    call->u.e.routine = routine;
    call->u.e.p = p;
#endif
#ifdef SAFEMOXC
    if (call->u.e.routine == 0) {
        gprintf(ERROR,"cause called with NULL routine\n");
        EXIT(1);
/* Intel (x86 or x86_64) allows odd routine addresses; 
 * RISC architectures do not.
 */
#if (!defined(DOS) && __i386__ != 1 && __x86_64__ != 1)
    } else if (((long) call->u.e.routine) & 1) {
        gprintf(ERROR, "cause called with bad routine address: 0x%lx\n",
                call->u.e.routine);
        EXIT(1);
#endif
    }
#endif
    /* put call in default queue */
    callinsert(timebase, call);
    if (moxcdebug) {
        gprintf(GDEBUG,"(cause) call is pending on timebase 0x%x:\n", timebase);
        callshow(call);
    }
}

/****************************************************************************
*                               causepri
* Inputs:
*       int delay: time before this call should occur
*       int pri: priority, lowest priority goes first
*       int (*routine)(): routine that implements the call
*       int p1 through p8: parameters to pass to routine
* Effect: 
*       builds a call and puts it in pending queue for later scheduling
****************************************************************************/

#ifndef DOTS_FOR_ARGS
THIS CODE IS OBSOLETE
void causepri(delay, pri, routine, p)
    delay_type  delay;
    int pri;
    int (*routine)();
    call_args_node p;
#else
/* already included stdarg.h */

void causepri(delay_type delay, int pri, void (*routine)(call_args_type args), call_args_type args)
#endif
{
    register call_type call = call_alloc();

    if (!call) {
        gprintf(ERROR, "cause: out of memory\n");
        EXIT(1);
    }

#ifdef DOTS_FOR_ARGS
    call->u.e.time = virttime + delay;
    call->u.e.priority = pri; /* default priority */
    call->u.e.routine = routine;
    call->u.e.p = *args;
#else
    call->u.e.time = virttime + delay;
    call->u.e.priority = pri; /* default priority */
    call->u.e.routine = routine;
    call->u.e.p = p;
#endif
#ifdef SAFEMOXC
    if (call->u.e.routine == 0) {
        gprintf(ERROR,"cause called with NULL routine\n");
        EXIT(1);
/* Intel (x86 or x86_64) allows odd routine addresses; 
 * RISC architectures do not.
 */
#if (!defined(DOS) && __i386__ != 1 && __x86_64__ != 1)
    } else if (((long) call->u.e.routine) & 1) {
        gprintf(ERROR, "causepri called with bad routine address: 0x%lx\n",
                call->u.e.routine);
        EXIT(1);
#endif
    }
#endif
    /* put call in default queue */
    callinsert(timebase, call);
    if (moxcdebug) {
        gprintf(GDEBUG,"(cause) call is pending:");
        callshow(call);
    }
}

/****************************************************************************
*                               callrun
* Inputs:
*       call_type call: the call to execute
* Effect: 
*       executes the previously scheduled call call and deallocates it
****************************************************************************/

private void callrun(void)
{
    call_type call;
    if (moxcdebug) {
        gprintf(GDEBUG,"(callrun) running a call: \n");
    }
    /* remove from head of queue */
    while (!timebase_queue) gprintf(TRANS, "callrun fatal error\n");
    timebase = timebase_queue;
    timebase_queue = timebase->next;

    if (debug) gprintf(TRANS, "callrun time %ld\n", timebase->next_time);
    eventtime = (timebase->next_time) >> 8; /* real time of the call */

    /* remove first call from timebase */
    call = remove_call(timebase);
    if (debug) gprintf(TRANS, "callrun call %p\n", call);
    insert_base(timebase);
    virttime = call->u.e.time;          /* virtual time of the call */
    if (moxcdebug) callshow(call);
    (*(call->u.e.routine))(&(call->u.e.p));
    call_free(call);
}

/****************************************************************************
*                                 m_restuntil
* Inputs:
*       int time: call time to rest until
* Effect: 
*       Waits until the specified time has been reached (absolute time).
*       Other "caused" calls will take place during the rest provided
*       this routine is called from "mainscore" (see m_rest description).
****************************************************************************/
void m_restuntil(time)
  time_type time;
{
    time = virt_to_real(timebase, time);
    while(time > gettime()) {
        moxcwait(time);
    }
}


/****************************************************************************
*                                   m_rest
* Inputs:
*       int time: Amount of time to rest
* Effect: 
*       Waits until the amount of time specified has lapsed
* Assumes:
*       Must not be called from a "caused" routine.  Must only be called
*       from "mainscore" or a routine called directly or indirectly from
*       "mainscore" without using "cause".
****************************************************************************/

void m_rest(time)
  time_type time;
{
    m_restuntil(time + real_to_virt(timebase, gettime()));      
}

/****************************************************************************
*                               moxcinit
* Inputs:
*       int argc: number of command line arguments
*       char * argv: command line argument array
* Effect: initializes moxc system
****************************************************************************/

boolean moxcinit(argc, argv)
    int argc;
    char * argv[];
{
    meminit();
    io_init();
#ifdef AMIGA
    pub_port_signal = AllocSignal(-1L);
    pub_port.mp_Node.ln_Type = NT_MSGPORT;
    pub_port.mp_SigBit = pub_port_signal;
    pub_port.mp_SigTask = FindTask(0L);
    pub_port.mp_Flags = PA_SIGNAL;
    pub_port.mp_Node.ln_Name = "CMTcmdport";
    pub_port.mp_MsgList.lh_Head =
        (struct Node *)&pub_port.mp_MsgList.lh_Tail;
    pub_port.mp_MsgList.lh_TailPred =
        (struct Node *)&pub_port.mp_MsgList.lh_Head;
    event_mask |= (1L << pub_port_signal);
    AddPort(&pub_port);
#endif
    cu_register((cu_fn_type) moxcterm, NULL);
    cl_syntax(midifns_syntax);
    cl_syntax("debug<s>Enable verbose debugging;\
        moxc<s>Enable moxc debug mode;");
    cl_syntax(app_syntax);

    if (!cl_init(argv, argc)) {
        /* make sure user gets to read the error message(s): */
        gprintf(TRANS, "Type anything to exit...");
#ifdef  DOS
        wait_ascii();
#else
        ggetchar();
#endif
        return FALSE;
    }
    debug = cl_switch("debug");
    moxcdebug = cl_switch("moxc");
    timebase = default_base = timebase_create(100);
    default_base->rate = 2560L;

    eventtime = 0L;
    next_wakeup = MAXTIME;
    musicinit();
#ifdef POSICIONADOR_3D
    ptInit();
#endif
    moxcdone = 0;
    return TRUE;
}


/****************************************************************************
*                               moxcwait
* Input:
*       -1 => wait for next keyboard or midi event or queued event
*       0 => don't wait
*       T => wait up to T for next keyboard or midi event or queued event
*               (this is used by m_restuntil)
* Assume: there is work to do (npending > 0 || evqueue) ??
* Effect: dispatch on user inputs, cause calls
****************************************************************************/

void moxcwait(dateoftimeout)
  time_type dateoftimeout;
{
    time_type maxtime = dateoftimeout;

    if (timebase_queue) {
        if ((timebase_queue->next_time >> 8) < maxtime) 
            maxtime = (timebase_queue->next_time) >> 8;
    }
    eventwait(maxtime);
    decode();
}
   

/****************************************************************************
*                               decode
* Effect: dispatch on user inputs, cause calls
****************************************************************************/

private void decode(void)
{
    /* It is important that midi_data is on a word boundary because we
        copy to it by doing a word transfer.
     */
    byte midi_data[4];
    time_type now;
    byte code;
    char k;
#ifdef AMIGA
    struct cmd_msg *cmd;
#endif

    now = gettime();
    timebase = default_base;
    eventtime = now;
    virttime = 0L;

/*   gprintf(GDEBUG, "decode at time %ld\n", now); */

/**********************************************
* poll for and decode midi keyboard input 
***********************************************/

    while (getbuf(FALSE, midi_data)) {
        /* only divide if necessary, divides take 100us on 8MHz 68000: */
        if (virttime == 0)
            virttime = real_to_virt(default_base, now);

        /* short-circuit midi decoding */
        if (!mididecode) {
            midievent(midi_data);
            continue;
        }

        code = midi_data[0] & MIDI_CODE_MASK;
        if (code == MIDI_ON_NOTE) {
            if (midi_data[2] == 0) {    /* velocity 0 -> note off */
                 keyup(1+(midi_data[0] & MIDI_CHN_MASK), midi_data[1]);
            } else {
                keydown((midi_data[0] & MIDI_CHN_MASK)+1,
                        midi_data[1], midi_data[2]);
            }
        } else if (code == MIDI_OFF_NOTE) {
            keyup((midi_data[0] & MIDI_CHN_MASK)+1, midi_data[1]);
        } else if (code == MIDI_TOUCH) {
            touchchange((midi_data[0] & MIDI_CHN_MASK)+1,midi_data[1]);
        } else if (code == MIDI_BEND) {
            bendchange((midi_data[0] & MIDI_CHN_MASK)+1,
                        midi_data[1] + (midi_data[2] << 7));
        } else if (code == MIDI_CTRL && midi_data[1] == SUSTAIN) {
            if (midi_data[2] == 0) pedup((midi_data[0] & MIDI_CHN_MASK) + 1);
            else peddown((midi_data[0] & MIDI_CHN_MASK) + 1);
        } else if (code == MIDI_CTRL) {
            ctrlchange((midi_data[0] & MIDI_CHN_MASK) + 1,
                        midi_data[1], midi_data[2]);
        } else if (code == MIDI_CH_PROGRAM) {
            prgmchange((midi_data[0] & MIDI_CHN_MASK) + 1, midi_data[1] + 1);
/* think C midi driver doesn't handle sysex the way the Amiga drivers do (yet) */
#ifndef MACINTOSH
        } else if (code == MIDI_SYSEX) {
            sysex();
#endif
        }
    }

/**********************************************
* poll for ASCII keyboard input 
***********************************************/
    while (get_ascii(&k)) {
        virttime = real_to_virt(default_base, now);
        asciievent(k);
        /* if user doesn't handle abort char in asciievent,
           we should exit now to avoid an infinite loop */
        if (abort_flag) EXIT(1);
    }

#ifdef POSICIONADOR_3D
/**********************************************
* poll for posicionador tridimensionale input
**********************************************/
    {
         pt_value pt_data;
         while (ptGetValue(&pt_data)) {
             /* only divide if necessary, divides take 100us on 8MHz 68000: */
             if (virttime == 0)
                 virttime = real_to_virt(default_base, now);
             ptevent(&pt_data);
        }
    }
#endif /* POSICIONADOR_3D */

#ifdef AMIGA
/**********************************************
* poll for proportional controller port 
**********************************************/
    if (prop_1_events) {
        int events;

        Disable();
            events = prop_1_events;
            prop_1_events = 0;
        Enable();
        
        if (events & BUTTON_1_RIGHT_CHANGE)
            buttonchange(3, prop_1_right_button);
        if (events & BUTTON_1_LEFT_CHANGE)
            buttonchange(2, prop_1_left_button);
        if (events & PROP_1_LEFT_CHANGE)
            propchange(2, prop_1_left_data);
        if (events & PROP_1_RIGHT_CHANGE)
            propchange(3, prop_1_right_data);
    }

/**********************************************
* poll for input from public command port
***********************************************/

    while (cmd = (struct cmd_msg *) GetMsg(&pub_port)) {
        struct symb_descr *desc = &HASHENTRY(lookup(cmd->symbol_name));
/*      gprintf(TRANS, "got %lx (%s) from pub_port\n", cmd, cmd->symbol_name); */
        virttime = real_to_virt(default_base, now);
        if (!desc) {
            gprintf(TRANS, "Error, symbol %s undefined.\n", cmd->symbol_name);
        } else if (desc->symb_type != cmd->symb_type) {
             gprintf(TRANS, "Error, wrong type for symbol %s\n",
                cmd->symbol_name); 
        } else if (cmd->symb_type == fn_symb_type) {
/*          gprintf(TRANS, "Calling routine\n"); */
            (*(desc->ptr.routine))(
             (int) cmd->the_args[0], (int) cmd->the_args[1],
             (int) cmd->the_args[2], (int) cmd->the_args[3],
             (int) cmd->the_args[4], (int) cmd->the_args[5],
             (int) cmd->the_args[6], (int) cmd->the_args[7]
            );
        } else if (cmd->symb_type == var_symb_type) {
            *(desc->ptr.intptr) = (int) cmd->the_args[0];
        } else if (cmd->symb_type == vec_symb_type) {
            if (cmd->the_args[0] >= desc->size) {
                gprintf(TRANS, "Error: Vector %s is of size %d\n",
                        cmd->symbol_name, desc->size);
            } else {
                (desc->ptr.intptr)[cmd->the_args[0]] = cmd->the_args[1];
/*              gprintf(TRANS, "vec: setting %lx\n",
                        &(desc->ptr.intptr)[cmd->the_args[0]]); */
            }
        } else gprintf(TRANS, "Symbol Type Error\n");
        ReplyMsg(&(cmd->msg));
    }
#endif
/**********************************************
* poll for next call in queue
***********************************************/
    now = (now + 1) << 8;  /* shift because next_time is also scaled,
                            * add 256 because next_time has added priority */
    if (debug)
        gprintf(TRANS, "now %ld next_time %ld\n",
                now, (timebase_queue ? timebase_queue->next_time : 1234));
    /* give pending events priority, but every 100 events, loop to allow
        input processing (user may want to give a "quit" command) */
    for (k = 0; 
          k < 100 && timebase_queue && (now > timebase_queue->next_time);
          k++) {
        callrun();
    }
/*******************
* flush text output 
********************/
#ifdef MACINTOSH_OR_UNIX
    gflush();
#endif
}


/****************************************************************************
*                               quit
* Effect: tells moxc to shut down
****************************************************************************/

void quit()
{
    moxcdone = TRUE;
}

/* moxcrun -- schedule events until done */
/**/
void moxcrun()
{
    moxcdone = FALSE;
    while (!moxcdone && !abort_flag) {          /* test for finish */
        if (!timebase_queue) moxcdone = TRUE;
        else moxcwait(MAXTIME);         /* do work */
    }
}

/* moxcterm -- clean up after moxcinit */
/**/
private void moxcterm(void)
{
#ifdef AMIGA
    FreeSignal((long) pub_port_signal);
    RemPort(&pub_port);
#endif
}
