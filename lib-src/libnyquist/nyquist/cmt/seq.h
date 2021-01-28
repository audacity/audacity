/* seq.h -- definitions for seq, the MIDI Toolkit sequence data type */

#define minpitch 0
#define maxpitch 127
#define NO_PITCH (maxpitch+1)
#define minprogram 1
#define maxprogram 128

/* keep these two lines in sync */
#define nmacroparms 4
#define parm_expected_error "Parameter number [1-4] expected"

#define seq_dflt_loud 127
#define seq_dflt_voice 1
#define seq_dflt_pitch 60

struct clock_struct {
    time_type ticksize;
};


struct ctrlramp_struct {
    unsigned char from_value;
    unsigned char to_value;
};


struct deframp_struct {
    unsigned char *definition;
    short parameter[nmacroparms];
    unsigned char parm_num;
    short to_value;
};


struct macctrl_struct {
    unsigned char ctrl_number;
    unsigned char value;
};


struct macro_struct {
    unsigned char *definition;
    short parameter[nmacroparms];
};


struct note_struct {
    long ndur;  /* duration */
    /* char nloud;  loudness (MIDI velocity) now stored as low order byte
     * of ndur
     */
};


struct ramp_struct {
    time_type dur;
    short ctrl;         /* encode pitch bend and after touch as negative */
    short step;
    union {
        struct ctrlramp_struct ctrl;
        struct deframp_struct def;
    } u;
};


struct seti_struct {
    int *int_to_set;
    int value;
};


#define SEQ_MAX_PARMS 8

typedef struct seq_arg_struct {
    long a[SEQ_MAX_PARMS];
} seq_arg_t;

typedef int (*seq_cmd_fn)(seq_arg_t args);

struct cause_struct {
    seq_cmd_fn routine;
    /* make a structure so we can copy by value */
    seq_arg_t args;
};


typedef struct event_struct {
    struct event_struct *next;
    time_type ntime;    /* start time */
    short nline;        /* line number from source code */
    unsigned char nvoice;    /* adagio voice (MIDI Channel)
                 *  if this is a control change, high order 4 bits
                 *  contain the control number, otherwise high order
                 *  4 bits are 0 (see is_note macro below)
                 */
    unsigned char value;
                    /* this is a note pitch or a control value.  It goes
                 * here rather than in the union to achieve word
                 * alignment (!).  Also, value is used for extra commands
                 * such as call, seti, setv.
                 */
    union {
        struct cause_struct call;
        struct clock_struct clock;
        struct macctrl_struct macctrl;
        struct macro_struct macro;
        struct note_struct note;
        struct ramp_struct ramp;
        struct seti_struct seti;
    } u;
} event_node, *event_type;

#define PSWITCH_CTRL 1
#define MODWHEEL_CTRL 2
#define TOUCH_CTRL 3
#define VOLUME_CTRL 4
#define BEND_CTRL 5
#define PROGRAM_CTRL 6
#define ESC_CTRL 7

#define CALL_VALUE 0
#define CLOCK_VALUE 1
#define MACCTRL_VALUE 2
#define MACRO_VALUE 3
#define CTRLRAMP_VALUE 4
#define DEFRAMP_VALUE 5
#define SETI_VALUE 6

#define commonsize (sizeof(struct event_struct) - sizeof(struct cause_struct))
#define rampcommon (sizeof(struct ramp_struct) - sizeof(struct deframp_struct))
#define ctrlsize commonsize
#define callsize (commonsize + sizeof(struct cause_struct))
#define clocksize (commonsize + sizeof(struct clock_struct))
#define ctrlrampsize (commonsize + rampcommon + sizeof(struct ctrlramp_struct))
#define deframpsize (commonsize + sizeof(struct ramp_struct))
#define macctrlsize (commonsize + sizeof(struct macctrl_struct))
#define macrosize (commonsize + sizeof(struct macro_struct))
#define notesize (commonsize + sizeof(struct note_struct))
#define setisize (commonsize + sizeof(struct seti_struct))
#define ctrl_voice(c, v) (((c) << 5) + ((v) - 1))
#define vc_ctrl(v) ((v) >> 5)
#define vc_voice(v) (((v) & 0x1F) + 1)

#define is_note(n) (((n)->nvoice & 0xE0) == 0)

#define CHUNK_SIZE 2000

typedef struct def_struct {
    struct def_struct *next;
    char *symbol;
    unsigned char *definition;
} def_node, *def_type;


typedef struct chunk_struct {
    struct chunk_struct *next;
    short free;
    union {
        char data[CHUNK_SIZE];
        struct info_struct {
            short refcount;
            struct chunk_struct *last_chunk; /* where to allocate memory */
            def_type dictionary;    /* macro defns, routine addresses */
            event_type eventlist;   /* first event in sequence */
            ulong used_mask;        /* tells what channels are actually present */
            long ctrlcount;
            long notecount;
            time_type duration;     /* time of the last event+dur in score */
        } info;
    } u;
} chunk_node, *chunk_type;


typedef struct seq_struct {
    void (*cause_noteoff_fn)(struct seq_struct *, time_type, int, int);
    void (*midi_bend_fn)(struct seq_struct * seq, int voice, int value);
    void (*midi_ctrl_fn)(struct seq_struct * seq, int voice, int ctrl, int value);
    void (*midi_program_fn)(struct seq_struct * seq, int voice, int prog);
    void (*midi_touch_fn)(struct seq_struct * seq, int voice, int value);
    void (*noteoff_fn)(call_args_type args);
    void (*noteon_fn)(struct seq_struct * seq, int chan, int pitch, int vel);
    void (*free_fn)(struct seq_struct * seq);
    void (*reset_fn)(struct seq_struct * seq);
    void (*stopfunc)(struct seq_struct *);
    chunk_type chunklist;
    /* event_type eventlist;
         seq->eventlist is now seq->eventlist->chunklist */
    event_type current;
    boolean runflag;            /* normally true, set to false as a flag for
                                 * processes started by score to terminate */
    boolean note_enable;        /* normally true, set to false as a flag to
                                 * suppress note output, e.g. when indexing
                                 * to a particular time point */
    boolean cycleflag;          /* normally false, set to true to make the
                                 * sequence cycle every cycledur */
    int transpose;
    int loudness;
    time_type cycledur;
    timebase_type timebase;
    time_type rate;                     /* remembers rate across pauses */
    boolean paused;             /* remembers if seq has been stopped or not */
    short noteoff_count;        /* keeps track of pending events, such as
                                 * note off commands.  When this count goes
                                 * to zero, the score is finished */
    ulong channel_mask;
} seq_node, *seq_type;

extern seq_type sequence;

chunk_type chunk_create(boolean first_flag);

#define seq_cause_noteoff(seq, delay, voice, pitch) \
        (*(((seq_type) seq)->cause_noteoff_fn))(seq, delay, voice, pitch)
#define seq_midi_bend(seq, voice, value) \
        (*(((seq_type) seq)->midi_bend_fn))(seq, voice, value)
#define seq_midi_ctrl(seq, voice, ctrl, value) \
        (*(((seq_type) seq)->midi_ctrl_fn))(seq, voice, ctrl, value)
#define seq_midi_program(seq, voice, prog) \
        (*(((seq_type) seq)->midi_program_fn))(seq, voice, prog)
#define seq_midi_touch(seq, voice, value) \
        (*(((seq_type) seq)->midi_touch_fn))(seq, voice, value)
#define seq_noteoff(seq, args) \
        (*(((seq_type) seq)->noteoff_fn))(args)
#define seq_noteon(seq, voice, pitch, vel) \
        (*(((seq_type) seq)->noteon_fn))(seq, voice, pitch, vel)
#define seq_free(seq) (*(((seq_type) seq)->free_fn))(seq)
#define seq_register(seq) \
    cu_register((cu_fn_type) (((seq_type) seq)->free_fn), seq)
#define seq_reset(seq) (*(((seq_type) seq)->reset_fn))(seq)
 /* LISP: void (SEQ-RESET SEQ) */

extern boolean seq_print;       /* debugging switch */

void seq_extensions(void);      /* to be defined outside of seq -- user dependent */
event_type insert_call(seq_type seq, time_type ctime, int cline, int voice,
        int (*addr)(seq_arg_t args), long value[SEQ_MAX_PARMS], int n);
event_type insert_clock(seq_type seq, time_type ctime, int cline,
                        time_type ticksize);
event_type insert_ctrl(seq_type seq, time_type ctime, int cline, int ctrl,
                       int voice, int value);
 /* LISP: (SEQ-INSERT-CTRL SEQ LONG LONG LONG LONG LONG) */
event_type insert_ctrlramp(seq_type seq, time_type rtime, int rline, int voice,
                      time_type step, time_type dur, int ctrl, int v1, int v2);
 /* LISP: (SEQ-INSERT-RAMP SEQ LONG LONG LONG LONG LONG LONG LONG LONG) */
def_type insert_def(seq_type seq, char *symbol, unsigned char *definition,
                    int deflen);
event_type insert_deframp(seq_type seq, time_type rtime, int rline, int voice,
                     time_type step, time_type dur, def_type def,
                     int nparms, short parms[], int parm_num, int to_value);
event_type insert_macctrl(seq_type seq, time_type ctime, int cline, int ctrl,
                          int voice, int value);
 /* LISP: (SEQ-INSERT-MACCTRL SEQ LONG LONG LONG LONG LONG) */
event_type insert_macro(seq_type seq, time_type ctime, int cline,
                        def_type def, int voice, int nparms, short *parms);
event_type insert_note(seq_type seq, time_type ntime, int nline, int voice,
                       int pitch, time_type dur, int loud);
 /* LISP: (SEQ-INSERT-NOTE SEQ LONG LONG LONG LONG LONG LONG) */
event_type insert_seti(seq_type seq, time_type stime, int sline, int voice,
                       int *addr, int value);
void noop(seq_type seq);
seq_type seq_alloc(void);
void seq_at_end(seq_type seq, void (*fn)(seq_type));
void seq_cause_noteoff_meth(seq_type seq, time_type delay, int voice, int pitch);
#define seq_channel_mask(seq) ((seq)->channel_mask)
seq_type seq_copy(seq_type from_seq); /* LISP: (SEQ-COPY SEQ) */
seq_type seq_create(void); /* LISP: (SEQ-CREATE) */
void seq_cycle(seq_type seq, boolean flag, time_type dur);
#define seq_duration(seq) (((seq_type) seq)->chunklist->u.info.duration)
void seq_end_event(call_args_type args);
#define seq_events(seq) (((seq_type) seq)->chunklist ? \
    (((seq_type) seq)->chunklist->u.info.eventlist) : NULL) 
#define seq_dictionary(seq) (seq)->chunklist->u.info.dictionary
#define seq_eventlist(seq) (seq)->chunklist->u.info.eventlist
#define seq_ctrlcount(seq) (seq)->chunklist->u.info.ctrlcount
#define seq_notecount(seq) (seq)->chunklist->u.info.notecount
#define seq_used_mask(seq) (seq)->chunklist->u.info.used_mask
void seq_free_chunks(seq_type seq);
seq_type seq_init(seq_type seq, int create_chunk);
#define seq_loudness(seq) (((seq_type) seq)->loudness)
void seq_midi_bend_meth(seq_type seq, int voice, int value);
void seq_midi_ctrl_meth(seq_type seq, int voice, int ctrl, int value);
void seq_midi_program_meth(seq_type seq, int voice, int prog);
void seq_midi_touch_meth(seq_type seq, int voice, int value);
void seq_noteon_meth(seq_type seq, int voice, int pitch, int vel);
void seq_noteoff_meth(call_args_type args);
time_type seq_pause(seq_type seq, boolean flag);
void seq_play(seq_type seq);
#define seq_rate(seq) ((seq_type) seq)->rate
void seq_reset_meth(seq_type seq);
#define seq_runflag(seq) ((seq_type) seq)->runflag
#define seq_set_channel_mask(seq, cm) ((seq)->channel_mask) = (cm)
void seq_set_loudness(seq_type seq, int offset);
void seq_set_rate(seq_type seq, time_type rate);
#define seq_set_timebase(seq, tb) ((seq_type) seq)->timebase = (tb)
void seq_set_transpose(seq_type seq, int trans);
void seq_start_time(seq_type seq, time_type start_time);
void seq_stop(seq_type seq);
#define seq_timebase(seq) ((seq_type) seq)->timebase
#define seq_transpose(seq) ((seq_type) seq)->transpose
