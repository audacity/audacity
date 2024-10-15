/* sound.h -- new nyquist sound data type */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  changes for portability: moved some defns out of here
 */

#include <math.h>
#include "stdefs.h"

/* used for *AUDIO-MARKERS* */
extern int64_t sound_frames;
extern double sound_srate;

extern long max_sample_blocks;

#if OSC
extern int nosc_enabled; /* enable polling for OSC messages */
#endif

#if USE_PRINTF
#define nyquist_printf printf
#endif

#define PERMS            0644           /* -rw-r--r-- */

/* default stop sample count (for clipping) */
#define MAX_STOP 0x7FFFFFFFFFFFFFFF

/* default stop time (for clipping) */
#define MAX_STOP_TIME 10E20
/* LISP-SRC: (SETF MAX-STOP-TIME 10E20) */
#define MIN_START_TIME -10E20
/* LISP-SRC: (SETF MIN-START-TIME -10E20) */

/* conversion from float to integer */
#define SCALE_FACTOR_TO_BYTE 127
#define SCALE_FACTOR_TO_SHORT 32767
#define SCALE_FACTOR_TO_24BIT 0x7FFFFF
#define SCALE_FACTOR_TO_LONG 2147483647

/* Note that the values assigned here are not arbitrary, but represent
   a dominance relationship among the interpolation types.
*/
#define INTERP_n 0
#define INTERP_s 1
#define INTERP_i 2
#define INTERP_r 3

#define INTERP_nn 0
#define INTERP_ns 1
#define INTERP_ni 2
#define INTERP_nr 3
#define INTERP_sn 4
#define INTERP_ss 5
#define INTERP_si 6
#define INTERP_sr 7
#define INTERP_in 8
#define INTERP_is 9
#define INTERP_ii 10
#define INTERP_ir 11
#define INTERP_rn 12
#define INTERP_rs 13
#define INTERP_ri 14
#define INTERP_rr 15

#define INTERP_nnn 0
#define INTERP_nns 1
#define INTERP_nni 2
#define INTERP_nnr 3
#define INTERP_nsn 4
#define INTERP_nss 5
#define INTERP_nsi 6
#define INTERP_nsr 7
#define INTERP_nin 8
#define INTERP_nis 9
#define INTERP_nii 10
#define INTERP_nir 11
#define INTERP_nrn 12
#define INTERP_nrs 13
#define INTERP_nri 14
#define INTERP_nrr 15
#define INTERP_snn 16
#define INTERP_sns 17
#define INTERP_sni 18
#define INTERP_snr 19
#define INTERP_ssn 20
#define INTERP_sss 21
#define INTERP_ssi 22
#define INTERP_ssr 23
#define INTERP_sin 24
#define INTERP_sis 25
#define INTERP_sii 26
#define INTERP_sir 27
#define INTERP_srn 28
#define INTERP_srs 29
#define INTERP_sri 30
#define INTERP_srr 31

#define INTERP_inn 32
#define INTERP_ins 33
#define INTERP_ini 34
#define INTERP_inr 35
#define INTERP_isn 36
#define INTERP_iss 37
#define INTERP_isi 38
#define INTERP_isr 39
#define INTERP_iin 40
#define INTERP_iis 41
#define INTERP_iii 42
#define INTERP_iir 43
#define INTERP_irn 44
#define INTERP_irs 45
#define INTERP_iri 46
#define INTERP_irr 47
#define INTERP_rnn 48
#define INTERP_rns 49
#define INTERP_rni 50
#define INTERP_rnr 51
#define INTERP_rsn 52
#define INTERP_rss 53
#define INTERP_rsi 54
#define INTERP_rsr 55
#define INTERP_rin 56
#define INTERP_ris 57
#define INTERP_rii 58
#define INTERP_rir 59
#define INTERP_rrn 60
#define INTERP_rrs 61
#define INTERP_rri 62
#define INTERP_rrr 63

#define INTERP_nnnn 0
#define INTERP_nnns 1
#define INTERP_nnsn 4
#define INTERP_nnss 5
#define INTERP_nsnn 16
#define INTERP_nsns 17
#define INTERP_nssn 20
#define INTERP_nsss 21
#define INTERP_nrrr 63
#define INTERP_snnn 64
#define INTERP_snns 65
#define INTERP_snsn 68
#define INTERP_snss 69
#define INTERP_ssnn 80
#define INTERP_ssns 81
#define INTERP_sssn 84
#define INTERP_ssss 85
#define INTERP_niii 42
#define INTERP_siii 106
#define INTERP_srrr 127
#define INTERP_iiii 170
#define INTERP_rrrr 255

#define INTERP_nnnnnn 0
#define INTERP_nnnnns 1
#define INTERP_nnnnsn 4
#define INTERP_nnnnss 5
#define INTERP_nnnsnn 16
#define INTERP_nnnsns 17
#define INTERP_nnnssn 20
#define INTERP_nnnsss 21
#define INTERP_nnsnnn 64
#define INTERP_nnsnns 65
#define INTERP_nnsnsn 68
#define INTERP_nnsnss 69
#define INTERP_nnssnn 80
#define INTERP_nnssns 81
#define INTERP_nnsssn 84
#define INTERP_nnssss 85
#define INTERP_nsnnnn 256
#define INTERP_nsnnns 257
#define INTERP_nsnnsn 260
#define INTERP_nsnnss 261
#define INTERP_nsnsnn 272
#define INTERP_nsnsns 273
#define INTERP_nsnssn 276
#define INTERP_nsnsss 277
#define INTERP_nssnnn 320
#define INTERP_nssnns 321
#define INTERP_nssnsn 324
#define INTERP_nssnss 325
#define INTERP_nsssnn 336
#define INTERP_nsssns 337
#define INTERP_nssssn 340
#define INTERP_nsssss 341
#define INTERP_snnnnn 1024
#define INTERP_snnnns 1025
#define INTERP_snnnsn 1028
#define INTERP_snnnss 1029
#define INTERP_snnsnn 1040
#define INTERP_snnsns 1041
#define INTERP_snnssn 1044
#define INTERP_snnsss 1045
#define INTERP_snsnnn 1088
#define INTERP_snsnns 1089
#define INTERP_snsnsn 1092
#define INTERP_snsnss 1093
#define INTERP_snssnn 1104
#define INTERP_snssns 1105
#define INTERP_snsssn 1108
#define INTERP_snssss 1109
#define INTERP_ssnnnn 1280
#define INTERP_ssnnns 1281
#define INTERP_ssnnsn 1284
#define INTERP_ssnnss 1285
#define INTERP_ssnsnn 1296
#define INTERP_ssnsns 1297
#define INTERP_ssnssn 1300
#define INTERP_ssnsss 1301
#define INTERP_sssnnn 1344
#define INTERP_sssnns 1345
#define INTERP_sssnsn 1348
#define INTERP_sssnss 1349
#define INTERP_ssssnn 1360
#define INTERP_ssssns 1361
#define INTERP_sssssn 1364
#define INTERP_ssssss 1365
#define INTERP_iiiiii 2730
#define INTERP_rrrrrr 4095

#define INTERP_nnnnnnnn 0
#define INTERP_ssssssss 21845


#define INTERP_MASK 3
#define INTERP_SHIFT 2

LVAL snd_badsr(void);
int64_t check_terminate_cnt(int64_t tc);

typedef double time_type;
typedef double rate_type;
typedef float sample_type;
typedef double promoted_sample_type;

/* use radians or degrees for phase? */
#define ANGLEBASE 360.0

/* used by sndwrite.c for output buffers.  This should be
 * eliminated:
 */
#define MAX_SND_CHANNELS 24

#define max_table_len 100000000
/* Set to 4 for debugging block allocation stuff, 1012? for
   production
*/
/* leave a few words short of 1024 in case we allocate powers of 2 */
#define max_sample_block_len 1016
// #define max_sample_block_len 4

/* longest allowed sample is basically 2^31 but a bit lower to 
   allow for rounding */
#define MAX_SND_LEN (MAX_STOP - max_sample_block_len * 2)


/* Defines needed for xlisp */
#define getsound(x)     ((sound_type) getinst(x))
#define xlgasound()     (testarg(typearg(soundp)))

typedef short SFDataType, *SFDataPtr;

typedef sample_type  sample_block_values[max_sample_block_len], 
                    *sample_block_values_type;

typedef struct {
    long                refcnt;                          /* reference count */
    sample_block_values samples;
} sample_block_node, *sample_block_type;
 

/* forward declaration for circular type dependencies */
typedef struct snd_list_struct *snd_list_type;

struct snd_susp_struct;

typedef void  (*snd_fetch_fn)(struct snd_susp_struct *, snd_list_type snd_list);
typedef void  (*snd_free_fn)(struct snd_susp_struct *);
typedef void  (*snd_mark_fn)(struct snd_susp_struct *);  /* marks LVAL nodes for GC */
typedef void  (*snd_print_tree_fn)(struct snd_susp_struct *, int);

typedef struct snd_susp_struct {
    snd_fetch_fn fetch;
    void  (*keep_fetch)(struct snd_susp_struct *, snd_list_type snd_list);
    snd_free_fn free;
    snd_mark_fn mark;
    snd_print_tree_fn print_tree; /* debugging */
    char *name;        /* string name for debugging */
    int64_t toss_cnt;  /* return this many zeros, then compute */
    int64_t current;   /* current sample number */
    double sr;         /* sample rate */
    time_type t0;      /* starting time */
    int64_t log_stop_cnt; /* logical stop count */
    /* other susp dependent stuff will be here... */
} snd_susp_node, *snd_susp_type;


typedef struct snd_list_struct {
    sample_block_type   block;  /* pointer to block of samples */
    union {
        struct snd_list_struct  *next;
        snd_susp_type           susp;
    }       u;
    short   refcnt;
    short   block_len;
    boolean logically_stopped;
} snd_list_node; /* , *snd_list_type; -- defined above */

extern snd_list_type list_watch; //DBY


typedef struct table_struct {
    long refcount;  /* reference count */
    double length;  /* number of samples in table 
                       (double allows fractional length)*/
    sample_type samples[1]; /* arbitrary length array of sample */
} table_node, *table_type;


/* some counts are biased by -max_sample_block_len, so UNKNOWN can't be -1
 * Any number less than -max_sample_block should do
 */
#define UNKNOWN (-10-max_sample_block_len)

typedef struct sound_struct {
    sample_block_type (*get_next)(struct sound_struct *snd, int *cnt);
    time_type         time;    /* logical starting time */
    time_type         t0;      /* quantized time of first sample */
    int64_t           stop;    /* stop (clipping) sample no. */
    time_type         true_t0; /* exact time of first sample */
    rate_type         sr;      /* sample rate */
    int64_t           current; /* current sample number,
                                  if negative, then the first 
                                  -current samples must be dropped
                                  in order to find the first sample */
    int64_t           logical_stop_cnt; /* log stop sample no, -1=unknwn */
    snd_list_type     list;    /* sample block list, starting at curr. samp */
    sample_type       scale;   /* scale factor for the result */
    int64_t           prepend_cnt; /* how many zeros to prepend */
    /* function to use as get_next after prepended zeros are generated: */
    sample_block_type (*after_prepend)
                      (struct sound_struct * snd, int *cnt);
    table_type table; /* pointer to table-ized version of this sound */
    int64_t *extra;   /* used for extra state information, extra[0]
                              should be the length of the extra state 
                              (see sound_unref()) */
} sound_node, *sound_type;

/* convert number of samples to memory size: */
#define table_size_in_bytes(n) \
    (sizeof(table_node) + sizeof(sample_type) * ((n) - 1))

extern sample_block_type zero_block;
extern sample_block_type internal_zero_block;

extern snd_list_type zero_snd_list;

extern sound_type printing_this_sound;  /* debugging global */

int64_t snd_set_max_audio_mem(int64_t m);
/* LISP: (SND-SET-MAX-AUDIO-MEM FIXNUM) */

extern double sound_latency; /* controls output latency */
double snd_set_latency(double latency); 
/* LISP: (SND-SET-LATENCY FLONUM) */

double compute_phase(double phase, double key, long n, double srate,
                     double new_srate, double freq, double *incr_ptr);

boolean soundp(LVAL);
/* LISP: (SOUNDP ANY) */

void snd_list_ref(snd_list_type list);
void sound_unref(sound_type snd);
void snd_list_unref(snd_list_type list);

LVAL cvsound(sound_type);
extern LVAL a_sound;

sample_block_type SND_get_next(sound_type snd, int *cnt);
sample_block_type SND_get_first(sound_type snd, int *cnt);
sample_block_type SND_get_zeros(sound_type snd, int *cnt);
sample_block_type SND_flush(sound_type snd, int *cnt);

double hz_to_step(double);    /* LISP: (HZ-TO-STEP ANYNUM) */
int interp_style(sound_type s, rate_type sr);
void set_logical_stop_time(sound_type sound, time_type when); /* LISP: (SND-SET-LOGICAL-STOP SOUND ANYNUM) */

#define xlog(x) log(x)
/* LISP: double (LOG FLONUM) */
snd_list_type snd_list_create(snd_susp_type susp);
void snd_list_terminate(snd_list_type snd_list);
void snd_sort_2(sound_type * s1_ptr, sound_type * s2_ptr, rate_type sr);

double snd_sref(sound_type s, time_type t); 
    /* LISP: (SND-SREF SOUND ANYNUM) */

double snd_sref_inverse(sound_type s, double val);
    /* LISP: (SREF-INVERSE SOUND ANYNUM) */

double snd_stop_time(sound_type s); /* LISP: (SND-STOP-TIME SOUND) */
#define snd_time(s) (s)->time
    /* LISP: double (SND-TIME SOUND) */

#define snd_srate(s) (s)->sr
    /* LISP: double (SND-SRATE SOUND) */
#define snd_t0(s) (s)->t0
    /* LISP: double (SND-T0 SOUND) */

sound_type snd_xform(sound_type snd, rate_type sr, time_type time, 
        time_type start_time, time_type stop_time, promoted_sample_type scale);
    /* LISP: (SND-XFORM SOUND ANYNUM ANYNUM ANYNUM ANYNUM ANYNUM) */
sound_type sound_create(snd_susp_type susp, time_type t0, rate_type sr,
        promoted_sample_type scale);

void min_cnt(int64_t *cnt_ptr, sound_type sound, snd_susp_type susp, long cnt);
void indent(int n);
void sound_prepend_zeros(sound_type snd, time_type t0);



#ifndef GCBUG
#define blocks_to_watch_max 50
extern long blocks_to_watch_len;
extern sample_block_type blocks_to_watch[blocks_to_watch_max];

void block_watch(int64_t sample_block);
    /* LISP: (BLOCK-WATCH FIXNUM) */
int64_t sound_nth_block(sound_type snd, long n);
    /* LISP: (SOUND-NTH-BLOCK SOUND LONG) */
#endif

LVAL sound_array_copy(LVAL sa);

sound_type sound_copy(sound_type snd); 
    /* LISP: (SND-COPY SOUND) */
void sound_xlmark(void *a_sound);
void sound_print(LVAL snd_expr, long n);
    /* LISP: (SND-PRINT ANY LONG) */
int64_t sound_play(LVAL snd_expr);
    /* LISP: (SND-PLAY ANY) */
void stats(void);
    /* LISP: (STATS) */
void sound_print_tree(sound_type snd);
    /* LISP: (SND-PRINT-TREE SOUND) */
    
void mark_audio_time(void);

void sound_print_tree_1(sound_type snd, int n);

sound_type sound_scale(double factor, sound_type snd);
    /* LISP: (SND-SCALE ANYNUM SOUND) */
void sound_init(void);

void sound_symbols(void);

table_type sound_to_table(sound_type s);

void table_unref(table_type table);

sound_type sound_zero(time_type t0, rate_type sr);
    /* LISP: (SND-ZERO ANYNUM ANYNUM) */

#define sound_get_next(s, n) ((*(s->get_next))(s, n))

#define susp_print_tree(s, n) (*((s)->print_tree))(s, n)

double step_to_hz(double);
    /* LISP: (STEP-TO-HZ ANYNUM) */

/* macros for access to samples within a suspension */
/* NOTE: assume suspension structure is named "susp" */

/* susp_check_samples points sample_ptr to a new sample block if necessary */
#define susp_check_samples(sound, sample_ptr, sample_cnt) \
    if (susp->sample_cnt == 0) \
        susp_get_samples(sound, sample_ptr, sample_cnt)

/* susp_check_samples_break is similar to susp_check_samples - "_break"
 *   normally means that this code will break out of the inner loop, but in
 *   this case, there is no reason (neither log nor term) to break.
 *   x2_sample is taken from sound
 */
#define susp_check_samples_break(sound, sample_ptr, sample_cnt, x2_sample) \
    if (susp->sample_cnt == 0) { \
        susp_get_samples(sound, sample_ptr, sample_cnt); \
        x2_sample = susp_current_sample(sound, sample_ptr); }


/* susp_get_samples always gets next block (useful only in initialization code) */
#define susp_get_samples(sound, sample_ptr, sample_cnt) \
        susp->sample_ptr = sound_get_next(susp->sound, &(susp->sample_cnt))->samples

/* susp_get_block_samples always gets next block (useful only in initialization code) */
#define susp_get_block_samples(sound, sample_block_ptr, sample_ptr, sample_cnt) \
    susp->sample_block_ptr = sound_get_next(susp->sound, &susp->sample_cnt); \
    susp->sample_ptr = susp->sample_block_ptr->samples

/* susp_took is called after you've taken n samples */
#define susp_took(sample_cnt, n) susp->sample_cnt -= n

/* susp_fetch_sample is used to grab just one sample, doesn't check for samples!,
 *    but applies scale factor:  */
#define susp_fetch_sample(sound, sample_ptr, sample_cnt) \
          (susp->sound->scale * (susp->sample_cnt--, *(susp->sample_ptr++)))

/* susp_current_sample grabs sample without advancing to next, applies scale
 *     factor: */
#define susp_current_sample(sound, sample_ptr) \
          (susp->sound->scale * (*(susp->sample_ptr)))

/* susp_check_term_samples checks for samples; if new ones are fetched, then
 * run termination test on signal and record result.
 */
#define susp_check_term_samples(sound, sample_ptr, sample_cnt) \
    if (susp->sample_cnt == 0) { \
        susp_get_samples(sound, sample_ptr, sample_cnt); \
        terminate_test(sample_ptr, sound, susp->sample_cnt); }

/* susp_check_term_log_samples checks for samples
 * if new ones are fetched, then run termination test and logical stop
 * test on signal and record results.
 */
#define susp_check_term_log_samples(sound, sample_ptr, sample_cnt) \
    if (susp->sample_cnt == 0) { \
        susp_get_samples(sound, sample_ptr, sample_cnt); \
        logical_stop_test(sound, susp->sample_cnt); \
        terminate_test(sample_ptr, sound, susp->sample_cnt); }

/* susp_check_term_log_block_samples checks for samples
 * if new ones are fetched, then run termination test and logical stop
 * test on signal and record results.  In this case, termination and logical
 * stop happen at the MAXIMUM of termination and logical stop times, resp.
 *
 * Originally, this code assumed that logical stops occurred on block boundaries,
 * but because of the SET-LOGICAL-STOP function, which just writes a stop time
 * into the sound_struct, the logical stop can be anywhere. As soon as the 
 * logical stop is known, we want to propagate the value from the sound being
 * read into the sound being computed. The propagation should set the logical
 * stop of the computed sound to the MAX of any current value and the new 
 * value. When the bit fields indicate that all logical stop times have been
 * encountered, then the sound being computed will make the logical stop happen
 * on a block boundary and set the flag on the block of samples where the stop
 * occurs.
 */
#define susp_check_term_log_block_samples(sound, sample_block_ptr, sample_ptr, sample_cnt, bit, all) \
    if (susp->sample_cnt == 0) { \
        susp_get_block_samples(sound, sample_block_ptr, \
                               sample_ptr, sample_cnt); \
        if (susp->sound->logical_stop_cnt != UNKNOWN && \
            !(susp->logical_stop_bits & bit)) { \
            susp->logical_stop_bits |= bit; \
            susp->susp.log_stop_cnt = (int64_t) max(susp->susp.log_stop_cnt, \
                    (((susp->sound->logical_stop_cnt / \
                       susp->sound->sr + susp->sound->t0) - \
                      susp->susp.t0) * susp->susp.sr + 0.5)); } \
        if (susp->sample_ptr == zero_block->samples) { \
            susp->terminate_bits |= bit; \
            if (susp->terminate_bits == all) { \
                susp->terminate_cnt = ROUNDBIG( \
                  (((susp->sound->current - susp->sample_cnt) / \
                    susp->sound->sr + susp->sound->t0) - \
                   susp->susp.t0) * susp->susp.sr); \
    } } }


/* logical_stop_cnt_cvt is used to convert from the logical stop count
 * at one sample rate to that of another sample rate -- this macro is
 * used by the snd_make_<op> routine in every <op>.c file, and assumes
 * the target sample rate is susp->susp.sr.
 *
 * NOTE: this macro does not take into account the possibility of different
 * start times - maybe it should.
 */
#define logical_stop_cnt_cvt(sound) \
    (sound->logical_stop_cnt == UNKNOWN ? UNKNOWN : \
     ROUNDBIG((sound->logical_stop_cnt / sound->sr) * susp->susp.sr))


/* logical_stop_test tests to see if sound has logically stopped; if so,
 * sets susp->susp.log_stop_cnt.  The resulting logical_stop_cnt will reflect
 * the minimum logical_stop time of all sounds to which this test is applied.
 */
#define logical_stop_test(sound, cnt) \
    if (susp->sound->logical_stop_cnt == susp->sound->current - (cnt)) {\
        min_cnt(&susp->susp.log_stop_cnt, susp->sound, (snd_susp_type) susp, cnt); }

/* terminate_test checks to see if sound has terminated; if so, 
 * sets susp->terminate_cnt.  The resulting terminate_cnt will reflect
 * the minimum termination time of all sounds to which this test is applied.
 */
#define terminate_test(sample_ptr, sound, cnt) \
    if (susp->sample_ptr == zero_block->samples) { \
            min_cnt(&susp->terminate_cnt, susp->sound, (snd_susp_type) susp, cnt); }


/* susp_check_log_samples checks for new samples then checks for
 * termination and logical stop conditions
 */
#define susp_check_log_samples(sound, sample_ptr, sample_cnt) \
    if (susp->sample_cnt == 0) { \
      susp_get_samples(sound, sample_ptr, sample_cnt); \
      logical_stop_test(sound, susp->sample_cnt); }

/* susp_check_term_samples_break checks for new samples then checks for
 * termination condition; breaks from inner loop
 */
#define susp_check_term_samples_break( \
  sound, sample_ptr, sample_cnt, x2_sample) \
    if (susp->sample_cnt == 0) { \
      susp_get_samples(sound, sample_ptr, sample_cnt); \
      x2_sample = susp_current_sample(sound, sample_ptr); \
      terminate_test(sample_ptr, sound, susp->sample_cnt); \
      if (susp->terminate_cnt < susp->susp.current + cnt + togo) { \
          break; }} \
    else x2_sample = susp_current_sample(sound, sample_ptr); 

/* susp_check_log_samples_break checks for new samples then checks for
 * logical stop conditions; breaks from inner loop
 */
#define susp_check_log_samples_break( \
  sound, sample_ptr, sample_cnt, x2_sample) \
    if (susp->sample_cnt == 0) { \
      susp_get_samples(sound, sample_ptr, sample_cnt); \
      x2_sample = susp_current_sample(sound, sample_ptr); \
      logical_stop_test(sound, susp->sample_cnt); \
      if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN && \
          (susp->susp.log_stop_cnt < susp->susp.current + cnt + togo)) { \
          break; }} \
    else x2_sample = susp_current_sample(sound, sample_ptr);


/* susp_check_term_log_samples_break checks for new samples then checks for
 * termination and logical stop conditions; breaks from inner loop
 */
#define susp_check_term_log_samples_break( \
  sound, sample_ptr, sample_cnt, x2_sample) \
    if (susp->sample_cnt == 0) { \
      susp_get_samples(sound, sample_ptr, sample_cnt); \
      x2_sample = susp_current_sample(sound, sample_ptr); \
      terminate_test(sample_ptr, sound, susp->sample_cnt); \
      logical_stop_test(sound, susp->sample_cnt); \
      if ((susp->terminate_cnt != UNKNOWN && \
           susp->terminate_cnt < susp->susp.current + cnt + togo) || \
          (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN && \
           susp->susp.log_stop_cnt < susp->susp.current + cnt + togo)) { \
          break; }} \
    else x2_sample = susp_current_sample(sound, sample_ptr);
        

