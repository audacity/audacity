/* sound.c -- nyquist sound data type */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  changes for portability and fix compiler warnings
 */

/* define size_t: */
#include <limits.h>
#ifdef UNIX
#include "sys/types.h"
#endif  
#include <stdio.h>
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"
#include "samples.h"
#include "extern.h"
#include "debug.h"
#include "assert.h"
#ifdef OSC
#include "nyq-osc-server.h"
#endif
#include "cext.h"
#include "userio.h"

/* default maximum sample blocks: 
 * 1GB / (sample_block_len samples/block * 4 bytes/sample) */
long max_sample_blocks = 1000000000 / (max_sample_block_len * sizeof(float));

/* #define GC_DEBUG */
#ifdef GC_DEBUG
extern sound_type sound_to_watch;
#endif

snd_list_type list_watch; //DBY

/* #define SNAPSHOTS */

long table_memory;

sample_block_type zero_block;
sample_block_type internal_zero_block;

snd_list_type zero_snd_list;

xtype_desc sound_desc;
LVAL a_sound;
LVAL s_audio_markers;

static void sound_xlfree(void *);
static void sound_xlprint(LVAL, void *);
static void sound_xlsave(FILE *fp, void *s);
static unsigned char *sound_xlrestore(FILE *);

void sound_print_array(LVAL sa, long n);
void sound_print_sound(LVAL s_as_lval, long n);
void sample_block_unref(sample_block_type sam);

#ifdef SNAPSHOTS
boolean sound_created_flag = false;
#endif

#ifdef OSC
int nosc_enabled = false;
#endif

/* m is in bytes */
int64_t snd_set_max_audio_mem(int64_t m)
{
    int64_t r = max_sample_blocks;
	// avoid overflow since max_sample_blocks is long
	int64_t msb = m / (max_sample_block_len * sizeof(float));
	if (msb > LONG_MAX) msb = LONG_MAX;
    max_sample_blocks = (long) msb;
    return r * max_sample_block_len * sizeof(float);
}


double sound_latency = 0.3; /* default value */
/* these are used so get times for *AUDIO-MARKERS* */
double sound_srate = 44100.0;
int64_t sound_frames = 0;

double snd_set_latency(double latency)
{
    double r = sound_latency;
    sound_latency = latency;
    return r;
}


int64_t check_terminate_cnt(int64_t tc)
{
    if (tc < 0) {
        xlfail("duration is less than 0 samples");
        tc = 0; /* this should not be reached */
    }
    return tc;
}


/* xlbadsr - report a "bad combination of sample rates" error */
LVAL snd_badsr(void)
{
    xlfail("bad combination of sample rates");
    return NIL; /* never happens */
}


/* compute-phase -  given a phase in radians, a wavetable specified as
 *  the nominal pitch (in half steps), the table length, and the sample
 *  rate, compute the sample number corresponding to the phase.  This
 *  routine makes it easy to initialize the table pointer at the beginning
 *  of various oscillator implementations in Nyquist.  Note that the table
 *  may represent several periods, in which case phase 360 is not the same
 *  as 0.  Also note that the phase increment is also computed and returned
 *  through incr_ptr.
 */
double compute_phase(phase, key, n, srate, new_srate, freq, incr_ptr)
  double phase;  /* phase in degrees (depends on ANGLEBASE) */
  double key;    /* the semitone number of the table played at srate */
  long n;        /* number of samples */
  double srate;  /* the sample rate of the table */
  double new_srate;  /* sample rate of the result */
  double freq;   /* the desired frequency */
  double *incr_ptr; /* the sample increment */
{
    double period = 1.0 / step_to_hz(key);

    /* convert phase to sample units */
    phase = srate * period * (phase / (double) ANGLEBASE);
    /* phase is now in sample units; if phase is less than zero, then increase
       it by some number of sLength's to make it positive:
     */
    if (phase < 0)
        phase += (((int) ((-phase) / n)) + 1) * n;

    /* if phase is longer than the sample length, wrap it by subtracting the
       integer part of the division by sLength:
     */
    if (phase > n)
        phase -= ((int) (phase / n)) * n;

    /* Now figure the phase increment: to reproduce original pitch
       required incr = srate / new_srate.  To get the new frequency,
       scale by freq / nominal_freq = freq * period:
     */
    *incr_ptr = (srate / new_srate) * freq * period;
    return phase;
}
#ifndef GCBUG
snd_list_type gcbug_snd_list = 0;
long blocks_to_watch_len = 0;
sample_block_type blocks_to_watch[blocks_to_watch_max];

void block_watch(int64_t sample_block)
{
    if (blocks_to_watch_len >= blocks_to_watch_max) {
        stdputstr("block_watch - no more space to save pointers\n");
        return;
    }
    blocks_to_watch[blocks_to_watch_len++] = (sample_block_type) sample_block;
    nyquist_printf("block_watch - added %d = %x\n",
                   (int)sample_block, (int)sample_block);
}


/* fetch_zeros -- the fetch function for appended zeros */
/*
 * zeros are appended when the logical stop time exceeds the 
 * (physical) terminate time.  This fetch function is installed
 * by snd_list_terminate().  When appending zeros, we just return
 * a pointer to the internal_zero_block and increment current until
 * it reaches log_stop_cnt.  Then we call snd_list_terminate() to
 * finish off the sound list.
 */

void fetch_zeros(snd_susp_type susp, snd_list_type snd_list)
{
    int64_t len = MIN(susp->log_stop_cnt - susp->current,
                      max_sample_block_len);
/*    nyquist_printf("fetch_zeros, lsc %d current %d len %d\n", 
            susp->log_stop_cnt, susp->current, len); */
    if (len < 0) {
        char error[80];
        sprintf(error, "fetch_zeros susp %p (%s) len %" PRId64,
                susp, susp->name, len);
        xlabort(error);
    }
    if (len == 0) { /* we've reached the logical stop time */
        /* nyquist_printf("fetch_zeros: reached the logical stop in %s cnt %d\n",
               susp->name, susp->log_stop_cnt); */
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = (short) len;
        susp->current += len;
    }
}


/* sound_nth_block - fetch the address of the nth sample block of a sound */
/*
 * NOTE: intended to be called from lisp.  Lisp can then call block_watch
 * to keep an eye on the block.
 */
int64_t sound_nth_block(sound_type snd, long n)
{
    long i;
    snd_list_type snd_list = snd->list;
    for (i = 0; i < n; i++) {
        if (i == 1) {
            gcbug_snd_list = snd_list;
            nyquist_printf("gcbug_snd_list = 0x%p\n", gcbug_snd_list);
        }
        if (!snd_list->block) return 0;
        snd_list = snd_list->u.next;
    }
    if (snd_list->block) return (int64_t) snd_list->block;
    else return 0;
}

#endif


/****************************************************************************
*                               snd_list_create
* Inputs:
*       snd_susp_type susp: A reference to the suspension
* Result: snd_list_type
*       A newly-created sound list type
* Effect: 
*       Allocates and initializes a snd_list node:
*         block    refcnt  block_len susp  logically_stopped
*       +--------+--------+-------+-------+---+
*       |////////|   1    |   0   | susp  | F |
*       +--------+--------+-------+-------+---+
****************************************************************************/

/* snd_list_create -- alloc and initialize a snd_list node */
/**/
snd_list_type snd_list_create(snd_susp_type susp)
{
    snd_list_type snd_list;

    falloc_snd_list(snd_list, "snd_list_create");

    snd_list->block = NULL;             /* no block of samples */
    snd_list->u.susp = susp;            /* point to suspension */
    snd_list->refcnt = 1;               /* one ref */
    snd_list->block_len = 0;            /* no samples */
    snd_list->logically_stopped = false;/* not stopped */
/*    nyquist_printf("snd_list_create => %p\n", snd_list);*/
    return snd_list;
}


/****************************************************************************
*                                sound_create
* Inputs:
*       snd_susp_type susp: The suspension block to be used for this sound
*       time_type t0: The initial time for this sound
*       rate_type sr: The sampling rate for this sound
*       sample_type scale: The scaling factor for this sound
*       sample_block_type (*proc)(...): The get_next_sound method
* Result: sound_type
*       
* Effect: 
*       Creates and initializes a sound type
* Notes:
*       The MSDOS conditional is actually a test for ANSI headers; the
*       presence of float parameters means that an ANSI prototype and
*       a non-ANSI header are incompatible.  Better solution would be
*       to ANSIfy source.
****************************************************************************/

sound_type last_sound = NULL;

sound_type sound_create(
  snd_susp_type susp,
  time_type t0,
  rate_type sr,
  promoted_sample_type scale)
{
    sound_type sound;
    falloc_sound(sound, "sound_create");
    if (((intptr_t) sound) & 3) errputstr("sound not word aligned\n");
    last_sound = sound; /* debug */
    if (t0 < 0) xlfail("attempt to create a sound with negative starting time");
    /* nyquist_printf("sound_create %p gets %g\n", sound, t0); */
    sound->t0 = sound->true_t0 = sound->time = t0;
    sound->stop = MAX_STOP;
    sound->sr = sr;
    sound->current = 0;
    sound->scale = (float) scale;
    sound->list = snd_list_create(susp);
    sound->get_next = SND_get_first;
    sound->logical_stop_cnt = UNKNOWN;
    sound->table = NULL;
    sound->extra = NULL;
    /* nyquist_printf("sound_create susp %p snd_list %p\n", susp, sound->list);
       nyquist_printf("sound_create'd %p\n", sound); */
#ifdef SNAPSHOTS
    sound_created_flag = true;
#endif
#ifdef GC_DEBUG
    if (sound == sound_to_watch) {
        nyquist_printf("Created watched sound\n");
        watch_snd_list(sound->list);
    }
#endif
    return sound;
}


/* sound_prepend_zeros -- modify sound_type so that it starts at t0 */
/*
 * assumes t0 is earlier than snd->t0, so the sound should return zeros
 * until snd->t0 is reached, after which we revert to normal computation.
 * When we return, the new snd->t0 will be t0, meaning that the first
 * sample returned will be at time t0.
 * NOTE: t0 may not be an exact multiple of samples earlier than snd->t0,
 * but Nyquist allows any sound to be shifted by +/- 0.5 samples in 
 * order to achieve alignment.  Since sound_prepend_zeros can be called
 * many times on the same sound_type, there is a chance that rounding 
 * errors could accumulate.  My first solution was to return with 
 * snd->t0 computed exactly and not reflecting any fractional sample 
 * shift of the signal, but this caused problems for the caller: a 
 * fractional sample shift at a low sample rate could correspond to 
 * many client samples,fooling the client into thinking that some 
 * initial samples should be discarded (or else requiring the client
 * to be pretty smart).  The solution used here is to return to the
 * client with snd->t0 exactly equal to t0, but to save snd->true_t0
 * equal to the time of the first sample with no sound shifting.  This
 * time is used for any future sound_prepend_zeros operations so that
 * any accumulated rounding errors are due only to floating point 
 * precision and not to accumulated fractional sample shifts of snd.
 */
void sound_prepend_zeros(sound_type snd, time_type t0)
{
    int64_t n;

    /* first, see if we're already prepending some zeros */
    if (snd->get_next != SND_get_zeros) {
/*        nyquist_printf("sound_prepend_zeros 1: snd->t0 %g t0 %g\n", snd->t0,  t0); */

        /* if not, then initialize some fields that support prepending */
        snd->prepend_cnt = 0;
        snd->true_t0 = snd->t0;

        /* save old get_next and plug in special get_next function */
        snd->after_prepend = snd->get_next;
        snd->get_next = SND_get_zeros;
    }

    n = ROUNDBIG((snd->true_t0 - t0) * snd->sr); /* how many samples to prepend */

    /* add to prepend_cnt so first sample will correspond to new t0 */
    snd->prepend_cnt += n;
    /* compute the true t0 which corresponds to the time of first sample */
    snd->true_t0 -= (n / snd->sr);
    /* make caller happy by claiming the sound now starts at exactly t0;
     * this is always true within 0.5 samples as allowed by Nyquist. */
    snd->t0 = t0;
/*    nyquist_printf("sound_prepend_zeros: snd %p true_t0 %g sr %g n %d\n", 
           snd, snd->true_t0, snd->sr, n);*/
}


/* sound_array_copy -- copy an array of sounds */
/*
 * NOTE: be sure to protect the result from gc!
 */
LVAL sound_array_copy(LVAL sa)
{
    long i = getsize(sa);
    LVAL new_sa = newvector(i);
    xlprot1(new_sa);

    while (i > 0) {
        i--;
        setelement(new_sa, i, 
                   cvsound(sound_copy(getsound(getelement(sa, i)))));
    }

    xlpop();
    return new_sa;
}


/* sound_copy - copy a sound structure, do reference counts */
/**/
sound_type sound_copy(sound_type snd)
{
    sound_type sndcopy;
    falloc_sound(sndcopy, "sound_copy");
    *sndcopy = *snd;    /* copy the whole structure */
    sndcopy->extra = NULL; /* except for the (private) extra data */
    snd_list_ref(snd->list);    /* copied a reference so fix the count */
/*    nyquist_printf("sound_copy'd %p to %p\n", snd, sndcopy); */
    if (snd->table) snd->table->refcount++;
#ifdef GC_DEBUG
    if (sndcopy == sound_to_watch) 
		printf("sndcopy->table %x\n", sndcopy->table);
#endif
    return sndcopy;
}


/* convert a sound to a wavetable, set length */
/**/
table_type sound_to_table(sound_type s)
{
    long len = (long) snd_length(s, max_table_len);
    long tx = 0;        /* table index */
    int blocklen;
    register double scale_factor = s->scale;
    sound_type original_s = s;
    table_type table; /* the new table */
    long table_bytes; /* how big is the table */

    if (s->table) {
        s->table->refcount++;
        return s->table;
    }

    if (len >= max_table_len) {
        char emsg[100];
        sprintf(emsg, "maximum table size (%d) exceeded", max_table_len);
        xlcerror("use truncated sound for table", emsg, NIL);
    } else if (len == 0) {
        xlabort("table size must be greater than 0");
    }


    len++;      /* allocate extra sample at end of table */
    s = sound_copy(s);

    /* nyquist_printf("sound_to_table: allocating table of size %d\n", len); */
    table_bytes = table_size_in_bytes(len);
    table = (table_type) malloc(table_bytes);
    if (!table) xlfail("osc_init couldn't allocate memory for table");
    table_memory += table_bytes;

    table->length = (double) (len - 1);

    while (len > 1) {
        sample_block_type sampblock = sound_get_next(s, &blocklen);
        long togo = MIN(blocklen, len);
        long i;
        sample_block_values_type sbufp = sampblock->samples;
/*      nyquist_printf("in sound_to_table, sampblock = %d\n", sampblock);*/
        for (i = 0; i < togo; i++) {
            table->samples[tx++] = (float) (*sbufp++ * scale_factor);
        }
        len -= togo;
    }
    /* for interpolation, duplicate first sample at end of table */
    table->samples[tx] = table->samples[0];
    table->refcount = 2;    /* one for the user, one from original_s */

    sound_unref(s);
    s = NULL;
    original_s->table = table;
    return table;
}


void table_free(table_type table)
{
    long len = (long) (table->length) + 1;
    long bytes = table_size_in_bytes(len);
    free(table);
    table_memory -= bytes;
}


void table_unref(table_type table)
{
    if (!table) return;
    table->refcount--;
    if (table->refcount <= 0) {
        /* nyquist_printf("table refcount went to zero\n"); */
        table_free(table);
    }
}


void sound_unref(sound_type snd)
/* note that sounds do not have ref counts, so sound_unref
 * always frees the sound object
 */
{
    if (!snd) return;
    snd_list_unref(snd->list);
    table_unref(snd->table);
/*    nyquist_printf("\t\t\t\t\tfreeing sound@%p\n", snd);*/
    if (snd->extra) free(snd->extra);
    ffree_sound(snd, "sound_unref");
}


void snd_list_ref(snd_list_type list)
{
    list->refcnt++;
}


void snd_list_terminate(snd_list_type snd_list)
{
    snd_susp_type susp = snd_list->u.next->u.susp;
    int64_t lsc = susp->log_stop_cnt;
    int64_t current = susp->current;
    /* unreference the empty sample block that was allocated: */
    sample_block_unref(snd_list->block);
    /* use zero_block instead */
    snd_list->block = zero_block;
    /* either fetch more zeros or terminate now */
    if (lsc != UNKNOWN && lsc > current) {
        /* nyquist_printf("snd_list_terminate: lsc %d current %d\n", 
                lsc, current); */
        susp->fetch = fetch_zeros;
        fetch_zeros(susp, snd_list);
    } else {
        snd_list->block_len = max_sample_block_len;
        snd_list->logically_stopped = true;
        snd_list_unref(snd_list->u.next);
        snd_list->u.next = zero_snd_list;       /* be zero forever */
    }
}


void snd_list_unref(snd_list_type list)
{
    if (list == NULL) {
        nyquist_printf("why did snd_list_unref get %p?\n", list);
        return;
    }

    while (list && (list != zero_snd_list)) {
        snd_list_type next = NULL;

        list->refcnt--;
        if (list->refcnt != 0) {
            break; // the rest of the list is shared, nothing more to free
        }

        // list nodes either point to a block of samples or this is the 
        // last list node (list->block == NULL) which points to a suspension
        // lists can also terminate at the zero_block, which is an infinite
        //     shared list (zero_block->block == zero_block) of zero samples
        if (list->block && list->block != zero_block) {
            /* there is a next snd_list */
            next = list->u.next;
            sample_block_unref(list->block);
        } else if (list->block == NULL) { /* the next thing is the susp */
            /* free suspension structure */
            /* nyquist_printf("freeing susp@%p\n", list->u.susp); */
            (*(list->u.susp->free))(list->u.susp);
        }
        /* if (list == list_watch) 
               printf("freeing watched snd_list %p\n", list); */
        ffree_snd_list(list, "snd_list_unref");
        list = next;
    }
}


void sample_block_ref(sample_block_type sam)
{
    sam->refcnt++;
}


void sample_block_test(sample_block_type sam, char *s)
{
    /* see if this block is being watched */
    int i;
    for (i = 0; i < blocks_to_watch_len; i++) {
        if ((sam > (blocks_to_watch[i] - 1)) &&
            (sam < (blocks_to_watch[i] + 1))) {
            nyquist_printf(
    "WOOPS! %s(0x%p) refers to a block 0x%p on the watch list!\n",
                    s, sam, blocks_to_watch[i]);
        }
    }
}


void sample_block_unref(sample_block_type sam)
{
    sam->refcnt--;
    if (sam->refcnt == 0) {
#ifndef GCBUG
    sample_block_test(sam, "sample_block_unref");
#endif        
/*      nyquist_printf("freeing sample block %p\n", sam); */
        ffree_sample_block(sam, "sample_block_unref");
    }
}



/****************************************************************************
*                                interp_style
* Inputs:
*       sound_type s: The sound we are using
*       rate_type sr: The sampling rate
* Result: int
*       A small integer which is one of the symbolic values:
*       The values are ordered, smallest to largest, as
*               INTERP_n - none
*               INTERP_s - scale
*               INTERP_i - interpolated
*               INTERP_r - ramp
*
* Notes: 
*       The sampling rate s->sr and scale factor s->scale are compared
*       with other values exactly (no fuzz).  
****************************************************************************/

int interp_style(sound_type s, rate_type sr)
{
    if (s->sr == sr) 
       { /* same sample rate */
        return ((s->scale == 1.0) ? INTERP_n : INTERP_s);
       } /* same sample rate */
    else 
    if (s->sr * 10.0 > sr) 
       { /* 10x sample rate */
        return INTERP_i;
       } /* 10x sample rate */
    else 
       return INTERP_r;
}


/****************************************************************************
*                                 snd_sort_2
* Inputs:
*       sound_type * s1_ptr:
*       sound_type * s2_ptr:
*       rate_type sr:
* Result: void
*       
* Effect: 
*       If the interp_style of s1 dominates the interp_style of s2,
*       the sound_types input are interchanged.
****************************************************************************/

/* snd_sort_2 -- sort 2 arguments by interpolation method */
void snd_sort_2(sound_type *s1_ptr, sound_type *s2_ptr, rate_type sr)
{
    if (interp_style(*s1_ptr, sr) > interp_style(*s2_ptr, sr)) {
        sound_type s = *s1_ptr;
        *s1_ptr = *s2_ptr;
        *s2_ptr = s;
    }
}


/* snd_sref -- access a sound at a given time point */
/**/
double snd_sref(sound_type s, time_type t)
{
    double exact_cnt;      /* how many fractional samples to scan */
    int64_t cnt;               /* how many samples to flush */
    sample_block_type sampblock = NULL;
    int blocklen;
    sample_type x1, x2;    /* interpolate between these samples */

        /* changed true_t0 to just t0 based on comment that true_t0 is only
         * for use by snd_prepend_zeros -RBD
         */
    exact_cnt = (t - s->t0) * s->sr;
    if (exact_cnt < 0.0) return 0.0;

    s = sound_copy(s);     /* don't modify s, create new reader */
    cnt = (int64_t) exact_cnt;       /* rounds down */
    exact_cnt -= cnt;      /* remember fractional remainder */

    /* now flush cnt samples */
    while (cnt >= 0) {
        sampblock = sound_get_next(s, &blocklen);
        cnt -= blocklen;
        if (sampblock == zero_block) {
            sound_unref(s);
            return 0.0;
        }
    }
    /* -blocklen <= cnt <= -1 */

    /* get next 2 samples and interpolate */
    x1 = sampblock->samples[blocklen + cnt];
    if (cnt == -1) {
        sampblock = sound_get_next(s, &blocklen);
        cnt -= blocklen;
    }
    x2 = sampblock->samples[blocklen + cnt + 1];
    sound_unref(s);        /* free the reader */

    return (x1 + exact_cnt * (x2 - x1)) * s->scale;
}


/* snd_sref_inverse -- find time point corresponding to some value */
/**/
double snd_sref_inverse(sound_type s, double val)
{
    double exact_cnt;      /* how many fractional samples to scan */
    int i;
    sample_block_type sampblock;
    int blocklen;
    sample_type x1, x2;    /* interpolate between these samples */

    if (val < 0) {
        xlcerror("return 0", "negative value", cvflonum(val));
        return 0.0;
    }
    s = sound_copy(s);     /* don't modify s, create new reader */

    x1 = 0.0F;
    /* now flush cnt samples */
    while (true) {
        sampblock = sound_get_next(s, &blocklen);
        x2 = sampblock->samples[blocklen - 1];
        if (x2 >= val) break;
        x1 = x2;
        if (sampblock == zero_block) {
            xlcerror("return 0", "too large, no inverse", cvflonum(val));
            sound_unref(s);
            return 0.0;
        }
    }
    /* x1 = last sample of previous block,
       sampblock contains a value larger than val
       blocklen is the length of sampblock */

    /* search for first element exceeding val - could
     * use binary search, but maximum block size places
     * an upper bound on how bad this can get and we
     * search for the right block linearly anyway.
     */
    for (i = 0; i < blocklen && sampblock->samples[i] <= val; i++) ;

    /* now i is index of element exceeding val */
    if (i > 1) x1 = sampblock->samples[i - 1];
    x2 = sampblock->samples[i];

    /* now interpolate to get fractional part */
    if (x2 == x1) exact_cnt = 0;
    else exact_cnt = (val - x1) / (x2 - x1);

    /* and add the sample count of x1 */
    exact_cnt += (s->current - blocklen) + (i - 1);

    /* negative counts are possible because the first x1 is at
     * sample -1, so force the location to be at least 0
     */
    if (exact_cnt < 0) exact_cnt = 0;

    /* compute time = t0 + count / samplerate; */
    exact_cnt = s->t0 + exact_cnt / s->sr;

    sound_unref(s);        /* free the reader */
    return exact_cnt;
}


time_type snd_stop_time(sound_type s)
{
    if (s->stop == MAX_STOP) return MAX_STOP_TIME;
    /* I think placing the stop time 0.5 samples later than the last
       is to avoid rounding errors somewhere. Sounds are supposed
       to be open-ended on the right, and I would guess s->stop
       should be one greater than the actual number of samples.
       Therefore, it seems that 0.5 should be 0.0 so that 
       converting back to sample count will round to s->stop.
       I'm not changing this because it has been this way for
       a long time and Nyquist seems to get it right. -RBD */
    else return s->t0 + (s->stop + 0.5) / s->sr;
}


/* snd_xform -- return a sound with transformations applied */
/*
 * The "logical" sound starts at snd->time and runs until some
 * as yet unknown termination time.  (There is also a possibly
 * as yet unknown logical stop time that is irrelevant here.)
 * The sound is clipped (zero) until snd->t0 and after snd->stop,
 * the latter being a sample count, not a time_type.
 * So, the "physical" sound starts at snd->t0 and runs for up to
 * snd->stop samples (or less if the sound terminates beforehand).
 *
 * The snd_xform procedure operates at the "logical" level, shifting
 * the sound from its snd->time to time.  The sound is stretched as
 * a result of setting the sample rate to sr.  It is then (further) 
 * clipped between start_time and stop_time.  If initial samples
 * are clipped, the sound is shifted again so that it still starts
 * at time.  The sound is then scaled by scale.
 *
 * To support clipping of initial samples, the "physical" start time
 * t0 is set to when the first unclipped sample will be returned, but
 * the number of samples to clip is saved as a negative count.  The
 * fetch routine SND_flush is installed to flush the clipped samples
 * at the time of the first fetch.  SND_get_first is then installed
 * for future fetches.
 *
 * An empty (zero) sound will be returned if all samples are clipped.
 *
 */
sound_type snd_xform(sound_type snd,
                      rate_type sr,
                      time_type time,
                      time_type start_time,
                      time_type stop_time,
                      promoted_sample_type scale)
{
    int64_t start_cnt, stop_cnt; /* clipping samples (sample 0 at new t0) */

    /* start_cnt should reflect max of where the sound starts (t0)
     * and the new start_time.
     */
    if (start_time == MIN_START_TIME) {
        start_cnt = 0;
    } else {
        double new_start_cnt = ((start_time - time) * sr) + 0.5;
        start_cnt = ((new_start_cnt > 0) ? (int64_t) new_start_cnt : 0);
    }
    /* if (start_cnt < -(snd->current)) start_cnt = -(snd->current); */

    /* stop_cnt should reflect min of the new stop_time and the previous
     * snd->stop.
     */
    if (stop_time == MAX_STOP_TIME) {
        stop_cnt = MAX_STOP;
    } else {
        double new_stop_cnt = ((stop_time - time) * sr) + 0.5;
        if (new_stop_cnt < MAX_STOP) {
            stop_cnt = (int64_t) new_stop_cnt;
        } else {
            errputstr("Warning: stop count overflow in snd_xform\n");
            stop_cnt = MAX_STOP;
        }
    }

    if (stop_cnt > snd->stop) {
        stop_cnt = snd->stop;
    }

    if (stop_cnt < 0 || start_cnt >= stop_cnt) {
        snd = sound_create(NULL, time, sr, 1.0);
        /* sound_create goes ahead and allocates a snd_list node, so
         * we need to free it.  
         * Calling snd_list_unref here seems like the right thing, but 
         * it assumes too much structure is in place.  ffree_snd_list
         * is simpler and more direct:
         */
        ffree_snd_list(snd->list, "snd_xform");
        snd->list = zero_snd_list;
        /* nyquist_printf("snd_xform: (stop_time < t0 or start >= stop) "
                       "-> zero sound = %p\n", snd); */
    } else {
        snd = sound_copy(snd);
        snd->t0 = time;
        if (start_cnt) {
            snd->current -= start_cnt; /* indicate flush with negative num. */
            /* the following code assumes that SND_get_first is the
              routine to be called to get the first samples from this 
              sound.  We're going to replace it with SND_flush.  First,
              make sure that the assumption is correct:
            */
            if ((snd->get_next != SND_get_first) &&
                (snd->get_next != SND_flush)) {
                errputstr("snd_xform: SND_get_first expected\n");
                EXIT(1);
            }
            /* this will flush -current samples and revert to SND_get_first */
            snd->get_next = SND_flush;
            stop_cnt -= start_cnt;
        }
        snd->stop = stop_cnt;
        snd->sr = sr;
        snd->scale *= (float) scale;
    }
    return snd;
}


/* SND_flush -- the get_next function for flushing clipped samples */
/*
 * this only gets called once: it flushes -current samples (a 
 * non-real-time operation) and installs SND_get_next to return
 * blocks normally from then on.
 */
sample_block_type SND_flush(sound_type snd, int *cnt)
{
    int mycnt;
    sample_block_type block = SND_get_first(snd, &mycnt);
    /* changed from < to <= because we want to read at least the first sample */
    while (snd->current <= 0) {
        block = SND_get_next(snd, &mycnt);
    }
    /* at this point, we've read to and including the block with
     * the first samples we want to return.  If the block boundary
     * is in the right place, we can do a minimal fixup and return:
     */
    if (snd->current == snd->list->block_len) {
        *cnt = (int) snd->current; /* == snd->list->block_len */
        /* snd->get_next = SND_get_next; -- done by SND_get_first */
        return block;
    } else /* snd->current < snd->list->block_len */ {
        int64_t i;
        sample_block_values_type from_ptr;
        /* we have to return a partial block */
        /* NOTE: if we had been smart, we would have had SND_get_next
         * return a pointer to samples rather than a pointer to the
         * block, which has a reference count.  Since the caller
         * expects a pointer to a reference count, we have to copy
         * snd->current samples to a new block
         */
        snd_list_type snd_list = snd_list_create((snd_susp_type) snd->list->u.next);
        snd_list->u.next->refcnt++;
        falloc_sample_block(snd_list->block, "SND_flush");
        /* now copy samples */
        from_ptr = block->samples + snd->list->block_len - snd->current;
        for (i = 0; i < snd->current; i++) {
            snd_list->block->samples[i] = from_ptr[i];
        }
        snd_list_unref(snd->list);
        snd->list = snd_list;
        *cnt = (int) snd->current;
        return snd_list->block;
    }
}


/* SND_get_zeros -- the get_next function for prepended zeros */
/*
 * when prepending zeros, we just return a pointer to the internal_zero_block
 * and decrement the prepend_cnt until it goes to zero.  Then we revert to 
 * the normal (original) get_next function.
 *
 */
sample_block_type SND_get_zeros(sound_type snd, int *cnt)
{
    int64_t len = MIN(snd->prepend_cnt, max_sample_block_len);
    /* stdputstr("SND_get_zeros: "); */
    if (len < 0) {
        char error[80];
        sprintf(error, "SND_get_zeros snd %p len %" PRId64, snd, len);
        xlabort(error);
    }
    if (len == 0) { /* we've finished prepending zeros */
        snd->get_next = snd->after_prepend;
        /* stdputstr("done, calling sound_get_next\n"); fflush(stdout); */
        return sound_get_next(snd, cnt);
    } else {
        *cnt = (int) len;
        snd->current += len;
        snd->prepend_cnt -= len;
/*        nyquist_printf("returning internal_zero_block@%p\n", internal_zero_block);
        fflush(stdout); */
        return internal_zero_block;
    }
}


/****************************************************************************
*                                SND_get_next
* Inputs:
*       sound_type snd: The iterator whose next block is to be computed
*       int * cnt: Place to put count of samples returned
* Result: snd_list_type
*       Pointer to the sample block computed ---------------------------+
* Effect:                                                               |
*       force suspension to compute next block of samples               |
*                                                                       |
*  Here's the protocol for using this and related functions:            |
*  Every client (sample reader) has a private sound_type (an iterator), |
*  and the sound_type's 'list' field points to a header (of type        |
*  snd_list_type).  The header in turn points to a block of samples.    |
*                                                                       |
*                               +---------------------------------------+
*                               |
*                               |
*                               |            sample_block_type
*       (snd)                   V            +---+--+--+--+--+--+--+-...-+--+
*       sound_type:        snd_list_type +-->|ref|  |  |  |  |//|//|     |//|
*       +---------+        +----------+  |   +---+--+--+--+--+--+--+-...-+--+
*       | list    +------->| block    +--+                 ^
*       +---------+        +----------+                    :
*       |  t0     |        | block_len|....................:
*       +---------+        +----------+
*       |  sr     |        | refcnt   |
*       +---------+        +-+--------+
*       | current |        | next   +---->...         Note: the union u
*       +---------+        |u|........| snd_list_type    points to only one
*       | rate    |        | | susp   +---->...          of the indicated
*       +---------+        +-+--------+ susp_type        types
*       | scalse  |        |log_stop  |
*       +---------+        +----------+
*       | lsc     |
*       +---------+
*       |get_next +-----> SND_get_next()
*       +---------+
*
*  The sound_type keeps track of where the next sample block will 
*  come from.  The field 'current' is the number of the first sample of
*  the next block to be returned, where sample numbers start
*  at zero.  The normal fetch procedure is this one, although special
*  cases may generate special block generators, e.g., CONST does not need
*  to allocate and refill a block and can reuse the same block over and
*  over again, so it may have its own fetch procedure.  This is the
*  general fetch procedure, which assumes that the generator function
*  actually produces a slightly different value for each sample.
*
*  The distinguishing characteristic of whether the 'u' field is to be
*  interpreted as 'next', a link to the next list element, or 'susp', a
*  reference to the suspension for generating a new sample block, is
*  whether the 'block' parameter is NULL or not.  If it is NULL, then
*  u.susp tells how to generate the block; if it is not NULL, u.next is
*  a pointer to the next sound block in the list.
*
*  When the 'block' pointer is NULL, we create a block of samples, and
*  create a new sound list element which follows it which has a NULL
*  'block' pointer; the 'u' field of the current list element is now
*  interpreted as 'u.next'.
*
*      The client calls SND_get_next to get a pointer to a block of samples.
*      The count of samples generated is returned via a ref parameter, and
*      SND_get_next will not be called again until this set is exhausted.
*
*      The next time SND_get_next is called, it knows that the sample block
*      has been exhausted.  It releases its reference to the block (and if
*      that was the last reference, frees the block to the block allocation
*      pool), allocates a new block from the block pool, and proceeds to
*      fill it with samples.
*
*      Note that as an optimization, if the refcnt field goes to 0 it
*      could immediately re-use the block without freeing back to the block
*      pool and reallocating it.
*
*  Because of the way we handle sound sample blocks, the sound sample blocks
*  themselves are ref-counted, so freeing the snd_list_type may not free
*  the sample block it references.  At the level of this procedure, that
*  is transparently handled by the snd_list_unref function.
*
*  Logical stop:
*
*  Logical stop is handled by several mechanisms.  The /intrinsic/ logical
*  stop is an immutable property of the signal, and is determined by the
*  specification in the algorithm description file.  When it is encountered,
*  the 'logically_stopped' flag of the snd_list_node is set.
*  The generators guarantee that the first time this is encountered, it
*  will always be constructed so that the first sample of the block it
*  references is the logical stop time.
*
*  In addition, the client may have set the /explicit logical stop time/ of
*  the iterator (e.g., in nyquist, the (set-logical-stop sound time) call copies
*  the sound, altering its logical stop).  The logical stop time, when set
*  in this way, causes the logical_stop_cnt ('lsc' in the above diagram)
*  to be set to the count of the last sample to be generated before the
*  <logical stop time.  This will guarantee that the sound will indicate that
*  it has reached its logical stop time when the indicated sample is 
*  generated.
****************************************************************************/

/* for debugging */
void add_s1_s2_nn_fetch(snd_susp_type a_susp, snd_list_type snd_list);

/* SND_get_first -- the standard fn to get a block, after returning 
 *    the first block, plug in SND_get_next for successive blocks
 */
sample_block_type SND_get_first(sound_type snd, int *cnt)
{
    snd_list_type snd_list = snd->list;
    /*
     * If there is not a block of samples, we need to generate one.
     */
    if (snd_list->block == NULL) {
        /*
         * Call the 'fetch' method for this sound_type to generate 
         * a new block of samples.
         */
        snd_susp_type susp = snd_list->u.susp;

        snd_list->u.next = snd_list_create(susp);
        snd_list->block = internal_zero_block;
        /* nyquist_printf("SND_get_first: susp->fetch %p\n",
                susp->fetch); */
        assert(susp->log_stop_cnt == UNKNOWN || susp->log_stop_cnt >= 0);
        (*(susp->fetch))(susp, snd_list);
#ifdef GC_DEBUG
        snd_list_debug(snd_list, "SND_get_first");
#endif
        /* nyquist_printf("SND_get_first: snd_list %p, block %p, length %d\n",
               snd_list, snd_list->block, snd_list->block_len); */
    }
    if ((snd->logical_stop_cnt == UNKNOWN) && snd_list->logically_stopped) {
        /* nyquist_printf("SND_get_first/next: snd %p logically stopped at %d\n",
                snd, snd->current); */
        snd->logical_stop_cnt = snd->current;
    }

    /* see if clipping needs to be applied */
    if (snd->current + snd_list->block_len > snd->stop) {
        /* need to clip: is clip on a block boundary? */
        if (snd->current == snd->stop) {
            /* block boundary: replace with zero sound */
            snd->list = zero_snd_list;
            snd_list_unref(snd_list);
        // the idea here is that we have reached snd->stop, which
        // means the next samples have to be zero, but we are reading
        // from the middle of a block of samples. Maybe, for example,
        // snd was constructed by snd_xform that imposed a new stop
        // time. Since we haven't read the next sample, we can take
        // care of this by just creating a new snd_list with a shorter
        // block_len to take whatever samples we need before stop, then
        // link ot zero_snd_list so that subsequent samples are zero.
        // However, if we actually start reading zeros from zero_snd_list,
        // the test above for > snd->stop will bring us back here. We
        // ignore these cases just below by testing if the current list
        // is the zero_snd_list. If so, we're just reading zeros, we're
        // past the stop time, and we can just keep reading zeros, so
        // do nothing.
        } else if (snd->list != zero_snd_list) {
            /* not a block boundary: build new list */
            snd->list = snd_list_create((snd_susp_type) zero_snd_list);
            snd->list->block_len = (short) (snd->stop - snd->current);
            snd->list->block = snd_list->block;
            snd->list->block->refcnt++;
            snd_list_unref(snd_list);
        }
        snd_list = snd->list; /* used below to return block ptr */
    }

    *cnt = snd_list->block_len;
    assert(snd_list->block_len >= 0);
    /* this should never happen */
    if (*cnt == 0) {
        stdputstr("SND_get_first returned 0 samples\n");
#if DEBUG_MEM
        dbg_mem_print("snd_list info:", snd_list);
        dbg_mem_print("block info:", snd_list->block);
#endif
        sound_print_tree(snd);
        stdputstr("It is possible that you created a recursive sound\n");
        stdputstr("using something like: (SETF X (SEQ (SOUND X) ...))\n");
        stdputstr("Nyquist aborts from non-recoverable error\n");
        abort();
    }
    snd->current += snd_list->block_len;    /* count how many we read */
    snd->get_next = SND_get_next;
    return snd_list->block;
}


sample_block_type SND_get_next(sound_type snd, int *cnt)
{
    snd_list_type snd_list = snd->list;
    /*
     * SND_get_next is installed by SND_get_first, so we know
     * when we are called that we are done with the current block
     * of samples, so free it now.
     */
    snd_list_type cur = snd_list;
    snd->list = snd_list = cur->u.next;
    snd_list_ref(snd_list);
    snd_list_unref(cur);  /* release the reference to the current block */

    /* now that we've deallocated, we can use SND_get_first to finish the job */
    return SND_get_first(snd, cnt);
}



/****************************************************************************
*                               make_zero_block
* Inputs:
*       
* Result: 
*       
* Effect: 
*       
****************************************************************************/

sample_block_type make_zero_block(void)
    {
     sample_block_type zb;
     int i;

     falloc_sample_block(zb, "make_zero_block");
     /* leave room for lots more references before overflow, 
        but set the count high so that even a large number of
        dereferences will not lead to a deallocation */
     zb->refcnt = 0x6FFFFFFF;

     for (i = 0; i < max_sample_block_len; i++) 
        { /* fill with zeros */
         zb->samples[i] = 0.0F;
        } /* fill with zeros */
     return zb;
    }


/* min_cnt -- help compute the logical stop or terminate as minimum */
/*
 * take the sound (which has just logically stopped or terminated at
 * current sample) and
 * convert the stop sample into the equivalent sample count as produced by
 * susp (which may have a different sample rate).  If the count is less than
 * the current *cnt_ptr, overwrite cnt_ptr with a new minimum.  By calling
 * this when each of S1, S2, ... Sn reach their logical stop or termiate
 * points, *cnt_ptr will end up with the minimum stop count, which is what
 * we want.  NOTE: the logical stop time and terminate for signal addition 
 * should be the MAX of logical stop times of arguments, so this routine 
 * would not be used.
 */
void min_cnt(int64_t *cnt_ptr, sound_type sound, snd_susp_type susp, long cnt)
{
    int64_t c = ROUNDBIG((((sound->current - cnt) / sound->sr + sound->t0) - susp->t0) *
      susp->sr);
    /* if *cnt_ptr is uninitialized, just plug in c, otherwise compute min */
    if ((*cnt_ptr == UNKNOWN) || (*cnt_ptr > c)) {
/*        nyquist_printf("min_cnt %p: new count is %d\n", susp, c);*/
/*        if (c == 0) sound_print_tree(printing_this_sound);*/
        *cnt_ptr = c;
    }
}



/****************************************************************************
*                                 sound_init
* Result: void
*       
* Effect: 
*       Module initialization
*       Allocates the 'zero block', the infinitely linked block of
*       0-valued sounds.  This is referenced by a list element which
*       refers to itself.
****************************************************************************/

void sound_init(void)
{
    zero_block = make_zero_block();
    internal_zero_block = make_zero_block();

    falloc_snd_list(zero_snd_list, "sound_init");

    zero_snd_list->block = zero_block;
    zero_snd_list->u.next = zero_snd_list;
    zero_snd_list->refcnt = 2;
    zero_snd_list->block_len = max_sample_block_len;
    zero_snd_list->logically_stopped = true;
#ifdef GC_DEBUG
    { long s;
      stdputstr("sound_to_watch: ");
      scanf("%p", &s);
      watch_sound((sound_type) s);
    }
#endif
   sound_desc = create_desc("SOUND", sound_xlfree, sound_xlprint,
                            sound_xlsave, sound_xlrestore, sound_xlmark);
}


/* sound_scale -- copy and change scale factor of a sound */
/**/
sound_type sound_scale(double factor, sound_type snd)
{
    sound_type sndcopy = sound_copy(snd);
    sndcopy->scale *= (float) factor;
    return sndcopy;
}




/****************************************************************************
*                            set_logical_stop_time
* Inputs:
*       sound_type sound: The sound for which the logical stop time is
*                         being set
*       time_type  when:  The logical stop time, expressed as an absolute
*                         time.
* Result: void
*       
* Effect: 
*       Converts the time 'when' into a count of samples.
****************************************************************************/

void set_logical_stop_time(sound_type sound, time_type when)
{
    /*
       'when' is an absolute time.  The number of samples to
       be generated is the number of samples between 't0' and
       'when'.

       -----------+---+---+---+---+---+---+---+---+---+
                  |                                |
                  t0                               when
     */
    int64_t n = ROUNDBIG((when - sound->t0) * sound->sr);
    if (n < 0) {
        xlcerror("retain the current logical stop", 
                 "logical stop sample count is negative", NIL);
    } else {
        sound->logical_stop_cnt = n;
    }
}




/* for debugging
sound_type printing_this_sound = NULL;
void ((**watch_me)()) = NULL;


void set_watch(where)
  void ((**where)());
{
    if (watch_me == NULL) {
        watch_me = where;
        nyquist_printf("set_watch: watch_me = %p\n", watch_me);
    }
}
*/

/*
 * additional routines
 */

/* snd_list_len - for debugging: how many sample blocks held? */
long snd_list_len(void *inst)
{
    int i = 0;
    sound_type snd = (sound_type) inst;
    snd_list_type list = snd->list;
    while (list->block && list->block != zero_block && list->block_len != 0) {
        i++;
        list = list->u.next;
    }
    return i;
}


/* sound_print - implement SND-PRINT, based on sound_save in sndwritepa.c */
/**/
void sound_print(snd_expr, n)
  LVAL snd_expr;
  long n;
{
    LVAL result;

    result = xleval(snd_expr);
    /* BE CAREFUL - DO NOT ALLOW GC TO RUN WHILE RESULT IS UNPROTECTED */
    if (vectorp(result)) {
        /* make sure all elements are of type a_sound */
        long i = getsize(result);
        while (i > 0) {
            i--;
            if (!exttypep(getelement(result, i), a_sound)) {
                xlerror("SND-PRINT: array has non-sound element",
                        result);
            }
        }
        sound_print_array(result, n);
    } else if (soundp(result)) {
        sound_print_sound(result, n);
    } else {
        xlprot1(result);
        xlerror("sound_print: expression did not return a sound",
                 result);
        xlpop();
    }
}


/* sound_print_sound - implements SND-PRINT for mono signal */
/**/
void sound_print_sound(LVAL s_as_lval, long n)
{
    int ntotal = 0;
    sound_type s;
    int blocklen;
    sample_block_type sampblock;

    /* for debugging 
    printing_this_sound = s; 
    */

    xlprot1(s_as_lval);
    s = sound_copy(getsound(s_as_lval));
    s_as_lval = cvsound(s); /* destroys our reference to original */

    nyquist_printf("SND-PRINT: start at time %g\n", s->t0);

    while (ntotal < n) {
        if (s->logical_stop_cnt != UNKNOWN) {
            nyquist_printf("logical stop time (in samples): %d ", 
                           (int)s->logical_stop_cnt);
        }
        sound_print_tree(s);
        sampblock = sound_get_next(s, &blocklen);
        if (sampblock == zero_block || blocklen == 0) {
            break;
        }
        print_sample_block_type("SND-PRINT", sampblock,
                                MIN(blocklen, n - ntotal));
        ntotal += blocklen;
    }
    nyquist_printf("total samples: %d\n", ntotal);
    xlpop();
}


void sound_print_array(LVAL sa, long n)
{
    int blocklen;
    long i, chans;
    LVAL sa_copy;
    long upper = 0;
    sample_block_type sampblock;
    time_type t0, tmax;

    chans = getsize(sa);
    if (chans == 0) {
        stdputstr("SND-PRINT: 0 channels!\n");
        return;
    }

    xlprot1(sa);
    sa_copy = newvector(chans);
    xlprot1(sa_copy);

    /* To be non-destructive, copy sounds from sa to sa_copy. */
    for (i = 0; i < chans; i++) {
        sound_type s = getsound(getelement(sa, i));
        setelement(sa_copy, i, cvsound(sound_copy(s)));
    }
    /* If sa and sounds in sa are not accessible, we do not want to retain
     * them because they will accumulate the computed samples.
     */
    sa = sa_copy; /* destroy original reference to (maybe) allow GC */

    /* take care of prepending zeros if necessary */
    t0 = tmax = (getsound(getelement(sa, 0)))->t0;
    for (i = 1; i < chans; i++) {
        sound_type s = getsound(getelement(sa, i));
        t0 = MIN(s->t0, t0);
        tmax = MAX(s->t0, tmax);
    }

    /* if necessary, prepend zeros */
    if (t0 != tmax) {
        stdputstr("prepending zeros to channels: ");
        for (i = 0; i < chans; i++) {
            sound_type s = getsound(getelement(sa, i));
            if (t0 < s->t0) {
                nyquist_printf(" %d ", (int)i);
                sound_prepend_zeros(s, t0);
            }
        }
        stdputstr("\n");
    }

    nyquist_printf("SND-PRINT: start at time %g\n", t0);

    while (upper < n) {
        int i;
        boolean done = true;
        for (i = 0; i < chans; i++) {
            sound_type s = getsound(getelement(sa, i));
            int64_t current = -1;  /* always get first block */
            while (current < upper) {
                sampblock = sound_get_next(s, &blocklen);
                if (sampblock != zero_block && blocklen != 0) {
                    done = false;
                }
                current = s->current - blocklen;
                nyquist_printf("chan %d current %d:\n", i, (int)current);
                 print_sample_block_type("SND-PRINT", sampblock,
                                         (int) MIN(blocklen, n - current));
                current = s->current;
                upper = (long) MAX(upper, current);
            }
        }
        if (done) break;
    }
    nyquist_printf("total: %d samples x %d channels\n",
                   (int)upper, (int)chans);
    xlpopn(2); // sa and sa_copy
}


/* sound_play -- compute sound, do not retain samples */
/*
 * NOTE: we want the capability of computing a sound without
 * retaining samples.  This requires that no references to
 * the sound exist, but if the sound is passed as an argument,
 * the argument stack will have a reference.  So, we pass in
 * an expression that evaluates to the sound we want.  The
 * expression is eval'd, the result copied (in case the
 * expression was a sound or a global variable and we really
 * want to preserve the sound), and then GC will
 * get rid of the original if there really are no other 
 * references.
 */

int64_t sound_play(snd_expr)
  LVAL snd_expr;
{
    int64_t ntotal;
    int blocklen;
    sample_block_type sampblock;
    LVAL result;
    sound_type s;

    xlsave1(result);
    result = xleval(snd_expr);
    if (!exttypep(result, a_sound)) {
        xlerror("SND-PLAY: expression did not return a sound",
                 result);
    }

    ntotal = 0;
    /* if snd_expr was simply a symbol, then result now points to
        a shared sound_node.  If we read samples from it, then
        the sound bound to the symbol will be destroyed, so
        copy it first.  If snd_expr was a real expression that
        computed a new value, then the next garbage collection
        will reclaim the sound_node.
    */
    s = sound_copy(getsound(result));
    result = cvsound(s);

    while (1) {
#ifdef OSC
        if (nosc_enabled) nosc_poll();
#endif
        sampblock = sound_get_next(s, &blocklen);
        if (sampblock == zero_block || blocklen == 0) {
            break;
        }
        /* print_sample_block_type("sound_play", sampblock, blocklen); */
        ntotal += blocklen;
    }
    nyquist_printf("total samples: %" PRId64 "\n", ntotal);
    xlpop();
    return ntotal;
}


/* sound_print_tree -- print a tree version of sound structure */
/**/
void sound_print_tree(snd)
  sound_type snd;
{
/*    nyquist_printf("sample_block_free %p\n", sample_block_free);*/
    nyquist_printf("SOUND PRINT TREE of %p\n", snd);
    sound_print_tree_1(snd, 0);
}


void indent(int n)
{
    while (n-- > 0) stdputstr(" ");
}


void sound_print_tree_1(snd, n)
  sound_type snd;
  int n;
{
    int i;
    snd_list_type snd_list;
    if (n > 100) {
        stdputstr("... (skipping remainder of sound)\n");
        return;
    }
    if (!snd) {
        stdputstr("\n");
        return;
    }
    nyquist_printf("sound_type@%p(%s@%p)t0 "
                   "%g stop %d sr %g lsc %d scale %g pc %d",
                   snd, 
                   (snd->get_next == SND_get_next ? "SND_get_next" :
                    (snd->get_next == SND_get_first ? "SND_get_first" : "?")),
                   snd->get_next, snd->t0, (int)snd->stop, snd->sr, 
                   (int)snd->logical_stop_cnt, snd->scale,
                   (int)snd->prepend_cnt);
    snd_list = snd->list;
    nyquist_printf("->snd_list@%p", snd_list);
    if (snd_list == zero_snd_list) {
        stdputstr(" = zero_snd_list\n");
        return;
    }
    for (i = 0; ; i++) {
        if (snd_list == zero_snd_list) {
            if (i > 1) nyquist_printf(" (skipping %d) ", i-1);
            stdputstr("->zero_snd_list\n");
            return;
        }
        if (!snd_list->block) {
            if (i > 0) nyquist_printf(" (skipping %d) ", i);
            stdputstr("->\n");
            indent(n + 2);

            nyquist_printf("susp@%p(%s)toss_cnt %d "
                           "current %d lsc %d sr %g t0 %g %p\n",
                           snd_list->u.susp, snd_list->u.susp->name,
                           (int)snd_list->u.susp->toss_cnt,
                           (int)snd_list->u.susp->current,
                           (int)snd_list->u.susp->log_stop_cnt,
                           snd_list->u.susp->sr,
                           snd_list->u.susp->t0, snd_list);
            susp_print_tree(snd_list->u.susp, n + 4);
            return;
        }
        snd_list = snd_list->u.next;
    }
}


/* mark_audio_time -- record the current playback time
 *
 * The global variable *audio-markers* is treated as a list.
 * When the user types ^Q, this function pushes the current
 * playback time onto the list
 */
void mark_audio_time()
{
    double playback_time = sound_frames / sound_srate - sound_latency;
    LVAL time_node = cvflonum(playback_time);
    setvalue(s_audio_markers, cons(time_node, getvalue(s_audio_markers)));
    gprintf(TRANS, " %g ", playback_time); 
    fflush(stdout);
}


/* compute constants p1 and p2:
  pitchconvert(0) * 2 = pitchconvert(12)  - octaves
          exp(p2) * 2 = exp(12 * p1 + p2)
                    2 = exp(12 * p1)
               log(2) = 12 * p1

         p1 = log(2.0)/12;

  pitchconvert(69) gives 440Hz
          exp(69 * p1 + p2) = 440
               69 * p1 + p2 = log(440)

        p2 = log(440.0) - (69 * p1);
*/

#define p1 0.0577622650466621
#define p2 2.1011784386926213


double hz_to_step(double hz)
{
    return (log(hz) - p2) / p1;
}


double step_to_hz(double steps)
{
    return exp(steps * p1 + p2);
}


/*
 * from old stuff...
 */

static void sound_xlfree(void *s)
{
    sound_unref((sound_type)s);
}


static void sound_xlprint(LVAL fptr, void *s)
{
        /* the type cast from s to LVAL is OK because
         * putatm does not dereference the 3rd parameter */
    putatm(fptr, "Sound", (LVAL) s);
}


static void sound_xlsave(FILE *fp, void *s)
{
    stdputstr("sound_save called\n");
}


static unsigned char *sound_xlrestore(FILE *fp)
{
   stdputstr("sound_restore called\n");
   return NULL;
}


/* sound_xlmark -- mark LVAL nodes reachable from this sound */
/**/
void sound_xlmark(void *a_sound)
{
    sound_type s = (sound_type) a_sound;
    snd_list_type snd_list;
    long counter = 0;
#ifdef TRACESNDGC
    nyquist_printf("sound_xlmark(%p)\n", s);
#endif
    if (!s) return; /* pointers to sounds are sometimes NULL */
    snd_list = s->list;
    while (snd_list->block != NULL) {
        if (snd_list == zero_snd_list) {
#ifdef TRACESNDGC
            stdputstr(" terminates at zero_snd_list\n");
#endif
            return;
        } else if (counter > max_sample_blocks) {
            /* exceded maximum length of sound in memory */
        } else if (counter > 1000000) {
           stdputstr("You created a recursive sound! This is a Nyquist bug.\n");
           stdputstr("The only known way to do this is by a SETF on a\n");
           stdputstr("local variable or parameter that is being passed to\n");
           stdputstr("SEQ or SEQREP. The garbage collector assumes that\n");
           stdputstr("sounds are not recursive or circular, and follows\n");
           stdputstr("sounds to their end. After following 1M nodes,\n");
           stdputstr("I'm pretty sure that there is a\n");
           stdputstr("cycle here, but since this is a bug, I cannot promise\n");
           stdputstr("to recover. Prepare to crash. If you cannot locate\n");
           stdputstr("the cause of this, contact the author -RBD.\n");
        }
        snd_list = snd_list->u.next;
        counter++;
    }
    if (snd_list->u.susp->mark) {
#ifdef TRACESNDGC
        nyquist_printf(" found susp (%s) at %p with mark method\n",
               snd_list->u.susp->name, snd_list->u.susp);
#endif
        (*(snd_list->u.susp->mark))(snd_list->u.susp);
    } else {
#ifdef TRACESNDGC
        nyquist_printf(" no mark method on susp %p (%s)\n",
               snd_list->u.susp, snd_list->u.susp->name);
#endif
    }
}


void sound_symbols()
{
   a_sound = xlenter("SOUND");
   s_audio_markers = xlenter("*AUDIO-MARKERS*");
   setvalue(s_audio_markers, NIL);
}


/* The SOUND Type: */


boolean soundp(LVAL s)
{
   return (exttypep(s, a_sound));
}


/* sound_zero - create and return a zero that terminates now */
/**/
sound_type sound_zero(time_type t0,rate_type sr)
{
    sound_type sound;
    falloc_sound(sound, "sound_zero");

    sound->get_next = SND_get_first;
    sound->list = zero_snd_list;
    sound->logical_stop_cnt = sound->current = 0;
    sound->true_t0 = sound->t0 = sound->time = t0;
    sound->stop = MAX_STOP;
    sound->sr = sr;
    sound->scale = 1.0F;
    sound->table = NULL;
    sound->extra = NULL;

    return sound;
}


LVAL cvsound(sound_type s)
{
/*   nyquist_printf("cvsound(%p)\n", s);*/
   return (cvextern(sound_desc, (unsigned char *) s));
}

