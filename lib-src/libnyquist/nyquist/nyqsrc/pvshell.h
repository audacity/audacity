/* pvshell.h -- a generic Nyquist primitive, esp. for phase vocoder */

/* how many bytes to provide for miscellaneous state info */
#define PVSHELL_STATE_MAX 256

/* define some bits to return conditions */
#define PVSHELL_FLAG_TERMINATE 4
#define PVSHELL_FLAG_LOGICAL_STOP 8

/* this function is called to compute samples. It should compute n
 * samples (floats == sample_type) and store them at out[i]. 
 * You can return less than n samples by writing the actual number
 * of samples computed into *n. Normally, you return zero.
 * To indicate that the time of the FIRST sample is the logical stop 
 * time, return PVSHELL_FLAG_LOGICAL_STOP. (If the logical stop time
 * is not at the first sample, but instead at sample j, then just
 * return j samples (from 0 to j-1), save the rest of the samples, 
 * and the next time, the first sample will correspond to the logical
 * stop time, so you can return PVSHELL_FLAG_LOGICAL_STOP.
 * To indicate that the sound has terminated, return 
 * PVSHELL_FLAG_TERMINATE. This should be the only time you return
 * zero samples. (As with logical stop time, if you have samples to
 * return before termination, then do it, and return 
 * PVSHELL_FLAG_TERMINATE the next time you are called, at which
 * point you've returned all the samples, so you can set *n = 0.
 */
struct pvshell_struct;

typedef long (*h_fn_type)(struct pvshell_struct *susp, 
                          sample_block_values_type out, long *n,
                          int64_t sample_count);
typedef void (*pvs_free_fn_type)(struct pvshell_struct *susp);

typedef struct pvshell_struct {
    sound_type f;
    int f_cnt;
    sample_block_values_type f_ptr;

    sound_type g;
    int g_cnt;
    sample_block_values_type g_ptr;

    long flags; /* for terminated and logically stopped flags */

    // state is extra storage for whatever you like
    char state[PVSHELL_STATE_MAX];
    
    // h is a function that computes sound from f, g, x, y, state
    h_fn_type h;
    pvs_free_fn_type free_fn;
} pvshell_node, *pvshell_type;    


/* to get samples from f or g, use these macros. For each sample, call
 * PVSHELL_TEST_X to get logical stop and terminate flags (but do not
 * fetch a sample). Then, if you want, call PVSHELL_FETCH_X to get the
 * next sample. You can call PVSHELL_TEST_X multiple times before
 * calling PVSHELL_FETCH_X, e.g. you can return exit a loop when you
 * see a logical stop flag and later call PVSHELL_TEST_X again. You
 * CANNOT call PVSHELL_FETCH_X multiples times without an intervening
 * call to PVSHELL_TEST_X. Finally, the logical stop flag is only
 * returned once. Normally you should write something like:
 *    new_flags = PVSHELL_TEST_F(susp);
 *    susp->flags | = new_flags; // remember flags
 *    if (new_flags) break;
 * in the sample loop so that you will break when you see logical_stop.
 * Outside the loop, you can return (*n ? 0 : susp->flags) which will
 * return 0 if you computed samples before the logical stop was detected.
 * Then the next time you are called, you will return the logical_stop
 * flag because you saved it in susp->flags, and the flag applies to the
 * *beginning* of the sample block. This code handles terminate too.
 */
long pvshell_test_f(pvshell_type susp);
long pvshell_test_g(pvshell_type susp);
#define PVSHELL_TEST_F(susp) ((susp)->f_cnt == 0 ? pvshell_test_f(susp) : 0)
#define PVSHELL_FETCH_F(susp) \
    ((susp)->f->scale * ((susp)->f_cnt--, *((susp)->f_ptr++)))

#define PVSHELL_TEST_G(susp) ((susp)->g_cnt == 0 ? pvshell_test_g(susp) : 0)
#define PVSHELL_FETCH_G(susp) \
    ((susp)->g->scale * ((susp)->g_cnt--, *((susp)->g_ptr++)))

/* snd_make_pvshell -- create an instance of pvshell.
   name -- string name of the operation, for debugging & printing
           (name is not copied. It must be a permanent, immutable string.)
   sr -- sample rate of output sound
   t0 -- start time of output sound
   h -- function that computes samples of output
   f -- first input sound, e.g. sound to be time-stretched
   g -- second input sound, e.g. sound to control varying stretch factor
   state -- initial state information needed by h. Declared as void * so
        that any struct can be passed in (provided the size is less than
        PVSHELL_STATE_MAX bytes)
   n -- number of bytes in state (<= PVSHELL_STATE_MAX)
*/
sound_type snd_make_pvshell(char *name, rate_type sr, time_type t0,
                            h_fn_type h, pvs_free_fn_type free_fn,
                            sound_type f, sound_type g, 
                            void *state, long n);

