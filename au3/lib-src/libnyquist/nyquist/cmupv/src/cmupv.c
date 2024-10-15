/*  cmupv.c -- phase vocoder */

/* Computation is driven by demands for output. The client calls either
pv_get_output() or pv_get_output2(). Either way, output is returned
one blocksize at a time. The blocksize is set by pv_set_blocksize()
and defaults to the synthesis hopsize, which defaults to fftsize/8.

Since the blocksize and hopsize are not necessarily matched in any
way, there is a buffer to accumulate the overlapping "grains" of sound
which we call the synthesis frames (each synthesis frame is computed
by adjusting phases of an analysis frame). The buffer is called
output_buffer, and the length (in floats) is output_buffer_len.

The output_buffer_len has to be big enough to contain blocksize samples
which are about to be returned as output plus fftsize samples which 
overlap with future synthesis frames that will be added later. The 
output_buffer_len also has to be hopsize + fftsize samples (probably
these worst-case sizes are conservative, but it's easier to set 
workable upper bounds than to think about all the special cases when
output_buffer_len, blocksize, hopsize, and fftsize are all arbitrary).

The basic structure to produce blocksize samples is as follows:
out_next is the pointer to the next block of blocksize samples. This
is a pointer directly into output_buffer, initially the first sample
in output_buffer.

We also keep a pointer frame_next, which is where the next synthesis
frame will be added into output_buffer. Thus, the number of samples
that have been completely computed but not output (i.e. no more 
overlapping frames will be added) is frame_next - out_next. When
(frame_next - out_next) > blocksize, we can deliver blocksize samples
to the caller.

The next constraint is that as we add overlapping synthesis frames into
output_buffer, we have to have room for them, so frame_next + fftsize
must not be greater than output_buffer + output_buffer_len. If this 
constraint is not met, we need to shift everything toward the beginning
of the output_buffer. Since we've output everything up to out_next, we
shift by out_next - output_buffer. 

Because output_buffer_len >= blocksize + fftsize, we are guaranteed that
there is always room to add in any synthesis frames that overlap the
blocksize samples to be returned next.

PHASE COHERENCE

To preserve phase relationships when a sine spills over multiple bins
(which happens always due to windowing), we adjust phase based on peaks
in the magnitude spectrum. The phases of neighboring bins are adjusted
by the same angle.

Specification: We want to divide the spectrum into peaks: between local
minima, adjust according to the peak.

Algorithm: Set "previous" minimum to -1 and previous magnitude to 0
- search for the "previous" peak
- iteratively do the following:
    - search for the next minimum
    - search for the next peak
    - compute range of bins to modify, assign minimum to the largest peak
    - compute the phase adjustment for the next peak
    - apply the phase adjustment to range of bins
    - set previous minumum to next minimum
    - set previous peak to next peak
*/

/* BEGIN PV_SINE_TEST debugging code...
    This debugging code is for testing a special case: the input is a sine tone
with amplitude about 1.0 and frequency about 689Hz at 44100Hz sample rate, 
resulting in a period of exactly 64 samples, which will be bin 8 with an
fftsize of 512.
    The goal here is to compute exactly what the phasevocoder should be doing
so we can compare to what it does. When SINETEST is enabled, data will be
printed. At the time this code is being written, the result with a stretch
factor of exactly 1.1 is a pulsing sound when the absolute interface is used.
With the absolute interface, the analysis hopsize is mostly 58, with a hop of
59 samples every 5 or 6 frames.
*/ 
// #define PV_SINE_TEST 1
#ifdef PV_SINE_TEST
double pvst_frequency = 44100.0 / 64.0; // about 689Hz
long pvst_offset = -1000; // -1 means we haven't seen first frame yet
// the first hop size is bogus, so we ignore it, then accumulate hop sizes here
#endif
/* END PV_SINE_TEST */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include "internal.h"
// only needed for some debugging code which is probably commented out
#include "cmupvdbg.h"

// define PHASE_FIX to tie neighboring bin phase to phase of spectral peaks
// #define PHASE_FIX 1

// debugging output on or off
// #define D if (1)
#define D if (0)

// more debugging
#define DD if (0)

#define LOGFFTSIZE_DEFAULT 11
#define RATIO_DEFAULT 1
#define BOOL int
#define FALSE 0
#define TRUE 1
#define TWOPI (2.0 * M_PI)

// These macros are used for memory allocation and freeing. Note that
// only ZERO acts like a function. The rest only work on fields of pv.
#define PVFREE(field) if (pv->field) { pv->free(pv->field); pv->field = NULL; }
#define PVALLOC(field, size) \
        pv->field = (float *)pv->malloc((size) * sizeof(*(pv->field)))
#define PVREALLOC(field, size) { PVFREE(field); PVALLOC(field, size); }
#define ZERO(array, size) memset((array), 0, (size) * sizeof(*(array)))


#include "cmupv.h"
#include "fftext.h"

typedef enum {
    PV_UNINITIALIZED,
    PV_START,
    PV_GOT_COUNT,
    PV_GOT_INPUT } pv_phase_type;

struct position  // each element in the structure array
{
    int64_t ana_pos; // the sample number of the center of analysis frame
    int64_t syn_pos; // the sample number of the center of synthesis frame
};

typedef struct {
    void *(*malloc)(size_t); // malloc is used to allocate memory. If you 
        // have a real-time system and want to avoid the standard library
        // malloc() which may have priority inversion problems due to locks,
        // you can supply your own lock-free implementation
    void (*free)(void *); // if you provide a custom malloc, you should
        // provide a matching custom free()
    int blocksize; // the size of audio blocks produced by the phase vocoder.
    int fftsize; // the number of samples in each FFT. Should be power of 2.
    int log2_fft; // 2 is the log base
    int syn_hopsize; // the hopsize used in reconstructing the output
    float ratio; // the time-stretch ratio. NOTE: even though ratio is 
        // specified as the amount input is stretched, THIS ratio is
        // the reciprocal, i.e. input duration / output duration 
    int max_ana_hopsize; // fftsize / 3 is max hopsize
    float pre_ratio; // previous ratio is used to calculate
        // the previous input_length
    int mode; // 0 - normal, 1 - phase fix, 2 - robovoice
    float *ana_win; // the window function used on input (analysis)
    float *syn_win; // the window function used on output (synthesis)
    int64_t input_eff_pos; // input effective position is the sample number
        // of the input that corresponds to the current output
    float *input_buffer; // used to buffer input samples
    long input_buffer_len; // how many floats can input_buffer hold?
    float *output_buffer; // used to buffer output samples
    long output_buffer_len; // how big in floats is the output buffer?
    float *input_head; // pointer to the start of the previous 
        // analysis frame. input_head is updated by
        // hopsize before reading each frame.
    float *input_rear; // pointer to the end of the input data
    int frames_to_compute; // how many frames we'll compute in pv_get_output()
    int expected_input; // the value computed by pv_get_input_count()
        // we check that pv_put_input delivers what we asked for
    float *out_next; // pointer into the output buffer from where the
                     // next output sample will delivered
    float *frame_next; // pointer into the output buffer to where the
                       // next frame will by added
    Pv_callback callback; // function to retrieve an input frame
    void *rock; // object pointer or context info to be passed to callback
    pv_phase_type phase;
    BOOL first_time; // true only on the first fft output frame
    BOOL absolute; // true if using the callback protocol -- set by create2()
    float *ana_frame; // analysis frame
    float *syn_frame; // synthesis frame
    float *mag; // magnitude for points in the frame being processed
    float *ana_phase; // phase for points in the analysis frame
                      // being processed
    float *syn_phase; // phase for points in the synthesis frame
                      // being processed
    float *pre_ana_phase; // recording last analysis phase for estimating
                          // the frequency
    float *pre_syn_phase; // recording last systhesis phase for rebuilting
                          // the new phase
    float *bin_freq; // bin frequency, used in phase unwrapping;
    
    struct position *pos_buffer; // Circular array storing the sample
                            // number of the middle of the frames
                            // (both for analysis and synthesis frames)
    struct position *pos_buffer_head; // beginning of the circular array,
        // points to oldest entry. If equal to pos_buffer_rear, the queue
        // is empty. Never points to 
        // pos_buffer + queue_length. It wraps around to pos_buffer.
    struct position *pos_buffer_rear; // rear of the circular array,
        // points to slot AFTER the most recent entry. If equal to 
        // pos_buffer_rear, the queue is empty. Never points to 
        // pos_buffer + queue_length. It wraps around to pos_buffer.
    long queue_length; // length of the circular queue of corresponding times
    int64_t input_total; // how many input samples did we get so far?
        // initially 0, and incremented upon pv_put_input()
        // so the input_total count corresponds to the input_rear pointer.
    int64_t output_total; // how many output samples did we produce so far?
        // initially 0, and incremented by block_size after each call to
        // pv_get_output(). Corresponds to out_next.
} PV;

//extern long int sig;
//extern long int sig1;
//extern long int sig2;

// round_log_power - round fftsize up to a power of 2
//    return log2(rounded up fftsize)
//    optionally set size_ptr to rounded up fftsize
//
int round_log_power(int fftsize, int *size_ptr)
{
    double log2_fft = log2l(fftsize);
    int round_log2_fft = (int) log2_fft;
    if (round_log2_fft < log2_fft) {
        round_log2_fft += 1;
    }
    if (log2_fft > 20 || ((1 << round_log2_fft) != fftsize)) {
        round_log2_fft = 1024; // on error, substitute a sane value
    }
    if (size_ptr) *size_ptr = 1 << round_log2_fft;
    return round_log2_fft;
}


Phase_vocoder pv_create(void *(*mallocfn)(size_t), void (*freefn)(void *))
{
    if (!mallocfn) mallocfn = &malloc;
    PV *pv = (PV *)mallocfn(sizeof(PV));
    ZERO(pv, 1);
    pv->phase = PV_UNINITIALIZED;
    pv->malloc = mallocfn;
    pv->free = freefn;
    pv_set_fftsize(pv, 1 << LOGFFTSIZE_DEFAULT);
    // syn_hopsize will now be FFTSIZE_DEFAULT / 8
    // pv_set_syn_hopsize(pv, FFTSIZE_DEFAULT / 8);
    pv->blocksize = pv->syn_hopsize;
    pv_set_ratio(pv, RATIO_DEFAULT);
    pv->first_time = TRUE;
    pv->mode = 0;
    return (Phase_vocoder)pv;
}


Phase_vocoder pv_create2(void *(*mallocfn)(size_t), void (*freefn)(void *), 
                         Pv_callback callback, void *rock)
{
    PV *pv = (PV *)pv_create(mallocfn, freefn);
    pv->absolute = TRUE;
    pv_set_callback(pv, callback, rock);
    return (Phase_vocoder)pv;
}


void pv_end(Phase_vocoder *x)
{
    PV *pv = (PV *)(*x);
    fftFree();
    PVFREE(ana_win);
    PVFREE(syn_win);
    PVFREE(input_buffer);
    PVFREE(output_buffer);
    PVFREE(ana_frame);
    PVFREE(syn_frame);
    PVFREE(mag);
    PVFREE(ana_phase);
    PVFREE(syn_phase);
    PVFREE(pre_ana_phase);
    PVFREE(pre_syn_phase);
    PVFREE(bin_freq);
    PVFREE(pos_buffer);
    pv->free(pv);
    *x = NULL;
}

void pv_set_callback(Phase_vocoder x, Pv_callback callback, void *rock)
{
    PV *pv = (PV *)x;
    pv->callback = callback;
    pv->rock = rock;
}

void pv_set_blocksize(Phase_vocoder x, int n)
{
    PV *pv = (PV *)x;
    pv->blocksize = n;
    pv->phase = PV_UNINITIALIZED;
}

void pv_set_fftsize(Phase_vocoder x, int n)
{
    PV *pv = (PV *)x;
    // n must be power of 2 and for sanity, we'll require it to be at least 16
    // power of 2 test: only if n is a power of 2 will n-1 clear the high order
    // bit:
    if ((n & (n - 1)) || (n < 16)) {
        return; // ignore bad argument
    }
    // preserve the same syn_hopsize ratio, e.g. if syn_hopsize
    // is fftsize/8, then after setting fftsize, new syn_hopsize
    // will be (new fftsize)/8
    int hop = (pv->syn_hopsize == 0 ? 8 :
               pv->fftsize / pv->syn_hopsize); // the divisor
    pv->fftsize = n;
    pv->log2_fft = round_log_power(n, &(pv->fftsize));
    pv_set_syn_hopsize(x, n / hop);
    pv->phase = PV_UNINITIALIZED;
    pv->max_ana_hopsize = n / 3;
}

void pv_set_ratio(Phase_vocoder x, float ratio)
{
    PV *pv = (PV *)x;
    assert(pv->phase == PV_START || pv->phase == PV_UNINITIALIZED);
    pv->pre_ratio = pv->ratio;
    pv->ratio = 1.0F / ratio;
}

void pv_set_syn_hopsize(Phase_vocoder x, int n)
    // set the hopsize. Must be fftsize divided by a power of 2.
    // non-power-of-two n will be rounded up.
    // hopsize must be at least 1 and at most fftsize/4.
    // out-of-bound n will be put within bounds
{
    PV *pv = (PV *)x;
    if (n < 1) n = 1;
    round_log_power(n, &(pv->syn_hopsize));
    if (pv->syn_hopsize > pv->fftsize / 4) {
        pv->syn_hopsize = pv->fftsize / 4;
    }
    pv->phase = PV_UNINITIALIZED;
}

void pv_initialize(Phase_vocoder x)
{
    PV *pv = (PV *)x;
    
    // allocate space and initialize for window
    if (! pv->ana_win)
        pv->ana_win = pv_window(pv, hann); //default analysis window is Hanning
    if (! pv->syn_win)
        pv->syn_win = pv_window(pv, hann); //default synthesis window is Hanning
    
    // allocate space and initialize for input buffer and output buffer
    if (pv->blocksize <= pv->syn_hopsize) {
        pv->input_buffer_len = pv->fftsize;
    } else {
        // The maximum of ana_hopsize is fftsize/3, so the 
        //    pv->input_buffer_len is set to the maximum so as to avoid 
        //    freeing and allocating memory for input buffer many times 
        //    due to the changing of time-stretching ratio.
        // The maximum amount needed is fftsize to produce one frame of 
        //    output plus: fftsize/3 for each additional frame. The total
        //    number of frames we must compute to form blocksize samples 
        //    of output is blocksize / hopsize.
        pv->input_buffer_len = pv->fftsize + 2 /* to avoid rounding error */ +
            lroundf((((float) pv->blocksize / pv->syn_hopsize) - 1) * (pv->fftsize / 3.0F));
    }
    if (! pv->absolute) {
        PVREALLOC(input_buffer, pv->input_buffer_len);
        // preload with fftsize/2 zeros so that the first sample of the input
        // will be at the center of the fft window
        pv->input_head = pv->input_buffer;
        ZERO(pv->input_buffer, pv->fftsize / 2);
        pv->input_rear = pv->input_buffer + pv->fftsize / 2;
    }
    
    // how long does the output buffer need to be?
    // It has to be long enough to add in an entire fft frame, so
    // at least fftsize. It should then be at least syn_hopsize - 2
    // bigger, so that if we're outputting syn_hopsize - 1 samples
    // (a really bad choice, by the way), we could have syn_hopsize - 2
    // samples in the output buffer and the next buffer gets added
    // starting at location syn_hopsize - 2, so we need syn_hopsize - 2 +
    // fftsize. Let's make it an even syn_hopsize + fftsize.
    // It also has to be long enough to hold an output buffer length + 
    // fftsize.
    // So overall, we need max(syn_hopsize, blocksize) + fftsize.
    //
    pv->output_buffer_len = pv->blocksize;
    if (pv->blocksize <= pv->syn_hopsize) {
        pv->output_buffer_len = pv->syn_hopsize;
    }
    pv->output_buffer_len += pv->fftsize;
    PVREALLOC(output_buffer, pv->output_buffer_len);
    D printf("pv_initialize: input_buffer_len %ld\n", pv->input_buffer_len);
    D printf("               output_buffer_len %ld\n", pv->output_buffer_len);
    D printf("               blocksize %d\n", pv->blocksize);
    D printf("               fftsize %d\n", pv->fftsize);
    D printf("               syn_hopsize %d\n", pv->syn_hopsize);
    pv->out_next = pv->output_buffer;
    pv->frame_next = pv->output_buffer;
    ZERO(pv->output_buffer, pv->output_buffer_len);
    PVREALLOC(ana_frame, pv->fftsize);
    PVREALLOC(syn_frame, pv->fftsize);
    PVREALLOC(mag, pv->fftsize / 2 + 1);
    // allocate space for phase and pre_phase which will be used in 
    // phase unwrapping
    PVREALLOC(ana_phase, pv->fftsize / 2 + 1);
    PVREALLOC(syn_phase, pv->fftsize / 2 + 1);
    PVREALLOC(pre_ana_phase, pv->fftsize / 2 + 1);
    PVREALLOC(pre_syn_phase, pv->fftsize / 2 + 1);
    // bin frequency, used in phase unwrapping
    PVREALLOC(bin_freq, pv->fftsize / 2 + 1);
    int i;
    for (i = 0; i <= pv->fftsize / 2; i++)
        pv->bin_freq[i] = (float) (TWOPI * i / pv->fftsize);
    // get_effective_pos() maps from the beginning of the
    // next block to the corresponding input sample. Since
    // the output hopsize is fixed, the next output sample
    // was at the center of the frame if we go back by
    // framesize / syn_hopsize frames. Thus we need 
    // framesize / syn_hopsize entries in the queue. We'll
    // pad by a couple to deal with rounding issues:
    pv->queue_length = pv->fftsize / (pv->syn_hopsize * 2) + 2;
    if (!pv->absolute) {
        PVFREE(pos_buffer);
        pv->pos_buffer = (struct position *)
            (pv->malloc((pv->queue_length + 1) * sizeof(struct position)));
    
        pv->pos_buffer_head = pv->pos_buffer;
        pv->pos_buffer_rear = pv->pos_buffer;
    }
    // make sure tables are constructed before we start real-time processing
#ifndef NDEBUG
    int fft_error_sign =
#endif
        fftInit(pv->log2_fft); // target fftInit
    assert(!fft_error_sign);

    pv->phase = PV_START;
}


void pv_set_ana_window(Phase_vocoder x, float *window)
{
    PV *pv = (PV*)x;
    PVREALLOC(ana_win, pv->fftsize);
    memcpy(pv->ana_win, window, pv->fftsize * sizeof(float));
}


void pv_set_syn_window(Phase_vocoder x, float *window)
{
    PV *pv = (PV*)x;
    PVREALLOC(syn_win, pv->fftsize);
    memcpy(pv->syn_win, window, pv->fftsize * sizeof(float));
}


void pv_set_mode(Phase_vocoder x, int mode)
{
    PV *pv = (PV*)x;
    if (mode >= 0 && mode <= 2) {
        pv->mode = mode;
    }
}


float *pv_window(Phase_vocoder x, float (*window_type)(double x)) 
    // window is after normalized
{
    PV *pv = (PV *)x;
    float sum_window_square = 0, COLA_factor;
    int window_length = pv->fftsize;
    float *window = (float *)pv->malloc(window_length * sizeof(float));
    int i;
    for (i = 0; i < window_length; i++) {
        window[i] = window_type((double)i / window_length);
        // note that the computation is all double even if window[i] is float
        sum_window_square += window[i] * window[i];
    }
    COLA_factor = sum_window_square / pv->syn_hopsize;
    for (i = 0; i <= pv->fftsize - 1; i++)
        window[i] = (float) (window[i] / sqrt(COLA_factor));
    return window;
}


int pv_get_input_count(Phase_vocoder x)
{
    PV *pv = (PV*)x;
    int ana_hopsize = lroundf((pv->syn_hopsize) * (pv->ratio));
    if (ana_hopsize > pv->max_ana_hopsize) {
        ana_hopsize = pv->max_ana_hopsize;
    }

    assert(pv->phase == PV_START);
    
    // To produce blocksize, how many samples do we need? The next
    // sample to output is at out_next, and the next frame will be
    // added at frame_next, so we've already computed
    // out_next - frame_next:
    int need = pv->blocksize - (int)(pv->frame_next - pv->out_next);
    // need is now the number of output samples we need
    // How many fft frames will be required? Round up by adding hopsize-1:
    int frames = (need + pv->syn_hopsize - 1) / pv->syn_hopsize;
    if (frames > 0) {
        // Skip hopsize frames except on the first time, where
        // we always put first sample in the middle of the frame:
        if (!pv->first_time) {
            pv->input_head += ana_hopsize;
        }
        // We need framesize + hopsize * (frames - 1) in order to compute
        // frames overlapping fft frames of size framesize.
        need = pv->fftsize + ana_hopsize * (frames - 1);
        // Now how many input samples do we have already?
        long have = (long) (pv->input_rear - pv->input_head);
        need -= have;
        // See if we have room for need samples in the buffer:
        if (pv->input_rear + need > pv->input_buffer + pv->input_buffer_len) {
            // not enough room. Shift the input buffer to make space.
            long shift = (long) (pv->input_head - pv->input_buffer);
            memmove(pv->input_buffer, pv->input_head,
                   (pv->input_rear - pv->input_head) * 
                   sizeof(*(pv->input_buffer)));
            pv->input_head -= shift;
            pv->input_rear -= shift;
            D printf("    after input shift by %ld, head at %ld\n",
                     shift, (long) (pv->input_head - pv->input_buffer));
        }
        // make sure our assumptions are true and we now have space:
        assert(pv->input_rear + need <= 
               pv->input_buffer + pv->input_buffer_len);
        // See if we have room in the output_buffer
        // last sample will be at frame_next + (frames - 1) * hopsize + fftsize
        float *last_output = pv->frame_next +
                (frames - 1) * pv->syn_hopsize + pv->fftsize;
        if (last_output > pv->output_buffer + pv->output_buffer_len) {
            // not enough room. Shift the output buffer to make space.
            long shift = (long) (pv->out_next - pv->output_buffer);
            memmove(pv->output_buffer, pv->out_next,
                   (pv->fftsize - pv->syn_hopsize) * 
                   sizeof(*(pv->output_buffer)));
            pv->frame_next -= shift;
            pv->out_next -= shift;
        }
    } else {
        frames = 0;
        need = 0;
    }
    pv->frames_to_compute = frames;
    pv->phase = PV_GOT_COUNT;
    pv->expected_input = need;
    return need;
}

#pragma warning(disable: 4715 4068) // return type and unknown pragma
#pragma clang diagnostic ignored "-Wreturn-type"
double pv_get_effective_pos(Phase_vocoder x)
{
    PV *pv = (PV*)x;
    assert(pv->phase == PV_START);
    
    // Find the appropriate position struct for the computation of 
    // effective audio position. We are given pv->output_total, the
    // sample count at which we want the equivalent input sample
    // count. We want to interpolate between two queue entries, so
    // we'll search the queue until we have an entry that is greater
    // than output_total. Then we set head to the previous entry 
    // because it will make future searches go faster.
    
    struct position *pos_find = pv->pos_buffer_head;
    struct position *pos_find_prev = NULL;
    while (pos_find != pv->pos_buffer_rear &&
           pos_find->syn_pos <= pv->output_total) {
        pos_find_prev = pos_find;
        pos_find++;
        if (pos_find == pv->pos_buffer + pv->queue_length) {
            pos_find = pv->pos_buffer; // wrap
        }
    }
    // if pos_find and pos_find_prev both point to something, we
    // can interpolate:
    if (pos_find != pv->pos_buffer_rear && pos_find_prev) {
        // we can drop old positions from queue now:
        pv->pos_buffer_head = pos_find_prev;
        // interpolate
        long output_step = (long) (pos_find->syn_pos - pos_find_prev->syn_pos);
        long input_step =  (long) (pos_find->ana_pos - pos_find_prev->ana_pos);
        return pos_find_prev->ana_pos + input_step * 
            (double)(pv->output_total - pos_find_prev->syn_pos) / 
            (double)output_step;
    // if there's nothing in the queue, then we must be starting. 
    // after the first frame there are TWO entries
    } else if (pos_find_prev == NULL) {
        // if any of these fail, maybe the queue_length is too small
        // and we dropped some history too early
        assert(pos_find == pv->pos_buffer_rear);
        assert(pv->first_time);
        assert(pv->output_total == 0);
        return -(pv->ratio * pv->fftsize / 2.0);
    } // I can't think of any other case.
    assert(FALSE);
}


// Send samples to phase vocoder. size should match the number of
// samples computed by get_input_count.
//
void pv_put_input(Phase_vocoder x, int size, float *samples)
    // 'samples' points to samples to be sent each time
{
    PV *pv = (PV *)x;
    assert(pv->phase == PV_GOT_COUNT);
    // size must agree with the value computed by pv_get_input_count:
    assert(pv->expected_input == size);
    D printf("pv_put_input: size %d, %g at %ld\n", 
             size, *samples, (long) (pv->input_rear - pv->input_buffer));
    if (size > 0) {
        memcpy(pv->input_rear, samples, size * sizeof(*pv->input_rear));
        pv->input_rear += size;
        pv->input_total += size;
    }
    pv->phase = PV_GOT_INPUT;
}


void compute_one_frame(PV *pv, int ana_hopsize)
{
    float *syn_frame = pv->syn_frame;
    float *ana_frame = pv->ana_frame;
    int fftsize = pv->fftsize;
    int log2_fft = pv->log2_fft;
    float *mag = pv->mag;
    float *ana_phase = pv->ana_phase;
    float *syn_phase = pv->syn_phase;
    float *syn_win = pv->syn_win;
    float *frame_next = pv->frame_next;
    int syn_hopsize = pv->syn_hopsize;
    float *pre_ana_phase = pv->pre_ana_phase;
    float *pre_syn_phase = pv->pre_syn_phase;
    float *bin_freq = pv->bin_freq;
    int i;
//#define SKIP_PHASE_ADJUST
#ifdef SKIP_PHASE_ADJUST
    // for debugging we just copy the windowed input to the output
    // without adjusting phases.
    memcpy(syn_frame, ana_frame, fftsize * sizeof(*syn_frame));
#else
    /*DBG
    long zeros = pv->output_total + frame_next - pv->out_next;
    write_pv_frame(zeros, ana_frame, fftsize, "pvsyn");
    DBG*/
    OneDimensionFFTshift(ana_frame, fftsize);  // FFTshift
    fftInit(log2_fft);
    rffts(ana_frame, log2_fft, 1);
    
    /* get magnitude and phase */
    mag[0] = ana_frame[0];
    ana_phase[0] = 0;
    mag[fftsize / 2] = ana_frame[1];
    ana_phase[fftsize / 2] = 0;
    for (i = 1; i < fftsize / 2; i++) {
        float real = ana_frame[2 * i];
        float imag = ana_frame[2 * i + 1];
        mag[i] = (float)sqrt(real * real + imag * imag);
        ana_phase[i] = (float)atan2(imag, real);
    }
#ifdef PV_SINE_TEST
    if (pvst_offset == -1000) pvst_offset = -24;
    else pvst_offset += ana_hopsize;
    // phase of the sine should be (offset / 64) * TWO_PI,
    // but when sine phase is 0, atan2(0, -1) is -pi/2 so we have
    // atan2_phase = sine_phase - pi/2
    // adding 3 pi/2 instead of subtracting pi/2 to make fmod result positive
    double est_atan2_phase = fmod((pvst_offset / 64.0) * TWOPI + (3 * M_PI_2), TWOPI);
    if (est_atan2_phase > M_PI) est_atan2_phase -= TWOPI;
    printf("offset %ld hop %ld ph[8] %5f est.ph %5f real %5f imag %5f\n",
           pvst_offset, ana_hopsize, ana_phase[8], est_atan2_phase,
           ana_frame[16], ana_frame[17]);
#endif
    /* phase unwrapping & set synthesis phase */
    if (pv->first_time) {
        D printf("phasevocoder fftsize %d hopsize %d\n",
               fftsize, syn_hopsize);
        memcpy(syn_phase, ana_phase, 
               ((fftsize / 2) + 1) * sizeof(*syn_phase));
    } else if (pv->mode == PV_MODE_PHASEFIX) {
        // we'll start each iteration with prev_min_x set to the lowest bin
        // that will be assigned to the peak at prev_peak_x. We'll find the
        // next_min_x and the following next_peak_x. Update the phases.
        //
        int prev_peak_x = 0; // index of previous peak
        float prev_peak_mag; // magnitude of previous peak
        int prev_min_x = 0; // index of previous local minimum
        int next_peak_x; // index of peak between prev_min_x and next_min_x
        float next_peak_mag; // magnitude of next peak
        int next_min_x; // index of next minimum (after peak_x)
        float next_min_mag; // magnitude at next_min_x
        float last_mag = mag[0]; // used in search for peaks
        float this_mag = mag[1];
        float next_mag;
        int i; // loop index
        // decide if we're starting on a peak or a minimum:
        if (last_mag <= this_mag) { // starting on a minimum
            // find peak
            for (i = 1; i < fftsize / 2; i++) {
                next_mag = mag[i + 1];     // invariant: last_mag <= this_mag
                if (this_mag > next_mag) { // found peak
                    prev_peak_x = i;
                    prev_peak_mag = this_mag;
                    break;  // invariant: this_mag > next_mag
                }
                // invariant: this_mag <= next_mag
                last_mag = this_mag;
                this_mag = next_mag;  // invariant: last_mag <= this_mag
            }
            if (i >= fftsize / 2) {
                prev_peak_x = i;
            }
        } else { // set up to start loop
            next_mag = this_mag;  // invariant: last_mag > this_mag
            this_mag = last_mag;  // invariant: this_mag > next_mag
            prev_peak_mag = last_mag;
        }
        while (prev_min_x <= fftsize / 2) {
            // invariant: prev_min_x is previous local minimum or 0
            //        prev_peak_x is first local peak after prev_min_x (or 0)
            //        last_mag is mag at prev_peak_x - 1
            //        this_mag is mag at prev_peak_x
            //        next_mag is mag at prev_peak_x + 1
            // find next minimum
            // Note: prev_peak_x might be fftsize/2, so i might be fftsize/2 + 1
            for (i = prev_peak_x + 1; i < fftsize / 2; i++) {
                last_mag = this_mag;  // invariant: this_mag > next_mag
                this_mag = next_mag;  // invariant: last_mag > this_mag
                next_mag = mag[i + 1];
                if (this_mag <= next_mag) { // found minimum
                    // here, last_mag at i-1, this_mag at i, next_mag at i+1
                    // and next_mix_x == i
                    break;
                } // loop invariant: this_mag > next_mag
            }
            // invariant: this_mag <= next_mag || i == fftsize/2
            if (i >= fftsize / 2) { // special case at end of spectrum
                // no minimum found
                next_min_x = fftsize / 2 + 1;
            } else {
                next_min_x = i; // either we found peak or we set i to fftsize/2
                next_min_mag = mag[i];
            } // invariant: this_mag <= next_mag || i == fftsize/2
            // search for second peak; 
            for (i = next_min_x + 1; i < fftsize / 2; i++) {
                last_mag = this_mag; // invariant: this_mag <= next_mag
                this_mag = next_mag; // invariant: last_mag <= this_mag
                next_mag = mag[i + 1];
                if (this_mag > next_mag) { // found peak
                    // here, last_mag at i-1, this_mag at i, next_mag at i+1
                    // and next_peak_x == i
                    break;
                } // loop invariant: this_mag <= next_mag
            }
            next_peak_x = i;
            
            // special case if we're at the end:
            if (i >= fftsize / 2) {
                if (next_mag < this_mag) {
                    next_peak_x = fftsize / 2 + 1;
                } else {
                    next_peak_x = fftsize / 2; // this may not be necessary
                    next_peak_mag = mag[next_peak_x];
                }
            } else {
                next_peak_mag = mag[i];
            }
            // now we have prev_min, prev_peak, next_min, and next_peak. We
            // want bins from prev_min to next_min to get the phase shift of
            // prev_peak. First decide if next_min gets assigned to prev_peak
            // or next_peak. Assign to the closest peak, but break ties by
            // picking the largest peak.
            if (next_min_x - prev_peak_x < next_peak_x - next_min_x) {
                // closer to prev_peak, so include min with it
                next_min_x++;
            } else if ((next_min_x - prev_peak_x == next_peak_x - next_min_x) &&
                       // equidistant so see if prev_peak_mag > next_peak_mag
                       (prev_peak_mag > next_peak_mag)) {
                next_min_x++;
            }
            // Now we want to adjust phases of prev_min_x, prev_min_x + 1, 
            //   prev_min_x + 2, ..., next_min_x - 1.
            //
            // Assign the new phases according to the peak...
            // increment between actual phase increment value
            // and the phase increment value got when the
            // it's the nearest bin frequency. Used
            // in phase unwrapping:
            int j = prev_peak_x; // just to make more concise notation

            double phase_increment = ana_phase[j] - pre_ana_phase[j] -
                                     bin_freq[j] * ana_hopsize;
            // need to get phase_increment between -M_PI and +M_PI.
            // Algorithm: add M_PI, get phase_increment between 0 and TWO_PI,
            // then subtract M_PI:
            phase_increment = fmod(phase_increment + M_PI, TWOPI);
            if (phase_increment < 0)
                phase_increment += TWOPI;
            phase_increment -= M_PI;
            // estimated frequency from phase unwrapping
            /*DBG
            if (j == 8)
                printf("phase_increment %g hopsize %d\n", phase_increment, ana_hopsize);
            DBG*/
            float estimate_freq = (float) (phase_increment / ana_hopsize +
                                           bin_freq[j]);
            // get synthesis phase adjustment
            phase_increment = pre_syn_phase[j] + 
                syn_hopsize * estimate_freq - ana_phase[j];
            /*DBG
            if (j == 8)
                printf("phase_increment at %d is %g, freq %g ", j, phase_increment,
                       estimate_freq);
            DBG*/
            // update the range of bins:
            for (i = prev_min_x; i < next_min_x; i++) {
                syn_phase[i] = fmodf((float) (ana_phase[i] + phase_increment),
                                     (float) TWOPI);
            }
            /*DBG
            if (j == 8) printf("syn_phase[8] %g\n", syn_phase[8]);
            DBG*/
            // now get ready for the next iteration
            prev_min_x = next_min_x;
            prev_peak_x = next_peak_x;
            prev_peak_mag = next_peak_mag;

        }
    } else if (pv->mode == PV_MODE_STANDARD) {
        for (i = 0; i <= fftsize / 2; i++) {
            // increment between actual phase increment value
            // and the phase increment value got when the
            // it's the nearest bin frequency. Used
            // in phase unwrapping:
            double phase_increment = ana_phase[i] - pre_ana_phase[i] -
                                     bin_freq[i] * ana_hopsize;
            // need to get phase_increment between -M_PI and +M_PI.
            // Algorithm: add M_PI, get phase_increment between 0 and TWO_PI,
            // then subtract M_PI:
            phase_increment = fmod(phase_increment + M_PI, TWOPI);
            if (phase_increment < 0)
                phase_increment += TWOPI;
            phase_increment -= M_PI;

            // estimated frequency from phase unwrapping
            float estimate_freq = (float) (phase_increment / ana_hopsize +
                                           bin_freq[i]);
            // set synthesis phase
            syn_phase[i] = fmodf((float) (pre_syn_phase[i] +
                                          syn_hopsize * estimate_freq),
                                 (float) TWOPI);
        }
    } else if (pv->mode == PV_MODE_ROBOVOICE) {
        ; // syn_phase[] is unmodified, i.e. constant
    } else {
        assert(FALSE); // bad mode value
    }
    for (i = 0; i < fftsize / 2; i++) {
        // record phases
        pre_ana_phase[i] = ana_phase[i];
        pre_syn_phase[i] = syn_phase[i];
        
        // update realpart and imagpart
        syn_frame[i * 2] = (float) (mag[i] * cos(syn_phase[i]));
        syn_frame[i * 2 + 1] = (float) (mag[i] * sin(syn_phase[i]));
    }
    pre_ana_phase[i] = ana_phase[i];
    pre_syn_phase[i] = syn_phase[i];
    // update realpart and imagpart
    syn_frame[1] = (float) (mag[i] * cos(syn_phase[i]));
     // inverse FFT
    riffts(syn_frame, log2_fft, 1);
    
    // fftshift
    OneDimensionFFTshift(syn_frame, fftsize);
#endif // SKIP_PHASE_ADJUST
    D printf("    mid syn_frame->%g\n", syn_frame[pv->fftsize / 2]);
    //D printf("  frame offset %ld\n", frame_next - pv->output_buffer);
    // window the frame and then add it to the output buffer
    // assume here that there is room to add in syn_frame
    D printf("    add to frame_next: %ld\n", 
             (long) (pv->frame_next - pv->output_buffer));
    /*DBG
    float tmp_frame[4096];
    for (int i = 0; i < fftsize; i++) {
        tmp_frame[i] = syn_win[i] * syn_frame[i];
    }
    write_pv_frame(zeros, syn_win, fftsize, "pvsyn");
    write_pv_frame(zeros, syn_frame, fftsize, "pvsyn");
    write_pv_frame(zeros, tmp_frame, fftsize, "pvsyn");
    DBG*/
    int sum_count = fftsize - syn_hopsize;
    for (i = 0; i < sum_count; i++) {
        frame_next[i] += syn_win[i] * syn_frame[i];
    }
    for (/* continue from i */; i < fftsize; i++) {
        DD assert(frame_next[i] == 0);
        frame_next[i] = syn_win[i] * syn_frame[i];
    }
    frame_next += syn_hopsize;
    pv->frame_next = frame_next;
}


void update_position_queue(PV *pv, float *ana_center)
{
    int fftsize = pv->fftsize;
    float *frame_next = pv->frame_next;
    int syn_hopsize = pv->syn_hopsize;
    float *out_next = pv->out_next;

    // put the positions of the processed frame into the queue
    if (pv->first_time) {
        // insert a special starting correspondence:
        pv->pos_buffer_rear->ana_pos = lroundf(-pv->ratio * fftsize / 2);
        pv->pos_buffer_rear->syn_pos = 0;
        pv->pos_buffer_rear++;
    }
    // center of analysis window was at ana_center
    // input_total corresponds to input_rear
    pv->pos_buffer_rear->ana_pos = 
        pv->input_total - (long) (pv->input_rear - ana_center);
    // output window center was at frame_next - syn_hopsize + fftsize / 2
    // output_total corresponds to out_next
    pv->pos_buffer_rear->syn_pos = pv->output_total +
        (long) ((frame_next - syn_hopsize + (fftsize / 2)) - out_next);
    
    // set pos_buffer_rear to new last element in the queue
    pv->pos_buffer_rear++;
    if (pv->pos_buffer_rear == pv->pos_buffer + pv->queue_length) {
        pv->pos_buffer_rear = pv->pos_buffer; // wrap
    }
    // if queue is too full, remove first element at pos_buffer_head
    if (pv->pos_buffer_head == pv->pos_buffer_rear) {
        pv->pos_buffer_head++;
        if (pv->pos_buffer_head == pv->pos_buffer + pv->queue_length) {
            pv->pos_buffer_head = pv->pos_buffer; // wrap
        }
    }
}


float *finish_output(PV *pv)
{
    assert(pv->frame_next - pv->out_next >= pv->blocksize);
    pv->phase = PV_START;
    // remember the current output:
    float *block = pv->out_next;
    // update out_next where next output block will be computed
    pv->out_next = block + pv->blocksize;
    pv->output_total += pv->blocksize;
    D printf("    return offset %ld = %g\n", 
             (long) (pv->out_next - pv->output_buffer), *(pv->out_next));
    /* DEBUG: To produce a 32767-sample-long sawtooth from 0 to 1 (roughly)
     * as output, uncomment the following loop. This might be the first step
     * in debugging. If you do not get a smoothly increasing ramp for 32K
     * samples, then you are not handling the output of cmupv properly.
     */
    /* for (int i = 0; i < pv->blocksize; i++) {
     *     block[i] = ((pv->output_total - pv->blocksize + i) % 32767) / 32768.0;
     * }
     */
    return block;
}


float *pv_get_output(Phase_vocoder x)
{
    PV *pv = (PV *)x;
    assert(pv->phase == PV_GOT_INPUT);
#ifndef NDEBUG
    long blocksize = pv->blocksize;
    float *out_next = pv->out_next;
#endif
    int fftsize = pv->fftsize;
    int frames_to_compute = pv->frames_to_compute;
    int syn_hopsize = pv->syn_hopsize;
    float *ana_win = pv->ana_win;
    float ratio = pv->ratio;
    float *input_head = pv->input_head;
    float *ana_frame = pv->ana_frame;
    float *ana_center;
    
    int ana_hopsize = lroundf(syn_hopsize * ratio);
    if (ana_hopsize > pv->max_ana_hopsize) {
        ana_hopsize = pv->max_ana_hopsize;
    }

    // compute frames and add them to the output_buffer until there
    // are blocksize samples ready to deliver
    D printf("pv_get_output: frames_to_compute %d\n", frames_to_compute);
    int frame;
    for (frame = 0; frame < frames_to_compute; frame++) {
        assert(pv->frame_next - out_next < blocksize);
	int i;
        for (i = 0; i < fftsize; i++) // get and window the buffer
            ana_frame[i] = input_head[i] * ana_win[i];
        ana_center = input_head + fftsize / 2;
        D printf("    mid ana_frame->%g at %ld\n", *ana_center,
                 (long) (ana_center - pv->input_buffer));
        if (frame < frames_to_compute - 1) {
            input_head += ana_hopsize; // get ready for next iteration,
            // but on the last iteration, we do not add hopsize because
            // ratio might change. ana_hopsize is added in get_input_count()
            // to set up the next analysis frame location
        } else {
            // on the last iteration, update pv->input_head.
            // Equivalently, after the for loop we could do
            //   if (frames_to_compute > 0)
            //       pv->input_head += ana_hopsize * (frames_to_compute - 1);
            pv->input_head = input_head;
        }
        compute_one_frame(pv, ana_hopsize);
        update_position_queue(pv, ana_center);
        // first_time is not reset in update_position_queue where it is tested
        // because it is also used in pv_get_output2, which does not call
        // update_position_queue()
        pv->first_time = FALSE;
    }
    return finish_output(pv);
}


float *pv_get_output2(Phase_vocoder x)
{
    PV *pv = (PV *)x;
    assert(pv->phase == PV_START);
    
    long blocksize = pv->blocksize;
    int fftsize = pv->fftsize;
    float *out_next = pv->out_next;
    float *output_buffer = pv->output_buffer;
    float *ana_frame = pv->ana_frame;
    float *ana_win = pv->ana_win;
    long output_buffer_len = pv->output_buffer_len;

    D printf("pv_get_output2: blocksize %ld frame_next %ld "
           "out_next %ld buffer offset %ld\n",
           blocksize, (long) (pv->frame_next - output_buffer), 
           (long) (out_next - output_buffer),
           (long) (pv->output_total - (out_next - output_buffer)));

    // To produce blocksize, how many samples do we need? The next
    // sample to output is at out_next, and the next frame will be
    // addded at frame_next, so we've already computed 
    // out_next - frame_next:
    while (blocksize > (pv->frame_next - out_next)) {
        int64_t out_cnt = (pv->output_total + (pv->frame_next - out_next) + 
                           fftsize / 2);
        // if there's no room in the output buffer, shift the samples.
        // This is done here to avoid extra work (sometimes pv_get_output2
        // can be called and the samples are already in the buffer so there's
        // no need to shift.
        if (pv->frame_next + fftsize > output_buffer + output_buffer_len) {
            long shift = (long) (out_next - output_buffer);
            D printf("shift output by %ld\n", shift);
            memmove(output_buffer, out_next,
                    (output_buffer_len - shift) * sizeof(*output_buffer));
            /* for debugging, fill the end with zero. When we write (rather
               than add) to the buffer, assert that we're over-writing zeros */
            DD ZERO(output_buffer + output_buffer_len - shift, shift);
            out_next = output_buffer;
            pv->out_next = output_buffer;
            pv->frame_next -= shift;
        }
        int ana_hopsize = (*pv->callback)(out_cnt, ana_frame,
                                          fftsize, pv->rock);
        /* DEBUG - To check input, the following commented code will 
           write each analysis frame as a file. The analysis frame is
           prefixed with zeros so that it will be placed at the right
           time, but this generates N^2 samples, so only the first 20
           frames are written */
        /*DBG
        write_pv_frame(out_cnt - fftsize / 2, ana_frame, fftsize, "pvana");
        DBG*/
        int i;
        for (i = 0; i < fftsize; i++) ana_frame[i] *= ana_win[i];
        compute_one_frame(pv, ana_hopsize);
        pv->first_time = FALSE;
        D printf("pv_get_output2: blocksize %ld frame_next %ld "
               "out_next %ld buffer offset %ld\n",
               blocksize, (long) (pv->frame_next - output_buffer), 
               (long) (out_next - output_buffer),
               (long) (pv->output_total - (out_next - output_buffer)));
    }
    D printf("pv_get_output2 returning at offset %ld abs %I64d\n",
             (long) (pv->out_next - pv->output_buffer), pv->output_total);
    /*DBG out_next position is output_total, so if we subtract out_next - output_buffer,
       we get the absolute position of the output_buffer
    write_pv_frame(pv->output_total - (pv->out_next - pv->output_buffer), 
                   pv->output_buffer, pv->output_buffer_len, "pvbuf");
    DBG*/
    return finish_output(pv);
}
