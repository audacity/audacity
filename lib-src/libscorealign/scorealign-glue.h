typedef long (*mixer_process_fn)(void *mix, float **buffer, long n);

#include "ScoreAlignParams.h"

int scorealign(void *mixer, mixer_process_fn fn_ptr,
                int chans, double srate, 
                double end_time, Alg_seq *seq, SAProgress *progress,
                ScoreAlignParams &params);
