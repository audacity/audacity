/* scorealign-glue.cpp -- interface between Audacity and scorealign library
 *
 * 21-Jul-08 RBD
 */

#include "allegro.h"
#include "audioreader.h"
#include "scorealign.h"
#include "scorealign-glue.h"
#include "audiomixerreader.h"


int scorealign(void *mixer, mixer_process_fn fn_ptr, int chans, double srate,
                double end_time, Alg_seq *seq, SAProgress *progress, 
                ScoreAlignParams &params)
{
    Scorealign sa;
    sa.frame_period = params.mFramePeriod;
    sa.window_size = params.mWindowSize;
    sa.silence_threshold = params.mSilenceThreshold;
    sa.force_final_alignment = (params.mForceFinalAlignment != 0.0);
    sa.ignore_silence = (params.mIgnoreSilence != 0.0);
    sa.presmooth_time = params.mPresmoothTime;
    sa.line_time = params.mLineTime;
    sa.smooth_time = params.mSmoothTime;

    Audio_mixer_reader reader(mixer, fn_ptr, chans, srate, end_time);
    reader.calculate_parameters(sa, false);
    sa.progress = progress;
    int result = sa.align_midi_to_audio(*seq, reader);

    params.mMidiStart = sa.first_x * sa.actual_frame_period_0;
    params.mMidiEnd = (sa.last_x + 1) * sa.actual_frame_period_0;
    params.mAudioStart = sa.first_y * sa.actual_frame_period_1;
    params.mAudioEnd = (sa.last_y + 1) * sa.actual_frame_period_1;

    if (result != SA_SUCCESS) {
        return result;
    }

    sa.midi_tempo_align(*seq);
    // seq has now been modified to conform to audio provided by mixer
    return SA_SUCCESS; // success
}
