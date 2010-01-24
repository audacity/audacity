/* scorealign-glue.cpp -- interface between Audacity and scorealign library
 *
 * 21-Jul-08 RBD
 */

#include "allegro.h"
#include "scorealign-glue.h"
#include "audioreader.h"
#include "audiomixerreader.h"
#include "scorealign.h"

void scorealign(void *mixer, mixer_process_fn fn_ptr, int chans, double srate,
                double end_time, Alg_seq *seq)
{
    Scorealign sa;
    sa.frame_period = 0.2;
    sa.window_size = 0.2;
    Audio_mixer_reader reader(mixer, fn_ptr, chans, srate, end_time);
    reader.calculate_parameters(sa, false);
    sa.align_midi_to_audio(*seq, reader, true);
    sa.midi_tempo_align(*seq, false);
    // seq has now been modified to conform to audio provided by mixer
    seq->set_real_dur(end_time);
}
