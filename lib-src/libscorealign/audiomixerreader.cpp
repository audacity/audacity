/* audiomixerreader.cpp -- implements a class to read samples from Audacity
*
* 17-Jun-08  RBD created based on audiofilereader.cpp
*/
#include "assert.h"
#include "stdlib.h"
#include "audioreader.h"
#include "allegro.h"
#include "scorealign-glue.h"
#include "audiomixerreader.h"

double Audio_mixer_reader::get_sample_rate()
{
    return sample_rate;
}


Audio_mixer_reader::Audio_mixer_reader(void *mixer_, 
        mixer_process_fn fn_ptr, int chans, 
        double srate, double end_time)
{
    mixer = mixer_; // store in member variable
    mixer_process = fn_ptr;
    buffer = NULL;
    buffer_len = 0;
    index = 0;
    channels = chans;
    sample_rate = srate;
    total_frames = end_time * srate + 0.5 /* for rounding */;
}


long Audio_mixer_reader::get_frames()
{
    // precondition: mixer is valid and no samples have been read
    return total_frames;
}


long Audio_mixer_reader::read(float *data, long n)
{
    for (int i = 0; i < n; i++) { // fill data
        float sum = 0;
        // note: assume mixer returns stereo (interleaved)
        for (int chan = 0; chan < channels; chan++) {
            // sum over channels within a frame
            if (index >= buffer_len * channels) {
                buffer_len = 
                   (*mixer_process) (mixer, &buffer, AMR_BUFFER_FRAMES);
                // frame_count = mixer->Process(AMR_BUFFER_FRAMES);
                // buffer = (float *) mixer->GetBuffer();
                index = 0;
                if (buffer_len == 0) { // no more samples to read
                    // but we processed i
                    return i;
                }
            }
            sum += buffer[index++];
        }
        data[i] = sum;
    }
    return n; // when end is reached, n will be 0, but the caller shouldn't
              // be asking for samples beyond the end because the caller knows
              // how many samples to ask for.
}

void Audio_mixer_reader::close()
{
    // mixer is deleted by the creator of this object, so don't delete here
    buffer = NULL; 
}

void Audio_mixer_reader::print_info()
{
    printf("   Audacity mixer at @ %p\n", mixer);
    printf("   sample rate %g\n", get_sample_rate());
    printf("   total frames %d\n", get_frames());
}
