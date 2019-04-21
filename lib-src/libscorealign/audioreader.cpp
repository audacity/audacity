/* audioreader.cpp -- reads sequence of overlapping windows
 *
 * 14-Jul-08  RBD
 */

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "sautils.h"
#include "string.h"
#include <fstream>
#include "allegro.h"
#include "audioreader.h"
#include "scorealign.h"

using namespace std;

long Audio_reader::read_window(float *data)
// reads the next window of samples
//   the first time, fill half the window with zeros and the second half 
// with data from the file
// after that, shift the window by hop_size and fill the end of the window 
//   with hop_size new samples
// the window is actually constructed in temp_data, then copied to data. 
// That way, the caller can apply a smoothing function to data and we'll
//   still have a copy.
// the function returns false on the next call when detecting that there
//   is no more samples, 
// data -- the window to be returned
// temp_data -- since we destroy data by windowing, temp_data saves 
//   overlapping samples so we don't have to read them again
// samples_per_frame -- must be even, note that first window is padded
//   half-full with zeros
// hop_samples -- additional samples read each time after the first window
{
    int frames_read;    // how many frames did we read?

    int hop = hop_samples;
    if (reading_first_window) {
        hop = samples_per_frame / 2; // first time we read more data	
        // zero end of temp_data, which will shift to beginning
        memset(temp_data + hop, 0, 
               sizeof(float) * (samples_per_frame - hop));
        reading_first_window = false;
    }
	
    // before reading in new sounds, shift temp_data by hop_size
    memmove(temp_data, temp_data + hop, 
            (samples_per_frame - hop) * sizeof(float));

    frames_read = read(temp_data + samples_per_frame - hop, hop);
    // zero any leftovers (happens at last frame):
    //printf("check fr %i  hs %i ws %i ",frames_read,hop_size,window_size); 
    memset(temp_data + samples_per_frame - hop + frames_read, 0, 
           sizeof(float) * (hop - frames_read));
    assert(samples_per_frame - frames_read >= 0);

    // now copy temp_data to data	
    memcpy(data, temp_data, sizeof(float) * samples_per_frame);
    
    if (frames_read != hop && reading_last_window == false) {
        reading_last_window = true;
        return true; 
    } else if (reading_last_window == true) {
        return false; 
    } else {
        return true; 
    }
}


void Audio_reader::calculate_parameters(Scorealign &sa, bool verbose)
{
    double sample_rate = get_sample_rate();
    long pcm_frames = get_frames();
    // we want to make sure samples_per_frame is even, to keep things 
    // consistent we'll change hopsize_samples the same way
    samples_per_frame = (int) (sa.window_size * sample_rate + 0.5);
    if (samples_per_frame % 2 == 1) 
        samples_per_frame = samples_per_frame + 1;
   
   /*=============================================================*/
	
    hop_samples = (int)(sa.frame_period * sample_rate + 0.5);
    if (hop_samples % 2 == 1) 
        hop_samples = hop_samples + 1;
    actual_frame_period = (hop_samples / sample_rate);

    // this is stored back in a field in sa as well as here in the reader
    frame_count= (int) ceil(((float) pcm_frames / hop_samples + 1)); 	
    temp_data = ALLOC(float, samples_per_frame);
    memset(temp_data, 0, samples_per_frame * sizeof(temp_data[0]));
    assert(temp_data);
}



