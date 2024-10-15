/* audiofilereader.cpp -- implements a class to read samples
 *
 * 14-Jun-08  RBD
 * 16-Jun-08  RBD revised to use libsndfile
 */
#include "assert.h"
#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "sndfile.h"
#include "audioreader.h"
#include "audiofilereader.h"

#ifdef WIN32
#include <malloc.h>
#include <windows.h>
#define bzero(addr, siz) memset(addr, 0, siz)
#define alloca _alloca
#endif

double Audio_file_reader::get_sample_rate()
{
    return sf_info.samplerate;
}


long Audio_file_reader::get_frames()
{
    return total_frames;
}


long Audio_file_reader::read(float *data, long n)
{
    // note that "samples_per_frame" is really "frames_per_window" in this
    // context, so we're computing bytes per window
    float *input_data = (float *) alloca(bytes_per_frame * samples_per_frame);
    assert(input_data != NULL) ;
	
    long frames_read = (long) sf_readf_float(sf, input_data, n);
    long chans = sf_info.channels;
    // now convert to mono and move to data
    for (int frame = 0; frame < frames_read; frame++) {
        float sum = 0;
        for (int chan = 0; chan < sf_info.channels; chan++) {
            // sum over channels within a frame
            sum += input_data[frame * chans + chan];
        }
        // write the frame sum to result array
        data[frame] = sum;
    }
    return frames_read;
}


bool Audio_file_reader::open(const char *filename, Scorealign &sa, bool verbose)
{
    bytes_per_frame = 0; // initialize now in case an error occurs
    name[0] = 0;
    bzero(&sf_info, sizeof(sf_info));
    sf = sf_open(filename, SFM_READ, &sf_info);
    if (!sf) {
#ifdef WIN32
		/* windows-specific code to report error opening file */
		char *msg;
		DWORD code = GetLastError();
		DWORD code2 = FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | 
								    FORMAT_MESSAGE_FROM_SYSTEM,
									0, code, 0, (LPSTR) &msg, 0, 0);
	    printf("Error string: %s\n", msg);
		LocalFree(msg);
#endif
		return false;
	}
    strncpy(name, filename, MAX_NAME_LEN);
    name[MAX_NAME_LEN] = 0; // just in case
    total_frames = (long) sf_seek(sf, 0, SEEK_END);
    sf_seek(sf, 0, SEEK_SET);
    // we're going to read floats, but they might be multi-channel...
    bytes_per_frame = sf_info.channels * sizeof(float);
    calculate_parameters(sa, verbose);
    return true;
}


void Audio_file_reader::close()
{
    sf_close(sf);
}


void Audio_file_reader::print_info()
{
    printf("   file name = %s\n", name);
    double sample_rate = sf_info.samplerate;
    printf("   sample rate = %g\n", sample_rate);
    printf("   channels = %d\n", sf_info.channels);
    /*=============================================================*/
    printf("   total frames number is = %ld\n", total_frames);
    printf("   audio duration = %g seconds\n", total_frames / sample_rate);
    /*=============================================================*/
}
