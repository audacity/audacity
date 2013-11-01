/* audiofilereader.cpp -- implements a class to read samples
 *
 * 14-Jun-08  RBD
 */
#include "assert.h"
#include "stdio.h"
#include "string.h"
#include "snd.h"
#include "audioreader.h"
#include "audiofilereader.h"

double Audio_file_reader::get_sample_rate()
{
    return snd.format.srate;
}


long Audio_file_reader::get_frames()
{
    return (snd.u.file.end_offset - snd.u.file.byte_offset) /
           snd_bytes_per_frame(&snd);
}


long Audio_file_reader::read(float *data, long n)
{
    // note that "samples_per_frame" is really "frames_per_window" in this
    // context, so we're computing bytes per window
    char *input_data = (char *) alloca(bytes_per_frame * samples_per_frame);
    assert(input_data != NULL) ;
	
    // read into the end of data
    long frames_read = snd_read(&snd, input_data, n);
    // now convert and move to beginning of data
    snd_node float_sound;
    float_sound.format = snd.format;
    float_sound.format.channels = 1; // make sure we convert to mono
    float_sound.format.mode = SND_MODE_FLOAT; // and convert to float
    float_sound.format.bits = 32;
    // note: snd_convert takes a frame count; divide samples by channels
    int converted = snd_convert(&float_sound, data, &snd, input_data, n);
    if (frames_read == n) assert(converted == n);
    return frames_read;
}


bool Audio_file_reader::open(char *filename, Scorealign &sa, bool verbose)
{
    bytes_per_frame = 0; // initialize now in case an error occurs
    snd.device = SND_DEVICE_FILE;
    snd.write_flag = SND_READ;
    strcpy(snd.u.file.filename, filename);
    long flags;
    int infile = snd_open(&snd, &flags);
    if (infile == SND_SUCCESS) {
        bytes_per_frame = snd_bytes_per_frame(&snd);
        calculate_parameters(sa, verbose);
        return true;
    }
    return false;
}


void Audio_file_reader::close()
{
    snd_close(&snd);
}


void Audio_file_reader::print_info()
{
    printf("   file name = %s\n", snd.u.file.filename);
    double sample_rate = snd.format.srate;
    printf("   sample rate = %g\n", sample_rate);
    printf("   channels = %d\n", snd.format.channels);
    /*=============================================================*/
    long frames =(snd.u.file.end_offset - snd.u.file.byte_offset) / 
            snd_bytes_per_frame(&snd);
    printf("   total frames number is = %d\n", frames);
    printf("   bits per sample is  %d\n", snd.format.bits);
    printf("   audio duration = %g seconds\n", (frames) / sample_rate);
    /*=============================================================*/
}
