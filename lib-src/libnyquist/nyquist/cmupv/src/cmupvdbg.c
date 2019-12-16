/* cmupvdbg.c -- debug support for cmupv
 * Roger B. Dannenberg
 * Nov 2015
 */

/* To see what data is going into the phase vocoder, write_pv_frame
 * saves frames or buffers to disk as audio.
 * If something is wrong with your applications delivery of analysis frames,
 * probably most frames are wrong, so you should write, say, 10 initial
 * files and look at them. Set MAX_FILES to 10 and MAX_SAVE to 10. Then,
 * after running your program, use Audacity to open the 10 frames saved
 * to disk: use the Files:Import:Audio command to load all 10 in one 
 * command as different tracks. You should see 10 tracks, each containing
 * one frame, shifted to the correct source time location. (If not, then
 * find and fix the bug.)
 *
 * This function is also useful for printing other data, e.g. synthesis frames
 * or output buffers. To get the most out of this, you should set the first
 * parameter, zeros, to the absolute sample offset, e.g. if you want to view
 * synthesis frames that are spaced with a 64 sample hop size, you should set
 * zeros to 0, 64, 128, 192, ... so that when you view the files in Audacity,
 * the samples will line up in time with each other and with the output file.
 *
 * You can display multiple frame types and lengths, so the prefix parameter
 * allows you to effectively label the data. This prefix is used to construct
 * the file name.
 *
 * MAX_FILES tells how many files to write. Because of padding, file writing
 * is an N-squared operation, so only write initial frames if possible, but
 * use a large number if you need to see everything.
 *
 * MAX_SAVE tells how many file *names* to use. Files are numbered, so we'll
 * simply take the number modulo MAX_SAVE so that file names are reused and
 * old files are overwritten. Use a large value of MAX_SAVE to save everything.
 * Use 1 to get only the last frame.
 */

#include "stdio.h"
#include "sndfile.h"


#define MAX_FILES 10
#define MAX_SAVE 10
static int file_no = 0;


void write_pv_frame(long zeros, float *frame, long fftsize, char *prefix)
{
    float z = 0;
    char path[128];
    SF_INFO sfinfo;
    SNDFILE *sf;
    if (file_no >= MAX_FILES) return;
    sprintf(path, "%s-%02d.wav", prefix, (file_no % MAX_SAVE) + 1);
    sfinfo.frames = 0;
    sfinfo.samplerate = 44100;
    sfinfo.channels = 1;
    sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    sfinfo.sections = 0;
    sfinfo.seekable = 0;
    sf = sf_open(path, SFM_WRITE, &sfinfo);
    for (long i = 0; i < zeros; i++) {
        sf_writef_float(sf, &z, 1);
    }
    sf_writef_float(sf, frame, fftsize);
    sf_close(sf);
    printf("wrote %s, %ld zeros\n", path, zeros);
    file_no++;
}
