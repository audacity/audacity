/* alignfiles.cpp -- do alignment on files
 *
 * 14-Jul-08 RBD
 *
 * This is an intermediate layer between main.cpp, the client, and 
 * scorealign.cpp, where the real work is done. 
 */
 
#include "stdio.h"
#include "string.h"
#include "sndfile.h"
#include <fstream>
#include "allegro.h"
#include "audioreader.h"
#include "audiofilereader.h"
#include "alignfiles.h"
#include "scorealign.h"

using namespace std;

/* is_midi_file -- see if file name ends in .mid */
/**/
bool is_midi_file(const char *filename)
{
    size_t len = strlen(filename);
    return (len > 4 && strcmp(filename + len - 4, ".mid") == 0);
}


bool align_files(const char *infilename1, const char *infilename2, 
                Scorealign &sa, bool verbose)
{
    sa.verbose = verbose;
    if (verbose) printf("opening %s\n", infilename1);
    if (is_midi_file(infilename1)) {
        // get sequence from infilename1
        Alg_seq seq(infilename1, true);
        if (seq.get_read_error()) { // error opening file
            if (verbose)
                printf ("Error: Not able to open input file %s\n", 
                        infilename1);
            return false;
        }
        if (verbose) printf("opening %s\n", infilename2);
        if (is_midi_file(infilename2)) {
            // get sequence from infilename2
            Alg_seq seq2(infilename2, true);
            if (seq2.get_read_error()) { // error opening file
                if (verbose)
                    printf ("Error: Not able to open input file %s\n", 
                            infilename2);
                return false;
            }
            sa.align_midi_to_midi(seq, seq2);
            return true;
        } else {
            // get audio from infilename2
            Audio_file_reader reader;
            if (!reader.open(infilename2, sa, verbose)) {
                if (verbose)
                    printf ("Error: Not able to open input file %s\n", 
                            infilename2);
                return false;
            }
            sa.align_midi_to_audio(seq, reader);
            return true;
        }
    } else { // if first file is audio, so is second file
        Audio_file_reader reader1;
        if (!reader1.open(infilename1, sa, verbose)) {
            if (verbose)
                printf ("Error: Not able to open input file %s\n", 
                        infilename1);
            return false;
        }
        if (verbose) printf("opening %s\n", infilename2);
        Audio_file_reader reader2;
        if (!reader2.open(infilename2, sa, verbose)) {
            if (verbose)
                printf ("Error: Not able to open input file %s\n", 
                        infilename2);
            return false;
        }
        sa.align_audio_to_audio(reader1, reader2);
        return true;
    }
}
