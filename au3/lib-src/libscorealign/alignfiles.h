/* alignfiles.h -- perform alignment given filenames
 *
 * this module is an intermediate between the command-line interface 
 * main.cpp and the alignment code in scorealign.cpp. The scorealign.cpp
 * module is supposed to work on data from any source, e.g. it could be
 * a file, or it could be an object that sucks samples out of an
 * Audacity wave track. This module is supposed to not assume a command
 * line, type-script based interface, but *does* assume that you want
 * to read data from files, so you pass filenames into this module and
 * it reads the files and calls scorealign.cpp to do the alignment work.
 *
 * 14-Jul-08  RBD
 */

bool align_files(const char *infilename1, const char *infilename2, 
                 Scorealign &sa, bool verbose);

bool is_midi_file(const char *filename);
