/*
 *	TwoLAME: an optimized MPEG Audio Layer Two encoder
 *
 *	Copyright (C) 2004-2007 The TwoLAME Project
 *
 *	This library is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU Lesser General Public
 *	License as published by the Free Software Foundation; either
 *	version 2.1 of the License, or (at your option) any later version.
 *
 *	This library is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *	Lesser General Public License for more details.
 *
 *	You should have received a copy of the GNU Lesser General Public
 *	License along with this library; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  $Id$
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>

#include <twolame.h>
#include <sndfile.h>
#include "frontend.h"



/* 
  Global Variables
*/
int use_raw = FALSE;            // use raw input?
int sample_size = 16;           // number of bits per sample for raw input
int single_frame_mode = FALSE;  // only encode a single frame of MPEG audio ?
int byteswap = FALSE;           // swap endian on input audio ?
int channelswap = FALSE;        // swap left and right channels ?
SF_INFO sfinfo;                 // contains information about input file format

char inputfilename[MAX_NAME_SIZE] = "\0";
char outputfilename[MAX_NAME_SIZE] = "\0";





/* 
  new_extension()
  Puts a new extension name on a file name <filename>.
  Removes the last extension name, if any.
*/
static void new_extension(char *filename, char *extname, char *newname)
{
    int found, dotpos;

    // First, strip the old extension
    dotpos = strlen(filename);
    found = 0;
    do {
        switch (filename[dotpos]) {
        case '.':
            found = 1;
            break;
        case '\\':
        case '/':
        case ':':
            found = -1;
            break;
        default:
            dotpos--;
            if (dotpos < 0)
                found = -1;
            break;
        }
    } while (found == 0);

    if (found == -1) {
        strncpy(newname, filename, MAX_NAME_SIZE);
    }
    if (found == 1) {
        strncpy(newname, filename, dotpos);
        newname[dotpos] = '\0';
    }
    // Make sure there is room in the string for the 
    // new filename and the extension
    if (strlen(newname) + strlen(extname) + 1 < MAX_NAME_SIZE) {
        strcat(newname, extname);
    }
}

static char *format_filesize_string(int filesize)
{
    static const int constKB = 1024;    // Kilobyte
    static const int constMB = 1024 * 1024; // Megabyte
    static const int constGB = 1024 * 1024 * 1024;  // Gigabyte
    char *string = malloc(MAX_NAME_SIZE);

    if (filesize < constKB) {
        snprintf(string, MAX_NAME_SIZE, "%d bytes", filesize);
    } else if (filesize < constMB) {
        snprintf(string, MAX_NAME_SIZE, "%2.2f KB", (float) filesize / constKB);
    } else if (filesize < constGB) {
        snprintf(string, MAX_NAME_SIZE, "%2.2f MB", (float) filesize / constMB);
    } else {
        snprintf(string, MAX_NAME_SIZE, "%2.2f GB", (float) filesize / constGB);
    }

    return string;
}

/*
	print_filenames()
	Display the input and output filenames
*/
static void print_filenames(int verbosity)
{
    char *ifn, *ofn;

    if (strcmp(inputfilename, "-") == 0)
        ifn = "STDIN";
    else
        ifn = inputfilename;

    if (strcmp(outputfilename, "-") == 0)
        ofn = "STDOUT";
    else
        ofn = outputfilename;


    if (verbosity == 1) {
        fprintf(stderr, "Encoding %s to %s\n", ifn, ofn);
    } else if (verbosity > 1) {
        fprintf(stderr, "---------------------------------------------------------\n");
        fprintf(stderr, "Input Filename: %s\n", ifn);
        fprintf(stderr, "Output Filename: %s\n", ofn);
    }

}



/* 
  usage_long() 
  Display the extended usage information
*/
static void usage_long()
{

    fprintf(stderr, "TwoLAME version %s (%s)\n", get_twolame_version(), get_twolame_url());
    fprintf(stderr, "MPEG Audio Layer II (MP2) encoder\n");
    fprintf(stderr, "Usage: \n");

    fprintf(stderr, "\ttwolame [options] <infile> [outfile]\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "Both input and output filenames can be set to - to use stdin/stdout.\n");
    fprintf(stderr, "  <infile>       input sound file (any format supported by libsndfile)\n");
    fprintf(stderr, "  <outfile>      output bit stream of encoded audio\n");

    fprintf(stderr, "\nInput Options\n");
    fprintf(stderr, "\t-r, --raw-input          input is raw signed PCM audio\n");
    fprintf(stderr, "\t-x, --byte-swap          force byte-swapping of input\n");
    fprintf(stderr, "\t-s, --samplerate srate   sampling frequency of raw input (Hz)\n");
    fprintf(stderr,
            "\t    --samplesize bits    size of raw input samples in bits (default 16-bit)\n");
    fprintf(stderr, "\t-N, --channels nch       number of channels in raw input\n");
    fprintf(stderr, "\t-g, --swap-channels      swap channels of input file\n");
    fprintf(stderr, "\t    --scale value        scale input (multiply PCM data)\n");
    fprintf(stderr, "\t    --scale-l value      scale channel 0 (left) input\n");
    fprintf(stderr, "\t    --scale-r value      scale channel 1 (right) input\n");


    fprintf(stderr, "\nOutput Options\n");
    fprintf(stderr, "\t-m, --mode mode          (s)tereo, (j)oint, (d)ual, (m)ono or (a)uto\n");
    fprintf(stderr,
            "\t-a, --downmix            downmix from stereo to mono file for mono encoding\n");
    fprintf(stderr, "\t-b, --bitrate br         total bitrate in kbps (default 192 for 44.1kHz)\n");
    fprintf(stderr, "\t-P, --psyc-mode psyc     psychoacoustic model -1 to 4 (default 3)\n");
    fprintf(stderr, "\t-v, --vbr                enable VBR mode\n");
    fprintf(stderr,
            "\t-V, --vbr-level lev      enable VBR and set VBR level -50 to 50 (default 5)\n");
    fprintf(stderr, "\t-B, --max-bitrate rate   set the upper bitrate when in VBR mode\n");
    fprintf(stderr, "\t-l, --ath lev            ATH level (default 0.0)\n");
    fprintf(stderr, "\t-q, --quick num          only calculate psy model every num frames\n");
    fprintf(stderr, "\t-S, --single-frame       only encode a single frame of MPEG Audio\n");


    fprintf(stderr, "\nMiscellaneous Options\n");
    fprintf(stderr, "\t-c, --copyright          mark as copyright\n");
    fprintf(stderr, "\t    --non-copyright      mark as non-copyright (default)\n");
    fprintf(stderr, "\t-o, --non-original       mark as non-original\n");
    fprintf(stderr, "\t    --original           mark as original (default)\n");
    fprintf(stderr, "\t-p, --protect            enable CRC error protection\n");
    fprintf(stderr, "\t-d, --padding            force padding bit/frame on\n");
    fprintf(stderr, "\t-R, --reserve-bits num   set number of reserved bits in each frame\n");
    fprintf(stderr, "\t-e, --deemphasis emp     de-emphasis n/5/c (default: (n)one)\n");
    fprintf(stderr, "\t-E, --energy             turn on energy level extensions\n");

    fprintf(stderr, "\nVerbosity Options\n");
    fprintf(stderr, "\t-t, --talkativity num    talkativity 0-10 (default is 2)\n");
    fprintf(stderr, "\t    --quiet              same as --talkativity=0\n");
    fprintf(stderr, "\t    --brief              same as --talkativity=1\n");
    fprintf(stderr, "\t    --verbose            same as --talkativity=4\n");


    fprintf(stderr, "\n");
    fprintf(stderr, "\nAllowable bitrates for 32, 44.1 and 48kHz sample input (MPEG-1)\n");
    fprintf(stderr, "  32,  48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320, 384\n");
    fprintf(stderr, "\nAllowable bitrates for 16, 22.05 and 24kHz sample input (MPEG-2)\n");
    fprintf(stderr, "   8,  16,  24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160\n");

    fprintf(stderr, "\n");
    exit(ERR_NO_ENCODE);
}



/* 
  usage_short() 
  Display the short usage information
*/
static void usage_short()
{
    /* print a bit of info about the program */
    fprintf(stderr, "TwoLAME version %s (%s)\n", get_twolame_version(), get_twolame_url());
    fprintf(stderr, "MPEG Audio Layer II (MP2) encoder\n\n");
    fprintf(stderr, "Usage: twolame [options] <infile> [outfile]\n\n");
    fprintf(stderr, "Try \"twolame --help\" for more information.\n");
    exit(ERR_NO_ENCODE);
}


/*
	build_shortopt_string()
	Creates a short args string from the options structure
	for use with getopt_long
*/
static char *build_shortopt_string(struct option *opts)
{
    int count = 0;
    char *shortstr = NULL;
    int c = 0, n = 0;

    // Start by counting the number of options
    while (opts[count].val != 0) {
        count++;
    }


    // Allocate memory for the string
    shortstr = malloc((count * 2) + 1);

    // And loop through the options again
    for (n = 0; opts[n].val != 0; n++) {
        if (opts[n].val > 0 && opts[n].val < 127) {
            shortstr[c++] = opts[n].val;
            if (opts[n].has_arg == optional_argument) {
                fprintf(stderr, "gah: can't do optional arguments\n");
            } else if (opts[n].has_arg == required_argument) {
                shortstr[c++] = ':';
            }
        }
    }

    // Finally - terminate the string
    shortstr[c] = '\0';

    return shortstr;
}



/* 
  parse_args() 
  Parse the command line arguments
*/
static void parse_args(int argc, char **argv, twolame_options * encopts)
{
    int ch = 0;

    // process args
    struct option longopts[] = {

        // Input
        {"raw-input", no_argument, NULL, 'r'},
        {"byte-swap", no_argument, NULL, 'x'},
        {"samplerate", required_argument, NULL, 's'},
        {"samplesize", required_argument, NULL, 1000},
        {"channels", required_argument, NULL, 'N'},
        {"swap-channels", no_argument, NULL, 'g'},
        {"scale", required_argument, NULL, 1001},
        {"scale-l", required_argument, NULL, 1002},
        {"scale-r", required_argument, NULL, 1003},

        // Output
        {"mode", required_argument, NULL, 'm'},
        {"downmix", no_argument, NULL, 'a'},
        {"bitrate", required_argument, NULL, 'b'},
        {"psyc-mode", required_argument, NULL, 'P'},
        {"vbr", no_argument, NULL, 'v'},
        {"vbr-level", required_argument, NULL, 'V'},
        {"max-bitrate", required_argument, NULL, 'B'},
        {"ath", required_argument, NULL, 'l'},
        {"quick", required_argument, NULL, 'q'},
        {"single-frame", no_argument, NULL, 'S'},

        // Misc
        {"copyright", no_argument, NULL, 'c'},
        {"non-copyright", no_argument, NULL, 1004},
        {"non-original", no_argument, NULL, 'o'},
        {"original", no_argument, NULL, 1005},
        {"protect", no_argument, NULL, 'p'},
        {"padding", no_argument, NULL, 'd'},
        {"reserve-bits", required_argument, NULL, 'R'},
        {"deemphasis", required_argument, NULL, 'e'},
        {"energy", no_argument, NULL, 'E'},

        // Verbosity
        {"talkativity", required_argument, NULL, 't'},
        {"quiet", no_argument, NULL, 1006},
        {"brief", no_argument, NULL, 1007},
        {"verbose", no_argument, NULL, 1008},
        {"help", no_argument, NULL, 'h'},

        {NULL, 0, NULL, 0}
    };


    // Create a short options structure from the long one
    char *shortopts = build_shortopt_string(longopts);
    // fprintf(stderr,"shortopts: %s\n", shortopts);


    // Input format defaults
    memset(&sfinfo, 0, sizeof(sfinfo));
    sfinfo.format = 0;
    sfinfo.samplerate = DEFAULT_SAMPLERATE;
    sfinfo.channels = DEFAULT_CHANNELS;
    sfinfo.frames = 0;


    while ((ch = getopt_long(argc, argv, shortopts, longopts, NULL)) != -1) {
        switch (ch) {

            // Input
        case 'r':
            use_raw = 1;
            break;

        case 'x':
            byteswap = TRUE;
            break;

        case 's':
            twolame_set_out_samplerate(encopts, atoi(optarg));
            sfinfo.samplerate = atoi(optarg);
            break;

        case 1000:             // --samplesize
            sample_size = atoi(optarg);
            break;

        case 'N':
            sfinfo.channels = atoi(optarg);
            break;

        case 'g':
            channelswap = TRUE;
            break;

        case 1001:             // --scale
            twolame_set_scale(encopts, atof(optarg));
            break;

        case 1002:             // --scale-l
            twolame_set_scale_left(encopts, atof(optarg));
            break;

        case 1003:             // --scale-r
            twolame_set_scale_right(encopts, atof(optarg));
            break;



            // Output
        case 'm':
            if (*optarg == 's') {
                twolame_set_mode(encopts, TWOLAME_STEREO);
            } else if (*optarg == 'd') {
                twolame_set_mode(encopts, TWOLAME_DUAL_CHANNEL);
            } else if (*optarg == 'j') {
                twolame_set_mode(encopts, TWOLAME_JOINT_STEREO);
            } else if (*optarg == 'm') {
                twolame_set_mode(encopts, TWOLAME_MONO);
            } else if (*optarg == 'a') {
                twolame_set_mode(encopts, TWOLAME_AUTO_MODE);
            } else {
                fprintf(stderr, "Error: mode must be a/s/d/j/m not '%s'\n\n", optarg);
                usage_long();
            }
            break;

        case 'a':              // downmix
            twolame_set_mode(encopts, TWOLAME_MONO);
            break;

        case 'b':
            twolame_set_bitrate(encopts, atoi(optarg));
            break;

        case 'P':
            twolame_set_psymodel(encopts, atoi(optarg));
            break;

        case 'v':
            twolame_set_VBR(encopts, TRUE);
            break;

        case 'V':
            twolame_set_VBR(encopts, TRUE);
            twolame_set_VBR_level(encopts, atof(optarg));
            break;

        case 'B':
            twolame_set_VBR_max_bitrate_kbps(encopts, atoi(optarg));
            break;

        case 'l':
            twolame_set_ATH_level(encopts, atof(optarg));
            break;

        case 'q':
            twolame_set_quick_mode(encopts, TRUE);
            twolame_set_quick_count(encopts, atoi(optarg));
            break;

        case 'S':
            single_frame_mode = TRUE;
            break;


            // Miscellaneous 
        case 'c':
            twolame_set_copyright(encopts, TRUE);
            break;
        case 1004:              // --non-copyright
            twolame_set_copyright(encopts, FALSE);
            break;
        case 'o':              // --non-original
            twolame_set_original(encopts, FALSE);
            break;
        case 1005:             // --original
            twolame_set_original(encopts, TRUE);
            break;
        case 'p':
            twolame_set_error_protection(encopts, TRUE);
            break;
        case 'd':
            twolame_set_padding(encopts, TWOLAME_PAD_ALL);
            break;
        case 'R':
            twolame_set_num_ancillary_bits(encopts, atoi(optarg));
            break;
        case 'e':
            if (*optarg == 'n')
                twolame_set_emphasis(encopts, TWOLAME_EMPHASIS_N);
            else if (*optarg == '5')
                twolame_set_emphasis(encopts, TWOLAME_EMPHASIS_5);
            else if (*optarg == 'c')
                twolame_set_emphasis(encopts, TWOLAME_EMPHASIS_C);
            else {
                fprintf(stderr, "Error: emphasis must be n/5/c not '%s'\n\n", optarg);
                usage_long();
            }
            break;
        case 'E':
            twolame_set_energy_levels(encopts, TRUE);
            break;


            // Verbosity
        case 't':
            twolame_set_verbosity(encopts, atoi(optarg));
            break;

        case 1006:             // --quiet
            twolame_set_verbosity(encopts, 0);
            break;

        case 1007:             // --brief
            twolame_set_verbosity(encopts, 1);
            break;

        case 1008:             // --verbose
            twolame_set_verbosity(encopts, 4);
            break;

        case 'h':
            usage_long();
            break;

        default:
            usage_short();
            break;
        }
    }



    // Look for the input and output file names
    argc -= optind;
    argv += optind;
    while (argc) {
        if (inputfilename[0] == '\0')
            strncpy(inputfilename, *argv, MAX_NAME_SIZE);
        else if (outputfilename[0] == '\0')
            strncpy(outputfilename, *argv, MAX_NAME_SIZE);
        else {
            fprintf(stderr, "excess argument: %s\n", *argv);
            usage_short();
        }

        argv++;
        argc--;
    }


    // Check that we now have input and output file names ok
    if (inputfilename[0] == '\0') {
        fprintf(stderr, "Missing input filename.\n");
        usage_short();
    }
    if (outputfilename[0] == '\0' && strcmp(inputfilename, "-") != 0) {
        // Create output filename from the inputfilename 
        // and change the suffix
        new_extension(inputfilename, OUTPUT_SUFFIX, outputfilename);
    }
    if (outputfilename[0] == '\0') {
        fprintf(stderr, "Missing output filename.\n");
        usage_short();
    }
    // Check -r is supplied when reading from STDIN
    if (strcmp(inputfilename, "-") == 0 && !use_raw) {
        fprintf(stderr, "Error: please use RAW audio '-r' switch when reading from STDIN.\n");
        usage_short();
    }
}



static FILE *open_output_file(char *filename)
{
    FILE *file;


    // Do they want STDOUT ?
    if (strncmp(filename, "-", 1) == 0) {
        file = stdout;
    } else {
        file = fopen(filename, "wb");
    }

    // Check for errors
    if (file == NULL) {
        perror("Failed to open output file");
        exit(ERR_OPENING_OUTPUT);
    }

    return file;
}


int main(int argc, char **argv)
{
    twolame_options *encopts = NULL;
    audioin_t *inputfile = NULL;
    FILE *outputfile = NULL;
    short int *pcmaudio = NULL;
    unsigned int frame_count = 0;
    unsigned int total_frames = 0;
    unsigned int frame_len = 0;
    unsigned int total_bytes = 0;
    unsigned char *mp2buffer = NULL;
    int samples_read = 0;
    int mp2fill_size = 0;
    int audioReadSize = 0;


    // Allocate memory for the PCM audio data
    if ((pcmaudio = (short int *) calloc(AUDIO_BUF_SIZE, sizeof(short int))) == NULL) {
        fprintf(stderr, "Error: pcmaudio memory allocation failed\n");
        exit(ERR_MEM_ALLOC);
    }
    // Allocate memory for the encoded MP2 audio data
    if ((mp2buffer = (unsigned char *) calloc(MP2_BUF_SIZE, sizeof(unsigned char))) == NULL) {
        fprintf(stderr, "Error: mp2buffer memory allocation failed\n");
        exit(ERR_MEM_ALLOC);
    }
    // Initialise Encoder Options Structure 
    encopts = twolame_init();
    if (encopts == NULL) {
        fprintf(stderr, "Error: initializing libtwolame encoder failed.\n");
        exit(ERR_MEM_ALLOC);
    }
    // Get options and parameters from the command line
    parse_args(argc, argv, encopts);

    // Display the filenames
    print_filenames(twolame_get_verbosity(encopts));

    // Open the input file
    if (use_raw) {
        // use raw input handler
        inputfile = open_audioin_raw(inputfilename, &sfinfo, sample_size);
    } else {
        // use libsndfile
        inputfile = open_audioin_sndfile(inputfilename, &sfinfo);
    }

    // Display input information
    if (twolame_get_verbosity(encopts) > 1) {
        inputfile->print_info(inputfile);
    }
    // Use information from input file to configure libtwolame 
    twolame_set_num_channels(encopts, sfinfo.channels);
    twolame_set_in_samplerate(encopts, sfinfo.samplerate);


    // Open the output file
    outputfile = open_output_file(outputfilename);

    // initialise twolame with this set of options
    if (twolame_init_params(encopts) != 0) {
        fprintf(stderr, "Error: configuring libtwolame encoder failed.\n");
        exit(ERR_INVALID_PARAM);
    }
    // display encoder settings
    twolame_print_config(encopts);


    // Only encode a single frame of mpeg audio ?
    if (single_frame_mode)
        audioReadSize = TWOLAME_SAMPLES_PER_FRAME;
    else
        audioReadSize = AUDIO_BUF_SIZE;

    // Calculate the size and number of frames we are going to encode
    frame_len = twolame_get_framelength(encopts);
    if (sfinfo.frames)
        total_frames = sfinfo.frames / TWOLAME_SAMPLES_PER_FRAME;


    // Now do the reading/encoding/writing
    while ((samples_read = inputfile->read(inputfile, pcmaudio, audioReadSize)) > 0) {
        int bytes_out = 0;

        // Force byte swapping if requested
        if (byteswap) {
            int i;
            for (i = 0; i < samples_read; i++) {
                short tmp = pcmaudio[i];
                char *src = (char *) &tmp;
                char *dst = (char *) &pcmaudio[i];
                dst[0] = src[1];
                dst[1] = src[0];
            }
        }
        // Calculate the number of samples we have (per channel)
        samples_read /= sfinfo.channels;

        // Do swapping of left and right channels if requested
        if (channelswap && sfinfo.channels == 2) {
            int i;
            for (i = 0; i < samples_read; i++) {
                short tmp = pcmaudio[(2 * i)];
                pcmaudio[(2 * i)] = pcmaudio[(2 * i) + 1];
                pcmaudio[(2 * i) + 1] = tmp;
            }
        }
        // Encode the audio to MP2
        mp2fill_size =
            twolame_encode_buffer_interleaved(encopts, pcmaudio, samples_read, mp2buffer,
                                              MP2_BUF_SIZE);

        // Stop if we don't have any bytes (probably don't have enough audio for a full frame of
        // mpeg audio)
        if (mp2fill_size == 0)
            break;
        if (mp2fill_size < 0) {
            fprintf(stderr, "error while encoding audio: %d\n", mp2fill_size);
            exit(ERR_ENCODING);
        }
        // Check that a whole number of frame was written
        // if (mp2fill_size % frame_len != 0) {
        // fprintf(stderr,"error while encoding audio: non-whole number of frames written\n");
        // exit(ERR_ENCODING);
        // }

        // Write the encoded audio out
        bytes_out = fwrite(mp2buffer, sizeof(unsigned char), mp2fill_size, outputfile);
        if (bytes_out != mp2fill_size) {
            perror("error while writing to output file");
            exit(ERR_WRITING_OUTPUT);
        }
        total_bytes += bytes_out;

        // Only single frame ?
        if (single_frame_mode)
            break;


        // Display Progress
        frame_count += (mp2fill_size / frame_len);
        if (twolame_get_verbosity(encopts) > 0) {
            fprintf(stderr, "\rEncoding frame: %i", frame_count);
            if (total_frames) {
                fprintf(stderr, "/%i (%i%%)", total_frames, (frame_count * 100) / total_frames);
            }
            fflush(stderr);
        }
    }

    // Was there an error reading the audio?
    if (inputfile->error_str(inputfile)) {
        fprintf(stderr, "Error reading from input file: %s\n", inputfile->error_str(inputfile));
    }
    // 
    // flush any remaining audio. (don't send any new audio data) There
    // should only ever be a max of 1 frame on a flush. There may be zero
    // frames if the audio data was an exact multiple of 1152
    // 
    mp2fill_size = twolame_encode_flush(encopts, mp2buffer, MP2_BUF_SIZE);
    if (mp2fill_size > 0) {
        frame_count++;
        int bytes_out = fwrite(mp2buffer, sizeof(unsigned char), mp2fill_size, outputfile);
        if (bytes_out <= 0) {
            perror("error while writing to output file");
            exit(ERR_WRITING_OUTPUT);
        }
        total_bytes += bytes_out;
    }

    if (twolame_get_verbosity(encopts) > 1) {
        char *filesize = format_filesize_string(total_bytes);
        fprintf(stderr, "\nEncoding Finished.\n");
        fprintf(stderr, "Total bytes written: %s.\n", filesize);
        free(filesize);
    }
    // Close input and output streams
    inputfile->close(inputfile);
    fclose(outputfile);

    // Close the libtwolame encoder
    twolame_close(&encopts);


    // Free up memory
    free(pcmaudio);
    free(mp2buffer);

    return (ERR_NO_ERROR);
}

/* vim:ts=4:sw=4:nowrap: */
