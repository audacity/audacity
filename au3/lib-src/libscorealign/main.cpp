/* main.cpp -- the command line interface for scorealign
 * 
 * 14-Jul-08 RBD
 */

#include "stdio.h"
#include "main.h"
#include <fstream>
#include "allegro.h"
#include "audioreader.h"
#include "scorealign.h"
#include "sautils.h"
#include "alignfiles.h"
#include "gen_chroma.h"
#include "comp_chroma.h"

// a global object with score alignment parameters and data
Scorealign sa;

static void print_usage(const char *progname) 
{
    printf("\nUsage: %s [-<flags> [<period> <windowsize> <path> <smooth> "
           "<trans> <midi> <beatmap> <image>]] <file0> [<file1>]\n", progname);
    printf("   specifying only <file0> simply transcribes MIDI in <file0> "
           "to\n");
    printf("   transcription.txt. Otherwise, align <file0> and <file1>.\n");
    printf("   -h 0.25 indicates a frame period of 0.25 seconds\n");
    printf("   -w 0.25 indicates a window size of 0.25 seconds.\n");
    printf("   -d 0.1 indicates a silence threshold of RMS=0.1.\n");
    printf("   -r indicates filename to write raw alignment path to "
           "(default path.data)\n");
    printf("   -s is filename to write smoothed alignment path(default is "
           "smooth.data)\n");
    printf("   -t is filename to write the time aligned transcription "
           "(default is transcription.txt)\n");
    printf("   -m is filename to write the time aligned midi file "
           "(default is midi.mid)\n");
    printf("   -b is filename to write the time aligned beat times "
           "(default is beatmap.txt)\n");
    printf("   -i is filename to write an image of the distance matrix "
           "(default is distance.pnm)\n");
    printf("   -o 2.0 indicates a smoothing window time of 2.0s\n");
    printf("   -p 3.0 indicates presmoothing with a 3s window\n");
    printf("   -x 6.0 indicates 6s line segment approximation\n");
#if (defined (_WIN32) || defined (WIN32))
    printf("   This is a Unix style command line application which\n"
           "   should be run in a MSDOS box or Command Shell window.\n\n");
    printf("   Type RETURN to exit.\n") ;
    getchar();
#endif
} /* print_usage */


/*				SAVE_SMOOTH_FILE 
	saves the smooth time map in SMOOTH_FILENAME

*/
void save_smooth_file(const char *smooth_filename, Scorealign &sa) {
    FILE *smoothf = fopen(smooth_filename, "w");
    assert(smoothf);
    for (int i = 0; i < sa.file0_frames; i++) {
        fprintf(smoothf, "%g \t%g\n", i * sa.actual_frame_period_0,
                sa.smooth_time_map[i] * sa.actual_frame_period_1);
    }
    fclose(smoothf);
}


/*				PRINT_BEAT_MAP
   prints the allegro beat_map (for debugging) which contain
   the time, beat pair for a song 
*/
void print_beat_map(Alg_seq &seq, const char *filename) {
    
    FILE *beatmap_print = fopen(filename, "w"); 
    
    Alg_beats &b = seq.get_time_map()->beats;
    long num_beats = seq.get_time_map()->length();
    
    for(int i = 0; i < num_beats; i++) { 
        fprintf(beatmap_print," %f  %f \n", b[i].beat, b[i].time); 
    }	
    fclose(beatmap_print); 
    
}


/*				EDIT_TRANSCRIPTION
	edit the allegro time map structure according
	to the warping and output a midi file and transcription
	file 

*/
void edit_transcription(Alg_seq &seq , bool warp, FILE *outf, 
                        const char *midi_filename, const char *beat_filename) {
    int note_x = 1;
    seq.convert_to_seconds();
    Alg_iterator iter(&seq, false); // no note-offs
    iter.begin();
    Alg_event_ptr e = iter.next();

    while (e) {
        if (e->is_note()) {
            Alg_note_ptr n = (Alg_note_ptr) e;
            fprintf(outf, "%d %ld %d %d ", 
                    note_x++, n->chan, ROUND(n->pitch), ROUND(n->loud));
            // now compute onset time mapped to audio time
            double start = n->time;
            double finish = n->time + n->dur;
            if (warp) {
                start = sa.map_time(start);
                finish = sa.map_time(finish);
            }
            fprintf(outf, "%.3f %.3f\n", start, finish-start);
        }
        e = iter.next();
    }
    iter.end();
    fclose(outf);
    if (warp) {
        // align the midi file and write out 	
        sa.midi_tempo_align(seq);
        seq.smf_write(midi_filename);
        print_beat_map(seq, beat_filename);
    }
}


// save image of distance matrix
void save_image(const char *image_filename, Scorealign &sa)
{
    FILE *outf = fopen(image_filename, "wb");
    if (!outf) {
        fprintf(stderr, "Error: could not open %s for write.\n", 
                image_filename);
    }
    float max_d = 0.0;
    float min_d = 999999.0;
    fputs("P5\n", outf);
    fprintf(outf, "%d %d 255\n", sa.file1_frames, sa.file0_frames);
    for (int row = 0; row < sa.file0_frames; row++) {
        for (int col = 0; col < sa.file1_frames; col++) {
            float d = sa.gen_dist(row, col);
#ifdef DEBUG_LOG
            fprintf(dbf, "%d %d %g\n", row, col, d);
#endif
            if (d > max_d) max_d = d;
            if (d < min_d) min_d = d;
            int pixel = (int) (255 * (d / 6.0) + 0.5);
            if (pixel > 255) pixel = 255;
            putc(pixel, outf);
        }
    }
    fclose(outf);
    printf("max distance %g, min distance %g\n", max_d, min_d);
}


/*		SAVE_TRANSCRIPTION
write note data corresponding to audio file

assume audio file is file 1 and midi file is file 2
so pathx is index into audio, pathy is index into MIDI

If warp is false, simply write a transcription of the midi file.

Every note has 6 fields separated by a space character. The fields are:
<sequence number> <channel> <pitch> <velocity> <onset> <duration> 
Where
   <sequence number> is just an integer note number, e.g. 1, 2, 3, ...
   <channel> is MIDI channel from 0 to 15 
   <pitch> is MIDI key number (60 = middle C)
   <velocity> is MIDI key velocity (1 to 127)
   <onset> is time in seconds, rounded to 3 decimal places (milliseconds)
   <duration> is time in seconds, rounded to 3 decimal places
*/
void save_transcription(const char *file0, const char *file1, 
                        bool warp, const char *filename, const char *smooth_filename, 
                        const char *midi_filename, const char *beat_filename)
{
    
    const char *midiname; //midi file to be read
    const char *audioname; //audio file to be read
    
    if (warp) save_smooth_file(smooth_filename, sa); 
    
    //If either is a midifile
    if (is_midi_file(file0) || is_midi_file(file1)) {
	
        if (is_midi_file(file0)) {
            midiname=file0;
            audioname=file1;
        } else {
            midiname=file1;
            audioname=file0;
        }
	
        Alg_seq seq(midiname, true);
	
        FILE *outf = fopen(filename, "w");
        if (!outf) {
            printf("Error: could not open %s\n", filename);
            return;
        }
        fprintf(outf, "# transcription of %s\n", midiname);
        if (warp) {
            fprintf(outf, "# note times are aligned to %s\n", audioname);
        } else {
            fprintf(outf, "# times are unmodified from those in MIDI file\n");
        }
        fprintf(outf, "# transcription format : <sequence number> "
                "<channel> <pitch> <velocity> <onset> <duration>\n");
        
        edit_transcription(seq, warp, outf, midi_filename, beat_filename); 
    }
}


/*		SAVE_PATH
	write the alignment path to FILENAME
*/
void save_path(const char *filename, int pathlen, short* pathx, short *pathy,
               float actual_frame_period_0, float actual_frame_period_1)
{
    // print the path to a (plot) file
    FILE *pathf = fopen(filename, "w");
    assert(pathf);
    int p;
    for (p = 0; p < pathlen; p++) {
        fprintf(pathf, "%g %g\n", pathx[p] * actual_frame_period_0, 
                pathy[p] * actual_frame_period_1);
    }
    fclose(pathf);
}


/*			
	Prints the chroma table (for debugging)
*/

void print_chroma_table(const float *chrom_energy, int frames)
{
    int i, j;
    for (j = 0; j < frames; j++) {
        for (i = 0; i <= CHROMA_BIN_COUNT; i++) {
            printf("%5.2f | ", AREF2(chrom_energy, j, i));
        }
        printf("\n");
    }
}


int main(int argc, char *argv []) 
{	
    const char *progname, *infilename0, *infilename1;
    const char *smooth_filename, *path_filename, *trans_filename;
    const char *midi_filename, *beat_filename, *image_filename;
    
    //just transcribe if trasncribe == 1
    int transcribe = 0;
	
    // Default for the user definable parameters
    
    path_filename = "path.data";
    smooth_filename = "smooth.data";
    trans_filename = "transcription.txt";
    midi_filename = "midi.mid";
    beat_filename = "beatmap.txt";
    image_filename = "distance.pnm";

    progname = strrchr(argv [0], '/'); 
    progname = progname ? progname + 1 : argv[0] ;

    // If no arguments, return usage 
    if (argc < 2) {
        print_usage(progname);
        return 1;
    }

	

    /*******PARSING CODE BEGINS*********/
    int i = 1; 
    while (i < argc) {
        //expected flagged argument
        if (argv[i][0] == '-') {
            char flag = argv[i][1];
            if (flag == 'h') {
                sa.frame_period = atof(argv[i+1]);	
            } else if (flag == 'w') {
                sa.window_size = atof(argv[i+1]); 
            } else if (flag == 'r') {
                path_filename = argv[i+1];
            } else if (flag == 's') {
                smooth_filename = argv[i+1];
            } else if (flag == 't') {
                trans_filename = argv[i+1]; 
            } else if (flag == 'm') {
                midi_filename = argv[i+1];
            } else if (flag == 'i') {
                image_filename = argv[i+1];
            } else if (flag == 'b') {
                beat_filename = argv[i+1];
            } else if (flag == 'o') {
                sa.smooth_time = atof(argv[i+1]);
            } else if (flag == 'p') {
                sa.presmooth_time = atof(argv[i+1]);
            } else if (flag == 'x') {
                sa.line_time = atof(argv[i+1]);
            } else if (flag == 'd') {
                sa.silence_threshold = atof(argv[i+1]);
            }
            i++;
        }
        // When aligning audio to midi we must force file0 to be midi 
        else {			
            // file 1 is midi
            if (transcribe == 0) {
                infilename0 = argv[i];
                transcribe++;
            }
            // file 2 is audio or a second midi 
            else {
                infilename1 = argv[i];
                transcribe++;
            }	
        }
        i++;
    }
    /**********END PARSING ***********/
    if (sa.presmooth_time > 0 && sa.line_time > 0) {
        printf("WARNING: both -p and -x options selected.\n");
    }

    if (transcribe == 1) {
	// if only one midi file, just write transcription and exit, 
        // no alignment
        save_transcription(infilename0, "", false, trans_filename,NULL, NULL, NULL);
        printf("Wrote %s\n", trans_filename);
        goto finish;
    }


    // if midi only in infilename1, make it infilename0
    if (is_midi_file(infilename1) && !is_midi_file(infilename0)) {
        const char *temp; 
        temp = infilename0; 
        infilename0 = infilename1;
        infilename1 = temp;
    }

    if (!align_files(infilename0, infilename1, sa, true /* verbose */)) {
        printf("An error occurred, not saving path and transcription data\n");
        goto finish;
    }
    if (sa.file0_frames <= 2 || sa.file1_frames <= 2) {
        printf("Error: file frame counts are low: %d (for input 1) and %d "
               "for input 2)\n...not saving path and transcription data\n",
               sa.file0_frames, sa.file1_frames);
    goto finish;
    }
    // save path
    save_path(path_filename, sa.pathlen, sa.pathx, sa.pathy, 
              sa.actual_frame_period_0, sa.actual_frame_period_1);
    // save image of distance matrix
    save_image(image_filename, sa);
    // save smooth, midi, transcription
    save_transcription(infilename0, infilename1, true, trans_filename, 
                       smooth_filename, midi_filename, beat_filename);

    // print what the chroma matrix looks like
    /*
      printf("file0 chroma table: \n"); 
      print_chroma_table(chrom_energy0,file0_frames);
      printf("\nfile1 chroma table: \n"); 
      print_chroma_table(chrom_energy1, file1_frames); 
    */
	
    // only path and smooth are written when aligning two audio files
    if (is_midi_file(infilename0) || is_midi_file(infilename1))
        printf("Wrote %s, %s, %s, and %s.\n", path_filename, smooth_filename, 
               trans_filename, beat_filename);
    else
        printf("Wrote %s and %s.", path_filename, smooth_filename); 
    
finish:

#ifdef WIN32
    printf("Type RETURN to exit\n");
    getchar();
#endif

    return 0 ;
} /* main */


/* print_path_range -- debugging output */
/**/
void print_path_range(const short *pathx, const short *pathy, int i, int j)
{
    while (i <= j) {
        printf("%d %d\n", pathx[i], pathy[i]);
        i++;
    }
}


