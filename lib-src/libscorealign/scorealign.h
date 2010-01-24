/* scorealign.h 
 *
 * RBD
 */

// turn on lots of debugging, comment this line out to disable
// #define SA_VERBOSE 1

#ifdef SA_VERBOSE
#define SA_V(stmt) stmt
#else
#define SA_V(stmt) 
#endif


class Scorealign {
 public:
    float frame_period; // time in seconds
    float window_size;
    float presmooth_time;
    float line_time;
    float smooth_time; // duration of smoothing window
    int smooth; // number of points used to compute the smooth time map

    Scorealign() {
        frame_period = 0.25;
        window_size = 0.25;
        presmooth_time = 0.0;
        line_time = 0.0;
        smooth_time = 1.75;
        pathlen = 0;
        path_count = 0;
        pathx = NULL;
        pathy = NULL;
    }

    ~Scorealign() {
        if (pathx) free(pathx);
        if (pathy) free(pathy);
    }

    // chromagrams and lengths, path data
    float *chrom_energy1;
    int file1_frames; // number of frames in file1
    float *chrom_energy2;
    int file2_frames; //number of frames in file2
    short *pathx;  //for midi (when aligning midi and audio)
    short *pathy; //for audio (when aligning midi and audio)
    int pathlen;
    void set_pathlen(int p) { pathlen = p; }
    float *time_map;
    float *smooth_time_map;

    // chroma vectors are calculated from an integer number of samples
    // that approximates the nominal frame_period. Actual frame period
    // is calculated and stored here:
    // time in seconds for midi (when aligning midi and audio)
    float actual_frame_period_1; 
    // time in seconds for audio (when aligning midi and audio)
    float actual_frame_period_2; 

    /* gen_chroma.cpp stuff:
       generates the chroma energy for a given file
       with a low cutoff and high cutoff.  
       The chroma energy is placed in the float** chrom_energy.
       this 2D is an array of pointers.  the pointers point to an array 
       of length 12, representing the 12 chroma bins
       The function returns the number of frames 
       (i.e. the length of the 1st dimention of chrom_energy
    */
    int gen_chroma_audio(Audio_reader &reader, int hcutoff, int lcutoff, 
                         float **chrom_energy, float *actual_frame_period,
                         int id, bool verbose);

    int gen_chroma_midi(Alg_seq &seq,  int hcutoff, int lcutoff, 
                        float **chrom_energy, float *actual_frame_period,
                        int id, bool verbose);

    /* scorealign.cpp stuff: */
    float map_time(float t1);
    void midi_tempo_align(Alg_seq &seq , char *midiname, char *beatname);
    void align_midi_to_audio(Alg_seq &seq, Audio_reader &reader, 
                            bool verbose);
    void align_midi_to_midi(Alg_seq &seq1, Alg_seq &seq2, bool verbose);
    void align_audio_to_audio(Audio_reader &reader1, 
                             Audio_reader &reader2, bool verbose);
    void align_chromagrams(bool verbose);

    int path_count; // for debug log formatting
    void path_step(int i, int j);
    void path_reverse();
    int sec_to_pathy_index(float sec);
    void compare_chroma(bool verbose);
    void linear_regression(int n, int width, float &a, float &b);
    void compute_smooth_time_map();
    void presmooth();
    void compute_regression_lines();
    void midi_tempo_align(Alg_seq &seq, bool verbose);
};

#define DEBUG_LOG 0
#if DEBUG_LOG
extern FILE *dbf;
#endif

int find_midi_duration(Alg_seq &seq, float *dur);
