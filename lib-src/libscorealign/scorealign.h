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

// a class to report (optionally) score alignment progress
class SAProgress /* not final */ {
  public:
    SAProgress() { smoothing = false; }
    // we need the frame period to convert seconds to work units
    // call this before set_duration()
    virtual void set_frame_period(double seconds) { frame_period = seconds; };
    // index = 0 or 1 to tell which file (first or second)
    // is_audio = true (audio) or false (midi)
    // seconds = duration of audio or midi data
    virtual void set_duration(int index, bool audio_flag, double seconds) { 
        durations[index] = seconds;
        is_audio[index] = audio_flag; };
    // if fitting pwl path to path, set smoothing to true
    virtual void set_smoothing(bool s) { smoothing = s; }
    // which alignment phase are we working on?
    // 0 = first file chroma, 1 = second file chroma, 2 = compute matrix,
    // 3 = smoothing
    // Note: set_phase(0) is REQUIRED and must be called only ONCE. 
    // This is when we calculate total work
    // and initialize any local state needed to handle set_feature_progress()
    // and set_matrix_progress().
    virtual void set_phase(int i) { phase = i; };
    // how many seconds have we processed (in phase 1 or 2)
    // return value is normally true; false is request to cancel
    virtual bool set_feature_progress(float seconds) { return true; };
    // report that some matrix elements have been computed?
    // return value is normally true; false is request to cancel
    virtual bool set_matrix_progress(int cells) { return true; };
    // report iterations of line smoothing
    virtual bool set_smoothing_progress(int i) { return true; };
  protected:
    double frame_period;
    int phase;
    double durations[2];
    bool is_audio[2];
    bool smoothing;
};


enum {
  SA_SUCCESS = 0,
  SA_TOOSHORT,
  SA_CANCEL
};


#define SA_DFT_FRAME_PERIOD 0.2
#define SA_DFT_FRAME_PERIOD_TEXT wxT("0.20 secs")

#define SA_DFT_WINDOW_SIZE 0.2
#define SA_DFT_WINDOW_SIZE_TEXT wxT("0.20 secs")

#define SA_DFT_FORCE_FINAL_ALIGNMENT true
#define SA_DFT_FORCE_FINAL_ALIGNMENT_STRING wxT("true")

#define SA_DFT_IGNORE_SILENCE true
#define SA_DFT_IGNORE_SILENCE_STRING wxT("true")

#define SA_DFT_SILENCE_THRESHOLD 0.1
#define SA_DFT_SILENCE_THRESHOLD_TEXT wxT("0.100")

#define SA_DFT_PRESMOOTH_TIME 0
#define SA_DFT_PRESMOOTH_TIME_TEXT wxT("(off)")

#define SA_DFT_LINE_TIME 0
#define SA_DFT_LINE_TIME_TEXT wxT("(off)")

#define SA_DFT_SMOOTH_TIME 1.75
#define SA_DFT_SMOOTH_TIME_TEXT wxT("1.75 secs")


class Scorealign {
 public:
    double frame_period; // time in seconds
    double window_size;
    double silence_threshold;
    bool force_final_alignment;
    bool ignore_silence;
    double presmooth_time;
    double line_time;
    double smooth_time; // duration of smoothing window
    int smooth; // number of points used to compute the smooth time map

    Scorealign();
    ~Scorealign();

    SAProgress *progress;
    bool verbose;

    // chromagrams and lengths, path data
    float *chrom_energy0;
    int file0_frames; // number of frames in file0
    float *chrom_energy1;
    int file1_frames; //number of frames in file1
    // pathx, pathy, and pathlen describe the shortest path through the
    // matrix from first_x, first_y to last_x, last_y (from the first
    // non-silent frame to the last non-silent frame). The length varies
    // depending upon the amount of silence that is ignored and how many
    // path steps are diagonal.
    short *pathx;  //for midi (when aligning midi and audio)
    short *pathy; //for audio (when aligning midi and audio)
    int pathlen;
    // first_x, first_y, last_x, last_y are the starting and ending
    // points of the path. (It's not 0, 0, file0_frames, file1_frames
    // because silent frames may be trimmed from beginning and ending.
    int first_x;
    int first_y;
    int last_x;
    int last_y;

    void set_pathlen(int p) { pathlen = p; }
    // time_map is, for each sequence 0 frame, the time of the matching
    // frame in sequence 1. If the path associates a frame of sequence 0
    // with multiple frames in sequence 1, the sequence 1 frame times
    // are averaged. The frames that are not mapped to sequence 1 are
    // marked with a time of -9999 or NOT_MAPPED. 
    // These will be silent frames of sequence 0.
#define NOT_MAPPED -9999.0F
    float *time_map;
    // smooth_time_map is a smoothed version of time_map. It also has
    // non-mapped frames marked with times of -9999 or NOT_MAPPED.
    // Because of smoothing, frames in smooth_time_map may map to 
    // negative times in sequence 1.
    // These negative times will not be as negative as -9999, but
    // the recommended coding style is to compare for equality with
    // NOT_MAPPED to test for that value.
    float *smooth_time_map;

    // chroma vectors are calculated from an integer number of samples
    // that approximates the nominal frame_period. Actual frame period
    // is calculated and stored here:
    // time in seconds for midi (when aligning midi and audio)
    double actual_frame_period_0; 
    // time in seconds for audio (when aligning midi and audio)
    double actual_frame_period_1; 

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
                         float **chrom_energy, double *actual_frame_period,
                         int id);

    int gen_chroma_midi(Alg_seq &seq, float dur, int nnotes, 
                        int hcutoff, int lcutoff,
                        float **chrom_energy, double *actual_frame_period,
                        int id);

    /* comp_chroma.cpp stuff */
    /*				GEN_DIST
     *
     * This function generates the Euclidean distance for points i
     * and j in two chroma vectors for use with dynamic time warping of 
     * the chroma vectors.
     */
    float gen_dist(int i, int j);

    /* scorealign.cpp stuff: */
    float map_time(float t1);
    int align_midi_to_audio(Alg_seq &seq, Audio_reader &reader);
    int align_midi_to_midi(Alg_seq &seq0, Alg_seq &seq2);
    int align_audio_to_audio(Audio_reader &reader1, Audio_reader &reader2);
    int align_chromagrams();

    int path_count; // for debug log formatting
    void path_step(int i, int j);
    void path_reverse();
    int sec_to_pathy_index(float sec);
    int compare_chroma();
    void linear_regression(int n, int width, float &a, float &b);
    void compute_smooth_time_map();
    void presmooth();
    void compute_regression_lines();
    void midi_tempo_align(Alg_seq &seq);
};

// #define DEBUG_LOG 1
#if DEBUG_LOG
extern FILE *dbf;
#endif

int find_midi_duration(Alg_seq &seq, float *dur);
