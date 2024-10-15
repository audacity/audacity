class Scorealign;

class Audio_reader {
 public:
    long samples_per_frame;
    long hop_samples;
    double actual_frame_period;
    long frame_count; // number of chroma vectors (analysis windows)
    virtual void print_info() = 0;
    long read_window(float *data);
    virtual long read(float *data, long n) = 0;
    virtual double get_sample_rate() = 0;
    virtual long get_frames() = 0; // returns frames of input audio 
    // i.e. (samples/channels)
    void calculate_parameters(Scorealign &sa, bool verbose);
    Audio_reader() {
        reading_first_window = true;
        reading_last_window = false;
        temp_data = NULL;
    }
    ~Audio_reader() {
        if (temp_data) free(temp_data);
    }
 protected:
    bool reading_first_window;
    bool reading_last_window;
    float *temp_data;
};

