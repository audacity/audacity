#define MAX_NAME_LEN 255

class Audio_file_reader : public Audio_reader {
 public:
    virtual long read(float *data, long n);
    SNDFILE *sf;
    SF_INFO sf_info;
    char name[MAX_NAME_LEN + 1];
    int bytes_per_frame;
    long total_frames;
    bool open(const char *filename, Scorealign &sa, bool verbose);
    void close();
    double get_sample_rate();
    long get_frames();
    void print_info();
};

