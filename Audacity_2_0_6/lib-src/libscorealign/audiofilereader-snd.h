class Audio_file_reader : public Audio_reader {
 public:
    virtual long read(float *data, long n);
    snd_node snd;
    int bytes_per_frame;
    bool open(char *filename, Scorealign &sa, bool verbose);
    void close();
    snd_type get_snd() { return &snd; }
    double Audio_file_reader::get_sample_rate();
    long get_frames();
    void print_info();
};

