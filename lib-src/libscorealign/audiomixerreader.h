/* audiomixerreader.h -- reads samples from an Audacity mixer on behalf of scorealign
 *
 * 21-Jul-08 RBD
 */

#define AMR_BUFFER_FRAMES 4096

class Audio_mixer_reader : public Audio_reader {
public:
  virtual long read(float *data, long n);
  // Mixer *mixer;
  void *mixer;
  mixer_process_fn mixer_process;
  float *buffer;
  int channels;
  long buffer_len; // number of samples pointed to by buffer
  long index; // index into buffer
  long total_frames; // number of frames in input audio 
                     // (returned by get_frames)
  double sample_rate;
  Audio_mixer_reader(void *mixer, mixer_process_fn fn_ptr, int chans, 
                     double srate, double end_time);
  void close();
  double get_sample_rate();
  long get_frames();
  void print_info();
};