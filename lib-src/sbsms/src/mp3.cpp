# include <stdio.h>
//# include <unistd.h>
# include <sys/stat.h>

#include "mp3tech.h"
#include "audio.h"
#include "import.h"
#include "audiobuffer.h"

#include <iostream>
#include <fstream>

#include "mp3.h"

namespace _sbsms_ {
#define MP3_INPUT_BUFFER_SIZE 4096
#define MP3_OUTPUT_BUFFER_SIZE 4096
#define MP3_OUTPUT_OVERFLOW_BUFFER_SIZE 4096
#define MP3_AUDIO_BUFFER_SIZE 65536

using namespace std;

static
enum mad_flow input(void *_data,
		    struct mad_stream *stream)
{
  MP3Reader *data = (MP3Reader*) _data;
  long input_buffer_size = MP3_INPUT_BUFFER_SIZE;

  if(data->file->eof()) {
    data->rb->writingComplete();
    return MAD_FLOW_STOP;
  }

   /* "Each time you refill your buffer, you need to preserve the data in
    *  your existing buffer from stream.next_frame to the end.
    *
    *  This usually amounts to calling memmove() on this unconsumed portion
    *  of the buffer and appending new data after it, before calling
    *  mad_stream_buffer()"
    *           -- Rob Leslie, on the mad-dev mailing list */

  unsigned int unconsumedBytes;
  if(stream->next_frame) {
    unconsumedBytes = data->inputBuffer + input_buffer_size - stream->next_frame;
    memmove(data->inputBuffer, stream->next_frame, unconsumedBytes);
  }
  else
    unconsumedBytes = 0;

  //use read instead of readsome so eof is reached
  long start = data->file->tellg();
  data->file->read((char*)data->inputBuffer + unconsumedBytes,
		   (input_buffer_size - unconsumedBytes));
  long end = data->file->tellg();
  long read = end - start;

  mad_stream_buffer(stream, data->inputBuffer, read + unconsumedBytes);
  
  return MAD_FLOW_CONTINUE;
}


inline float scale(mad_fixed_t sample)
{
   return (float) (sample / (float) (1L << MAD_F_FRACBITS));
}

static long copy_mad_audio(float *out, struct mad_pcm *pcm, unsigned int nsamples) 
{
  unsigned int nchannels = pcm->channels;

  mad_fixed_t *ch;
  ch = pcm->samples[0];
  
  for(unsigned int k = 0;k<nsamples;k++) {
    out[2*k] = scale( *(ch++) ); 
  }
  if(nchannels==2) {
    ch = pcm->samples[1];
    for(unsigned int k = 0;k<nsamples;k++) {
      out[2*k+1] = scale( *(ch++) ); 
    }
  } else {
    ch = pcm->samples[0];
    for(unsigned int k = 0;k<nsamples;k++) {
      out[2*k+1] = scale( *(ch++) ); 
    }
  }
  return nsamples;
}

/*
 * This is the output callback function. It is called after each frame of
 * MPEG audio data has been completely decoded. The purpose of this callback
 * is to output (or play) the decoded PCM audio.
 */

static
enum mad_flow output(void *_data,
		     struct mad_header const *header,
		     struct mad_pcm *pcm)
{
  MP3Reader *data = (MP3Reader*) _data;
  data->channels = pcm->channels;
  audio_in_cb cb = data->cb;
  unsigned int nsamples = pcm->length;
  float *out = data->getOutputBuffer(nsamples);
  copy_mad_audio(out,pcm,nsamples);

  if(cb) {
    cb(out,nsamples,pcm->samplerate,data->data); 
  }
  return MAD_FLOW_CONTINUE;
}

/*
 * This is the error callback function. It is called whenever a decoding
 * error occurs. The error is indicated by stream->error; the list of
 * possible MAD_ERROR_* errors can be found in the mad.h (or stream.h)
 * header file.
 */

static
enum mad_flow error(void *_data,
		    struct mad_stream *stream,
		    struct mad_frame *frame)
{
  fprintf(stderr, "decoding error 0x%04x (%s) at stream frame %u\n",
	  stream->error, mad_stream_errorstr(stream),
	  (unsigned int)stream->this_frame );

  /* return MAD_FLOW_BREAK here to stop decoding (and propagate an error) */
  return MAD_FLOW_CONTINUE;
}



/*
 * It instantiates a decoder object and configures it with the input,
 * output, and error callback functions above. A single call to
 * mad_decoder_run() continues until a callback function returns
 * MAD_FLOW_STOP (to stop decoding) or MAD_FLOW_BREAK (to stop decoding and
 * signal an error).
 */


int read_cb(float *buf, long n, int Fs, void *data) 
{
  MP3Reader *decoder = (MP3Reader*)data;
  return decoder->rb->write(buf,n);
}

void *mp3importThreadCB(void *data) 
{
  /* start decoding */
  MP3Reader *reader = (MP3Reader*)data;
  mad_decoder_run(&(reader->decoder), MAD_DECODER_MODE_SYNC);
  reader->bDone = true;
  return NULL;
}

long MP3Reader :: read(float *outputBuffer, long block_size) 
{
  // on first call, start the decoder
  if(bFirst) {
    bFirst = false;
    this->data = this;
    this->cb = read_cb;
    pthread_create(&importThread, NULL, mp3importThreadCB, this);
  }  

  return rb->read(outputBuffer, block_size);
}

float* MP3Reader :: getOutputBuffer(long nsamples)
{
  return outputBuffer;
}

bool MP3Reader :: done()
{
  return bDone;
}

bool MP3Reader :: isError()
{
  return bError;
}

MP3Reader :: MP3Reader(const char *filename) 
{
  bError = false;
  file = new ifstream();
  file->open(filename, ios::in | ios::binary);
  if (!file->is_open()) {
    bError = true;
  }

  mp3info mp3;
  mp3.file = fopen(filename,"rb");
  mp3.filename = filename;
  get_mp3_info(&mp3);
  this->samples = mp3.samples;
  this->sampleRate = mp3.sample_rate;
  fclose(mp3.file);

  this->n_done = 0;
  this->bFirst = true;
  this->bDone = false;

  /* configure input, output, and error functions */
  mad_decoder_init(&decoder, this,
		   input, 0 /* header */, 0 /* filter */, output,
		   error, 0 /* message */);

  this->outputBuffer = new float[MP3_OUTPUT_BUFFER_SIZE<<1];
  this->inputBuffer = new unsigned char [MP3_INPUT_BUFFER_SIZE];
  this->rb = new AudioBuffer(MP3_AUDIO_BUFFER_SIZE,2);
}

long MP3Reader :: getFrames()
{
  return samples;
}

int MP3Reader :: getSampleRate()
{
  return sampleRate;
}

int MP3Reader :: getChannels()
{
  return 2;
}

MP3Reader :: ~MP3Reader()
{
  file->close();  
  delete file;
  mad_decoder_finish(&decoder);
  delete outputBuffer;
  delete inputBuffer; 
  delete rb;
}

}
