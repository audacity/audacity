#include <stdlib.h>
#include <stdio.h>

#include "pcm.h"
#include "utils.h"

namespace _sbsms_ {
long PcmReader :: read(float *buf, long block_size) {
  if(info.channels == 1) {
    float srcbuf[2*PCM_READ_BUF_SIZE];
    long nread = -1;
    long nreadTotal = 0;
    while(nreadTotal < block_size && nread) {
      long ntoread = min((long)PCM_READ_BUF_SIZE,block_size-nreadTotal);
      nread = sf_readf_float(in, srcbuf, ntoread);
      for(int i=0;i<nread;i++) {
	int i2 = (nreadTotal+i)<<1;
	buf[i2] = buf[i2+1] = srcbuf[i];
      }
      nreadTotal += nread;
    }
    if(nreadTotal == 0)
      bDone = true;
    return nreadTotal;
  } else if(info.channels == 2) {
    long nread = sf_readf_float(in, buf, block_size);
    if(nread == 0)
      bDone = true;
    return nread;
  } else {
    abort();
	return 0;
  }
}

bool PcmReader :: done()
{
  return bDone;
}

PcmReader :: PcmReader(const char *filename) 
{
  total = 0;
  bDone = false;
  in = sf_open(filename, SFM_READ, &info);
  
  bError = false;
  if (!in) {
    perror("cannot open file for reading");
    bError = true;
  }
}

bool PcmReader :: isError()
{
  return bError;
}

int PcmReader :: getSampleRate()
{
  return info.samplerate;
}

long PcmReader :: getFrames()
{
  return info.frames;
}

int PcmReader :: getChannels()
{
  return info.channels;
}

PcmReader :: ~PcmReader() 
{  
  sf_close(in);
}

PcmWriter :: PcmWriter(const char *filename, long size, int samplerate, int channels) 
{

  total = 0;
  info.format = SF_FORMAT_WAV | SF_FORMAT_FLOAT;
  info.frames = size;
  info.samplerate = samplerate;
  info.channels = channels;
  info.sections = 1;
  info.seekable = 0;

  if (!sf_format_check(&info))
    info.format = (info.format & SF_FORMAT_TYPEMASK);

  out = sf_open(filename, SFM_WRITE, &info);

  bError = false;
  if (!sf_format_check(&info)) {
    bError = true;
    perror("bad format for writing pcm");
  }
  if (!out) {
    perror("cannot open file for writing");
    bError = true;
  }
}

bool PcmWriter :: isError()
{
  return bError;
}

long PcmWriter :: write(float *data, long n)
{
  return sf_writef_float(out, (float *)data, n);
}
 
PcmWriter :: ~PcmWriter()
{
}

void PcmWriter :: close() 
{
  if(out) sf_close(out);
}

}
