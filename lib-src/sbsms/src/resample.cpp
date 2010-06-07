#include "sbsms.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "sincCoeffs.h"
#include "real.h"
#include "utils.h"
#include <algorithm>
using namespace std;

namespace _sbsms_ {

#define SBSMS_RESAMPLE_CHUNK_SIZE 8192L

Resampler :: Resampler(sbsms_resample_cb cb, void *data)
{
  init();
  this->cb = cb;
  this->data = data;
  bPull = true;
  bInput = true;
  frame.size = 0;
}

Resampler :: Resampler(SampleBuf *in, real ratio)
{
  init();
  bPull = false;
  this->in = in;
  bInput = true;
  frame.ratio0 = ratio;
  frame.ratio1 = ratio;
}

void Resampler :: init()
{
  inOffset = 0;
  startAbs = 0;
  midAbs = 0;
  endAbs = 0;
  writePosAbs = 0;
  midAbsf = 0.0;
  out = new SampleBuf(0);
  bWritingComplete = false;
  sincZeros = SBSMS_SINC_SAMPLES;
}

void Resampler :: reset()
{
  delete out;
  init();
  frame.size = 0;
  bInput = true;
}

long Resampler :: read(audio *audioOut, long samples)
{
  if(!bPull) {
    frame.in = in->getReadBuf();
    frame.size = in->n_readable();
    if(frame.size) bInput = true;
  }

  long nRead = out->n_readable();
  while(nRead < samples && bInput) {
    if(bInput && inOffset == frame.size) {
      if(!bPull) {
	in->advance(frame.size);
	bInput = false;
      } else {
	cb(data,&frame);
	if(!frame.size) 
	  bWritingComplete = true;
      }
      if(bWritingComplete) {
	bInput = false;
	out->grow(midAbs - writePosAbs);
	out->writePos += midAbs - writePosAbs;
	out->delay = 0;
      }
      inOffset = 0;
    }
    if(frame.size) {
      real dratio = 1.0f/(real)frame.size*(frame.ratio1-frame.ratio0);

      real ratio = frame.ratio0 + (real)inOffset*dratio;
      real ratiorec = ratio<1.0f?1.0f:1.0f/ratio;
      real f = ratiorec*SBSMS_SINC_RES;
      int fi = round2int(f-0.5f);
      real ff = f - fi;
      if(ff<0.0f) {
	ff += 1.0f;
	fi--;
      }
      real scale = ratio<1.0f?ratio:1.0f;
      int maxDist = round2int(sincZeros*ratio-0.5f);
      // absolute start position
      startAbs = max((long)0,midAbs-maxDist);
      // samples to advance
      long advance = max((long)0,startAbs - maxDist - writePosAbs);
      writePosAbs += advance;
      // absolute end position
      endAbs = midAbs + maxDist;
      // starting position in output
      int start = startAbs - writePosAbs;
      assert(start>=0);
      // zero point in output
      int mid = midAbs - writePosAbs;
      // ending position in output
      int end = endAbs - writePosAbs;
      // provide extra delay for variable rate ratio
      out->delay = maxDist<<1;
      out->writePos += advance;

      if(fabs(ratio-1.0f) < 1e-6f && fabs(dratio) < 1e-8f ) {
	// how far ahead to write
	int nAhead = mid+frame.size;
	out->N = nAhead;
	out->grow(nAhead);
	long nWrite = min(SBSMS_RESAMPLE_CHUNK_SIZE,frame.size-inOffset);
	for(int j=0;j<nWrite;j++) {
	  out->buf[out->writePos+mid+j][0] += frame.in[j+inOffset][0];
	  out->buf[out->writePos+mid+j][1] += frame.in[j+inOffset][1];
	}
	inOffset += nWrite;
	midAbsf += ratio*nWrite;
	int nWritten = round2int(midAbsf);
	midAbsf -= nWritten;
	midAbs += nWritten;
      } else {
	long nWrite = min(SBSMS_RESAMPLE_CHUNK_SIZE,frame.size-inOffset);
	audio *i = &(frame.in[inOffset]);
	for(int j=0;j<nWrite;j++) {
	  // how far ahead to write
	  int nAhead = end;
	  out->N = nAhead;
	  out->grow(nAhead);
	  audio *o = &(out->buf[out->writePos+start]);
	  real d = (start-mid-midAbsf)*f;
	  int di = round2int(d-0.5f);
	  real df = d-di;
	  if(df<0.0f) {
	    df += 1.0f;
	    di--;
	  }
	  real i0 = (*i)[0];
	  real i1 = (*i)[1];
	  for(int k=start;k<end;k++) {
	    int k0 = (di<0)?-di:di; 
	    int k1 = (di<0)?k0-1:k0+1;
	    real sinc;
	    if(k1>=SBSMS_SINC_SIZE) {
	      if(k0>=SBSMS_SINC_SIZE) {
		sinc = 0.0f;
	      } else {
		sinc = scale*sincTable[k0];
	      }
	    } else if(k0>=SBSMS_SINC_SIZE) {
	      sinc = scale*sincTable[k1];
	    } else {
	      sinc = scale*((1.0f-df)*sincTable[k0] + df*sincTable[k1]);
	    }
	    (*o)[0] += i0 * sinc;
	    (*o)[1] += i1 * sinc;
	    di += fi;
	    df += ff;
	    if(!(df<1.0f)) {
	      df -= 1.0f;
	      di++;
	    }
	    o++;
	  }
	  i++;
	  midAbsf += ratio;
	
	  if(dratio != 0.0f) {
	    ratio += dratio;
	    ratiorec = ratio<1.0f?1.0f:1.0f/ratio;
	    f = ratiorec*SBSMS_SINC_RES;
	    fi = round2int(f-0.5f);
	    ff = f - fi;
	    if(ff<0.0f) {
	      ff += 1.0f;
	      fi--;
	    }
	    scale = ratio<1.0f?ratio:1.0f;
	    maxDist = round2int(sincZeros*ratio-0.5f);
	  }

	  int nWritten = round2int(midAbsf);
	  midAbsf -= nWritten;
	  midAbs += nWritten;
	  startAbs = max((long)0,midAbs-maxDist);
	  endAbs = midAbs + maxDist;
	  start = startAbs - writePosAbs;
	  mid = midAbs - writePosAbs;
	  end = endAbs - writePosAbs;
	}
	inOffset += nWrite;
      }
    }
    nRead = out->n_readable();
  }

  nRead = min(samples,out->n_readable());
  if(nRead) {
    out->read(audioOut,nRead);
    out->advance(nRead);
  }

  return nRead;
}

long Resampler :: samplesInOutput()
{
  long samplesFromBuffer = round2int(0.5f*(frame.ratio0+frame.ratio1)*(frame.size-inOffset));
  return out->writePos + midAbs - writePosAbs - out->readPos + samplesFromBuffer;
}

void Resampler :: writingComplete()
{
  bWritingComplete = true;
}

Resampler :: ~Resampler()
{
  delete out;
}

}
