/***************************************************/
/*! \class WvIn
    \brief STK audio input abstract base class.

    This class provides common functionality for a variety of audio
    data input subclasses.

    WvIn supports multi-channel data.  It is important to distinguish
    the tick() methods, which return samples produced by averaging
    across sample frames, from the tickFrame() methods, which return
    references or pointers to multi-channel sample frames.

    Both interleaved and non-interleaved data is supported via the use
    of StkFrames objects.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "WvIn.h"
#include <cmath>

using namespace Nyq;

WvIn :: WvIn()
{
}

WvIn :: ~WvIn()
{
}

StkFloat WvIn :: lastOut( void ) const
{
  if ( lastOutputs_.empty() ) return 0.0;

  if ( lastOutputs_.size() == 1 )
    return lastOutputs_[0];

  StkFloat output = 0.0;
  for ( unsigned int i=0; i<lastOutputs_.size(); i++ ) {
    output += lastOutputs_[i];
  }
  return output / lastOutputs_.size();
}

StkFloat WvIn :: tick( void )
{
  computeFrame();
  return lastOut();
}

StkFrames& WvIn :: tick( StkFrames& frames, unsigned int channel )
{
  if ( channel >= frames.channels() ) {
    errorString_ << "WvIn::tick(): channel and StkFrames arguments are incompatible!";
    handleError( StkError::FUNCTION_ARGUMENT );
  }

  if ( frames.channels() == 1 ) {
    for ( unsigned int i=0; i<frames.frames(); i++ )
      frames[i] = tick();
  }
  else if ( frames.interleaved() ) {
    unsigned int hop = frames.channels();
    unsigned int index = channel;
    for ( unsigned int i=0; i<frames.frames(); i++ ) {
      frames[index] = tick();
      index += hop;
    }
  }
  else {
    unsigned int iStart = channel * frames.frames();
    for ( unsigned int i=0; i<frames.frames(); i++ )
      frames[iStart++] = tick();
  }

  return frames;
}

StkFrames& WvIn :: tickFrame( StkFrames& frames )
{
  unsigned int nChannels = lastOutputs_.channels();
  if ( nChannels == 0 ) {
    errorString_ << "WvIn::tickFrame(): no data has been loaded!";
    handleError( StkError::WARNING );
    return frames;
  }

  if ( nChannels != frames.channels() ) {
    errorString_ << "WvIn::tickFrame(): incompatible channel value in StkFrames argument!";
    handleError( StkError::FUNCTION_ARGUMENT );
  }

  unsigned int j;
  if ( nChannels == 1 || frames.interleaved() ) {
    unsigned int counter = 0;
    for ( unsigned int i=0; i<frames.frames(); i++ ) {
      this->computeFrame();
      for ( j=0; j<nChannels; j++ )
        frames[counter++] = lastOutputs_[j];
    }
  }
  else { // non-interleaved data
    unsigned int hop = frames.frames();
    unsigned int index;
    for ( unsigned int i=0; i<frames.frames(); i++ ) {
      this->computeFrame();
      index = i;
      for ( j=0; j<nChannels; j++ ) {
        frames[index] = lastOutputs_[j];
        index += hop;
      }
    }
  }

  return frames;
}
