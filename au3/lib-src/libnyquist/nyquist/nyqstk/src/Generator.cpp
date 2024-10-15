/***************************************************/
/*! \class Generator
    \brief STK abstract unit generator parent class.

    This class provides common functionality for
    STK unit generator sample-source subclasses.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Generator.h"

using namespace Nyq;

Generator :: Generator() : Stk()
{
  lastOutput_ = 0.0;
}

Generator :: ~Generator()
{
}

StkFloat Generator :: tick( void )
{
  return computeSample();
}

StkFrames& Generator :: tick( StkFrames& frames, unsigned int channel )
{
  if ( channel >= frames.channels() ) {
    errorString_ << "Generator::tick(): channel and StkFrames arguments are incompatible!";
    handleError( StkError::FUNCTION_ARGUMENT );
  }

  if ( frames.channels() == 1 ) {
    for ( unsigned int i=0; i<frames.frames(); i++ )
      frames[i] = computeSample();
  }
  else if ( frames.interleaved() ) {
    unsigned int hop = frames.channels();
    unsigned int index = channel;
    for ( unsigned int i=0; i<frames.frames(); i++ ) {
      frames[index] = computeSample();
      index += hop;
    }
  }
  else {
    unsigned int iStart = channel * frames.frames();
    for ( unsigned int i=0; i<frames.frames(); i++, iStart++ )
      frames[iStart] = computeSample();
  }

  return frames;
}
