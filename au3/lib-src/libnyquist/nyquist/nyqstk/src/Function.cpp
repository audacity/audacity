/***************************************************/
/*! \class Function
    \brief STK abstract function parent class.

    This class provides common functionality for STK classes which
    implement tables or other types of input to output function
    mappings.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Function.h"

using namespace Nyq;

Function :: Function() : Stk()
{
  lastOutput_ = (StkFloat) 0.0;
}

Function :: ~Function()
{
}

StkFloat Function :: tick( StkFloat input )
{
  return computeSample( input );
}

StkFrames& Function :: tick( StkFrames& frames, unsigned int channel )
{
  if ( channel >= frames.channels() ) {
    errorString_ << "Function::tick(): channel and StkFrames arguments are incompatible!";
    handleError( StkError::FUNCTION_ARGUMENT );
  }

  if ( frames.channels() == 1 ) {
    for ( unsigned int i=0; i<frames.frames(); i++ )
      frames[i] = computeSample( frames[i] );
  }
  else if ( frames.interleaved() ) {
    unsigned int hop = frames.channels();
    unsigned int index = channel;
    for ( unsigned int i=0; i<frames.frames(); i++ ) {
      frames[index] = computeSample( frames[index] );
      index += hop;
    }
  }
  else {
    unsigned int iStart = channel * frames.frames();
    for ( unsigned int i=0; i<frames.frames(); i++, iStart++ )
      frames[iStart] = computeSample( frames[iStart] );
  }

  return frames;
}
