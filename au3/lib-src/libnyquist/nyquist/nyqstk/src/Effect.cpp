/***************************************************/
/*! \class Effect
    \brief STK abstract effects parent class.

    This class provides common functionality for
    STK effects subclasses.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Effect.h"
#include <math.h>

using namespace Nyq;

Effect :: Effect()
{
}

Effect :: ~Effect()
{
}

void Effect :: setEffectMix(StkFloat mix)
{
  if ( mix < 0.0 ) {
    errorString_ << "Effect::setEffectMix: mix parameter is less than zero ... setting to zero!";
    handleError( StkError::WARNING );
    effectMix_ = 0.0;
  }
  else if ( mix > 1.0 ) {
    errorString_ << "Effect::setEffectMix: mix parameter is greater than 1.0 ... setting to one!";
    handleError( StkError::WARNING );
    effectMix_ = 1.0;
  }
  else
    effectMix_ = mix;
}

StkFloat Effect :: lastOut() const
{
  return (lastOutput_[0] + lastOutput_[1]) * 0.5;
}

StkFloat Effect :: lastOutLeft() const
{
  return lastOutput_[0];
}

StkFloat Effect :: lastOutRight() const
{
  return lastOutput_[1];
}

StkFloat Effect :: tick( StkFloat input )
{
  return computeSample( input );
}

StkFrames& Effect :: tick( StkFrames& frames, unsigned int channel )
{
  if ( channel >= frames.channels() ) {
    errorString_ << "Effect::tick(): channel and StkFrames arguments are incompatible!";
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

bool Effect :: isPrime(int number)
{
  if (number == 2) return true;
  if (number & 1)	{
	  for (int i=3; i<(int)sqrt((double)number)+1; i+=2)
		  if ( (number % i) == 0) return false;
	  return true; // prime
	}
  else return false; // even
}
