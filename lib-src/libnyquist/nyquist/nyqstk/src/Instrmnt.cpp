/***************************************************/
/*! \class Instrmnt
    \brief STK instrument abstract base class.

    This class provides a common interface for
    all STK instruments.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Instrmnt.h"

using namespace Nyq;

Instrmnt :: Instrmnt()
{
}

Instrmnt :: ~Instrmnt()
{
}

void Instrmnt :: setFrequency(StkFloat frequency)
{
  errorString_ << "Instrmnt::setFrequency: virtual setFrequency function call!";
  handleError( StkError::WARNING );
}

StkFloat Instrmnt :: lastOut() const
{
  return lastOutput_;
}

// Support for stereo output:
StkFloat Instrmnt :: lastOutLeft(void) const
{
  return 0.5 * lastOutput_;
}
                                                                                
StkFloat Instrmnt :: lastOutRight(void) const
{
  return 0.5 * lastOutput_;
}

StkFloat Instrmnt :: tick( void )
{
  return computeSample();
}

StkFrames& Instrmnt :: tick( StkFrames& frames, unsigned int channel )
{
  if ( channel >= frames.channels() ) {
    errorString_ << "Instrmnt::tick(): channel and StkFrames arguments are incompatible!";
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
    for ( unsigned int i=0; i<frames.frames(); i++, iStart++ )
      frames[iStart] = tick();
  }

  return frames;
}

void Instrmnt :: controlChange(int number, StkFloat value)
{
  errorString_ << "Instrmnt::controlChange: virtual function call!";
  handleError( StkError::WARNING );
}
