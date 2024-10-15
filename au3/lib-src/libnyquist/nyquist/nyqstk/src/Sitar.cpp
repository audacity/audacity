/***************************************************/
/*! \class Sitar
    \brief STK sitar string model class.

    This class implements a sitar plucked string
    physical model based on the Karplus-Strong
    algorithm.

    This is a digital waveguide model, making its
    use possibly subject to patents held by
    Stanford University, Yamaha, and others.
    There exist at least two patents, assigned to
    Stanford, bearing the names of Karplus and/or
    Strong.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Sitar.h"
#include <math.h>

using namespace Nyq;

Sitar :: Sitar(StkFloat lowestFrequency)
{
  unsigned long length = (unsigned long) (Stk::sampleRate() / lowestFrequency + 1);
  delayLine_.setMaximumDelay( length );
  delay_ = 0.5 * length;
  delayLine_.setDelay( delay_ );
  targetDelay_ = delay_;

  loopFilter_.setZero(0.01);
  loopGain_ = 0.999;

  envelope_.setAllTimes(0.001, 0.04, 0.0, 0.5);
  this->clear();
}

Sitar :: ~Sitar()
{
}

void Sitar :: clear()
{
  delayLine_.clear();
  loopFilter_.clear();
}

void Sitar :: setFrequency(StkFloat frequency)
{
  StkFloat freakency = frequency;
  if ( frequency <= 0.0 ) {
    errorString_ << "Sitar::setFrequency: parameter is less than or equal to zero!";
    handleError( StkError::WARNING );
    freakency = 220.0;
  }

  targetDelay_ = (Stk::sampleRate() / freakency);
  delay_ = targetDelay_ * (1.0 + (0.05 * noise_.tick()));
  delayLine_.setDelay( delay_ );
  loopGain_ = 0.995 + (freakency * 0.0000005);
  if ( loopGain_ > 0.9995 ) loopGain_ = 0.9995;
}

void Sitar :: pluck(StkFloat amplitude)
{
  envelope_.keyOn();
}

void Sitar :: noteOn(StkFloat frequency, StkFloat amplitude)
{
  this->setFrequency( frequency );
  this->pluck( amplitude );
  amGain_ = 0.1 * amplitude;

#if defined(_STK_DEBUG_)
  errorString_ << "Sitar::NoteOn: frequency = " << frequency << ", amplitude = " << amplitude << ".";
  handleError( StkError::DEBUG_WARNING );
#endif
}

void Sitar :: noteOff(StkFloat amplitude)
{
  loopGain_ = (StkFloat) 1.0 - amplitude;
  if ( loopGain_ < 0.0 ) {
    errorString_ << "Sitar::noteOff: amplitude is greater than 1.0 ... setting to 1.0!";
    handleError( StkError::WARNING );
    loopGain_ = 0.0;
  }
  else if ( loopGain_ > 1.0 ) {
    errorString_ << "Sitar::noteOff: amplitude is < 0.0  ... setting to 0.0!";
    handleError( StkError::WARNING );
    loopGain_ = 0.99999;
  }

#if defined(_STK_DEBUG_)
  errorString_ << "Sitar::NoteOff: amplitude = " << amplitude << ".";
  handleError( StkError::DEBUG_WARNING );
#endif
}

StkFloat Sitar :: computeSample()
{
  if ( fabs(targetDelay_ - delay_) > 0.001 ) {
    if ( targetDelay_ < delay_ )
      delay_ *= 0.99999;
    else
      delay_ *= 1.00001;
    delayLine_.setDelay( delay_ );
  }

  lastOutput_ = delayLine_.tick( loopFilter_.tick( delayLine_.lastOut() * loopGain_ ) + 
                                (amGain_ * envelope_.tick() * noise_.tick()));
  
  return lastOutput_;
}
