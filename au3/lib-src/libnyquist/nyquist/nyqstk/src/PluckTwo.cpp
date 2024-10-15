/***************************************************/
/*! \class PluckTwo
    \brief STK enhanced plucked string model class.

    This class implements an enhanced two-string,
    plucked physical model, a la Jaffe-Smith,
    Smith, and others.

    PluckTwo is an abstract class, with no excitation
    specified.  Therefore, it can't be directly
    instantiated.

    This is a digital waveguide model, making its
    use possibly subject to patents held by
    Stanford University, Yamaha, and others.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "PluckTwo.h"

using namespace Nyq;

PluckTwo :: PluckTwo(StkFloat lowestFrequency)
{
  length_ = (unsigned long) (Stk::sampleRate() / lowestFrequency + 1);
  lastLength_ = length_ * 0.5;
  delayLine_.setMaximumDelay( length_ );
  delayLine_.setDelay( lastLength_ );
  delayLine2_.setMaximumDelay( length_ );
  delayLine2_.setDelay( lastLength_ );
  combDelay_.setMaximumDelay( length_ );
  combDelay_.setDelay( lastLength_ );

  baseLoopGain_ = 0.995;
  loopGain_ = 0.999;
  pluckAmplitude_ = 0.3;
  pluckPosition_ = 0.4;
  detuning_ = 0.995;
  lastFrequency_ = lowestFrequency * 2.0;

}

PluckTwo :: ~PluckTwo()
{
}

void PluckTwo :: clear()
{
  delayLine_.clear();
  delayLine2_.clear();
  combDelay_.clear();
  filter_.clear();
  filter2_.clear();
}

void PluckTwo :: setFrequency(StkFloat frequency)
{
  lastFrequency_ = frequency;
  if ( lastFrequency_ <= 0.0 ) {
    errorString_ << "Clarinet::setFrequency: parameter is less than or equal to zero!";
    handleError( StkError::WARNING );
    lastFrequency_ = 220.0;
  }

  // Delay = length - approximate filter delay.
  lastLength_ = Stk::sampleRate() / lastFrequency_;
  StkFloat delay = (lastLength_ / detuning_) - 0.5;
  if ( delay <= 0.0 ) delay = 0.3;
  else if ( delay > length_ ) delay = length_;
  delayLine_.setDelay( delay );

  delay = (lastLength_ * detuning_) - 0.5;
  if ( delay <= 0.0 ) delay = 0.3;
  else if ( delay > length_ ) delay = length_;
  delayLine2_.setDelay( delay );

  loopGain_ = baseLoopGain_ + (frequency * 0.000005);
  if ( loopGain_ > 1.0 ) loopGain_ = 0.99999;
}

void PluckTwo :: setDetune(StkFloat detune)
{
  detuning_ = detune;
  if ( detuning_ <= 0.0 ) {
    errorString_ << "Clarinet::setDeturn: parameter is less than or equal to zero!";
    handleError( StkError::WARNING );
    detuning_ = 0.1;
  }
  delayLine_.setDelay(( lastLength_ / detuning_) - 0.5);
  delayLine2_.setDelay( (lastLength_ * detuning_) - 0.5);
}

void PluckTwo :: setFreqAndDetune(StkFloat frequency, StkFloat detune)
{
  detuning_ = detune;
  this->setFrequency( frequency );
}

void PluckTwo :: setPluckPosition(StkFloat position)
{
  pluckPosition_ = position;
  if ( position < 0.0 ) {
    errorString_ << "PluckTwo::setPluckPosition: parameter is less than zero ... setting to 0.0!";
    handleError( StkError::WARNING );
    pluckPosition_ = 0.0;
  }
  else if ( position > 1.0 ) {
    errorString_ << "PluckTwo::setPluckPosition: parameter is greater than one ... setting to 1.0!";
    handleError( StkError::WARNING );
    pluckPosition_ = 1.0;
  }
}

void PluckTwo :: setBaseLoopGain(StkFloat aGain)
{
  baseLoopGain_ = aGain;
  loopGain_ = baseLoopGain_ + (lastFrequency_ * 0.000005);
  if ( loopGain_ > 0.99999 ) loopGain_ = 0.99999;
}

void PluckTwo :: noteOff(StkFloat amplitude)
{
  loopGain_ =  (1.0 - amplitude) * 0.5;

#if defined(_STK_DEBUG_)
  errorString_ << "PluckTwo::NoteOff: amplitude = " << amplitude << ".";
  handleError( StkError::DEBUG_WARNING );
#endif
}

