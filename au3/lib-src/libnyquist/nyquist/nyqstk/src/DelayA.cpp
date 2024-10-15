/***************************************************/
/*! \class DelayA
    \brief STK allpass interpolating delay line class.

    This Delay subclass implements a fractional-length digital
    delay-line using a first-order allpass filter.  A fixed maximum
    length of 4095 and a delay of 0.5 is set using the default
    constructor.  Alternatively, the delay and maximum length can be
    set during instantiation with an overloaded constructor.

    An allpass filter has unity magnitude gain but variable phase
    delay properties, making it useful in achieving fractional delays
    without affecting a signal's frequency magnitude response.  In
    order to achieve a maximally flat phase delay response, the
    minimum delay possible in this implementation is limited to a
    value of 0.5.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "DelayA.h"

using namespace Nyq;

DelayA :: DelayA() : Delay()
{
  this->setDelay( 0.5 );
  apInput_ = 0.0;
  doNextOut_ = true;
}

DelayA :: DelayA(StkFloat delay, unsigned long maxDelay)
{
  if ( delay < 0.0 || maxDelay < 1 ) {
    errorString_ << "DelayA::DelayA: delay must be >= 0.0, maxDelay must be > 0!";
    handleError( StkError::FUNCTION_ARGUMENT );
  }

  if ( delay > (StkFloat) maxDelay ) {
    errorString_ << "DelayA::DelayA: maxDelay must be > than delay argument!";
    handleError( StkError::FUNCTION_ARGUMENT );
  }

  // Writing before reading allows delays from 0 to length-1. 
  if ( maxDelay > inputs_.size()-1 ) {
    inputs_.resize( maxDelay+1 );
    this->clear();
  }

  inPoint_ = 0;
  this->setDelay(delay);
  apInput_ = 0.0;
  doNextOut_ = true;
}

DelayA :: ~DelayA()
{
}

void DelayA :: clear()
{
  Delay::clear();
  apInput_ = 0.0;
}

void DelayA :: setDelay(StkFloat delay)  
{
  StkFloat outPointer;
  unsigned long length = inputs_.size();

  if ( delay > inputs_.size() - 1 ) { // The value is too big.
    errorString_ << "DelayA::setDelay: argument (" << delay << ") too big ... setting to maximum!";
    handleError( StkError::WARNING );

    // Force delay to maxLength
    outPointer = inPoint_ + 1.0;
    delay_ = length - 1;
  }
  else if (delay < 0.5) {
    errorString_ << "DelayA::setDelay: argument (" << delay << ") less than 0.5 not possible!";
    handleError( StkError::WARNING );

    outPointer = inPoint_ + 0.4999999999;
    delay_ = 0.5;
  }
  else {
    outPointer = inPoint_ - delay + 1.0;     // outPoint chases inpoint
    delay_ = delay;
  }

  if (outPointer < 0)
    outPointer += length;  // modulo maximum length

  outPoint_ = (long) outPointer;         // integer part
  if ( outPoint_ == length ) outPoint_ = 0;
  alpha_ = 1.0 + outPoint_ - outPointer; // fractional part

  if (alpha_ < 0.5) {
    // The optimal range for alpha is about 0.5 - 1.5 in order to
    // achieve the flattest phase delay response.
    outPoint_ += 1;
    if (outPoint_ >= length) outPoint_ -= length;
    alpha_ += (StkFloat) 1.0;
  }

  coeff_ = ((StkFloat) 1.0 - alpha_) / 
    ((StkFloat) 1.0 + alpha_);         // coefficient for all pass
}

StkFloat DelayA :: getDelay(void) const
{
  return delay_;
}

StkFloat DelayA :: nextOut(void)
{
  if ( doNextOut_ ) {
    // Do allpass interpolation delay.
    nextOutput_ = -coeff_ * outputs_[0];
    nextOutput_ += apInput_ + (coeff_ * inputs_[outPoint_]);
    doNextOut_ = false;
  }

  return nextOutput_;
}

StkFloat DelayA :: computeSample( StkFloat input )
{
  inputs_[inPoint_++] = input;

  // Increment input pointer modulo length.
  if (inPoint_ == inputs_.size())
    inPoint_ = 0;

  outputs_[0] = nextOut();
  doNextOut_ = true;

  // Save the allpass input and increment modulo length.
  apInput_ = inputs_[outPoint_++];
  if (outPoint_ == inputs_.size())
    outPoint_ = 0;

  return outputs_[0];
}
