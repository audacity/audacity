/***************************************************/
/*! \class DelayL
    \brief STK linear interpolating delay line class.

    This Delay subclass implements a fractional-
    length digital delay-line using first-order
    linear interpolation.  A fixed maximum length
    of 4095 and a delay of zero is set using the
    default constructor.  Alternatively, the
    delay and maximum length can be set during
    instantiation with an overloaded constructor.

    Linear interpolation is an efficient technique
    for achieving fractional delay lengths, though
    it does introduce high-frequency signal
    attenuation to varying degrees depending on the
    fractional delay setting.  The use of higher
    order Lagrange interpolators can typically
    improve (minimize) this attenuation characteristic.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "DelayL.h"

using namespace Nyq;

DelayL :: DelayL() : Delay()
{
  doNextOut_ = true;
}

DelayL :: DelayL(StkFloat delay, unsigned long maxDelay)
{
  if ( delay < 0.0 || maxDelay < 1 ) {
    errorString_ << "DelayL::DelayL: delay must be >= 0.0, maxDelay must be > 0!";
    handleError( StkError::FUNCTION_ARGUMENT );
  }

  if ( delay > (StkFloat) maxDelay ) {
    errorString_ << "DelayL::DelayL: maxDelay must be > than delay argument!";
    handleError( StkError::FUNCTION_ARGUMENT );
  }

  // Writing before reading allows delays from 0 to length-1. 
  if ( maxDelay > inputs_.size()-1 ) {
    inputs_.resize( maxDelay+1 );
    this->clear();
  }

  inPoint_ = 0;
  this->setDelay(delay);
  doNextOut_ = true;
}

DelayL :: ~DelayL()
{
}

void DelayL :: setDelay(StkFloat delay)
{
  StkFloat outPointer;

  if ( delay > inputs_.size() - 1 ) { // The value is too big.
    errorString_ << "DelayL::setDelay: argument (" << delay << ") too big ... setting to maximum!";
    handleError( StkError::WARNING );

    // Force delay to maxLength
    outPointer = inPoint_ + 1.0;
    delay_ = inputs_.size() - 1;
  }
  else if (delay < 0 ) {
    errorString_ << "DelayL::setDelay: argument (" << delay << ") less than zero ... setting to zero!";
    handleError( StkError::WARNING );

    outPointer = inPoint_;
    delay_ = 0;
  }
  else {
    outPointer = inPoint_ - delay;  // read chases write
    delay_ = delay;
  }

  while (outPointer < 0)
    outPointer += inputs_.size(); // modulo maximum length

  outPoint_ = (long) outPointer;   // integer part
  if ( outPoint_ == inputs_.size() ) outPoint_ = 0;
  alpha_ = outPointer - outPoint_; // fractional part
  omAlpha_ = (StkFloat) 1.0 - alpha_;
}

StkFloat DelayL :: getDelay(void) const
{
  return delay_;
}

StkFloat DelayL :: nextOut(void)
{
  if ( doNextOut_ ) {
    // First 1/2 of interpolation
    nextOutput_ = inputs_[outPoint_] * omAlpha_;
    // Second 1/2 of interpolation
    if (outPoint_+1 < inputs_.size())
      nextOutput_ += inputs_[outPoint_+1] * alpha_;
    else
      nextOutput_ += inputs_[0] * alpha_;
    doNextOut_ = false;
  }

  return nextOutput_;
}

StkFloat DelayL :: computeSample( StkFloat input )
{
  inputs_[inPoint_++] = input;

  // Increment input pointer modulo length.
  if (inPoint_ == inputs_.size())
    inPoint_ = 0;

  outputs_[0] = nextOut();
  doNextOut_ = true;

  // Increment output pointer modulo length.
  if (++outPoint_ == inputs_.size())
    outPoint_ = 0;

  return outputs_[0];
}

