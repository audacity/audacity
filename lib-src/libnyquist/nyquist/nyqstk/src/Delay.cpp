/***************************************************/
/*! \class Delay
    \brief STK non-interpolating delay line class.

    This protected Filter subclass implements
    a non-interpolating digital delay-line.
    A fixed maximum length of 4095 and a delay
    of zero is set using the default constructor.
    Alternatively, the delay and maximum length
    can be set during instantiation with an
    overloaded constructor.
    
    A non-interpolating delay line is typically
    used in fixed delay-length applications, such
    as for reverberation.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Delay.h"

using namespace Nyq;

Delay :: Delay() : Filter()
{
  // Default maximum delay length set to 4095.
  inputs_.resize( 4096 );
  this->clear();

  inPoint_ = 0;
  outPoint_ = 0;
  delay_ = 0;
}

Delay :: Delay(unsigned long delay, unsigned long maxDelay)
{
  // Writing before reading allows delays from 0 to length-1. 
  // If we want to allow a delay of maxDelay, we need a
  // delay-line of length = maxDelay+1.
  if ( maxDelay < 1 ) {
    errorString_ << "Delay::Delay: maxDelay must be > 0!\n";
    handleError( StkError::FUNCTION_ARGUMENT );
  }

  if ( delay > maxDelay ) {
    errorString_ << "Delay::Delay: maxDelay must be > than delay argument!\n";
    handleError( StkError::FUNCTION_ARGUMENT );
  }

  if ( maxDelay > inputs_.size()-1 ) {
    inputs_.resize( maxDelay+1 );
    this->clear();
  }

  inPoint_ = 0;
  this->setDelay( delay );
}

Delay :: ~Delay()
{
}

void Delay :: clear(void)
{
  for (unsigned int i=0; i<inputs_.size(); i++)
    inputs_[i] = 0.0;
  outputs_[0] = 0.0;
}

void Delay :: setMaximumDelay(unsigned long delay)
{
  if ( delay < inputs_.size() ) return;

/*
  if ( delay < 0 ) {
    errorString_ << "Delay::setMaximumDelay: argument (" << delay << ") less than zero!\n";
    handleError( StkError::WARNING );
    return;
  }
  else
*/
  if (delay < delay_ ) {
    errorString_ << "Delay::setMaximumDelay: argument (" << delay << ") less than current delay setting (" << delay_ << ")!\n";
    handleError( StkError::WARNING );
    return;
  }

  inputs_.resize( delay + 1 );
}

void Delay :: setDelay(unsigned long delay)
{
  if ( delay > inputs_.size() - 1 ) { // The value is too big.
    errorString_ << "Delay::setDelay: argument (" << delay << ") too big ... setting to maximum!\n";
    handleError( StkError::WARNING );

    // Force delay to maximum length.
    outPoint_ = inPoint_ + 1;
    if ( outPoint_ == inputs_.size() ) outPoint_ = 0;
    delay_ = inputs_.size() - 1;
  }
  /*
  else if ( delay < 0 ) {
    errorString_ << "Delay::setDelay: argument (" << delay << ") less than zero ... setting to zero!\n";
    handleError( StkError::WARNING );

    outPoint_ = inPoint_;
    delay_ = 0;
  }
  */
  else { // read chases write
    if ( inPoint_ >= delay ) outPoint_ = inPoint_ - delay;
    else outPoint_ = inputs_.size() + inPoint_ - delay;
    delay_ = delay;
  }
}

unsigned long Delay :: getDelay(void) const
{
  return (unsigned long) delay_;
}

StkFloat Delay :: energy(void) const
{
  unsigned long i;
  StkFloat e = 0;
  if (inPoint_ >= outPoint_) {
    for (i=outPoint_; i<inPoint_; i++) {
      StkFloat t = inputs_[i];
      e += t*t;
    }
  } else {
    for (i=outPoint_; i<inputs_.size(); i++) {
      StkFloat t = inputs_[i];
      e += t*t;
    }
    for (i=0; i<inPoint_; i++) {
      StkFloat t = inputs_[i];
      e += t*t;
    }
  }
  return e;
}

StkFloat Delay :: contentsAt(unsigned long tapDelay)
{
  unsigned long i = tapDelay;
  if (i < 1) {
    errorString_ << "Delay::contentsAt: argument (" << tapDelay << ") too small!";
    handleError( StkError::WARNING );
    return 0.0;
  }
  else if (i > delay_) {
    errorString_ << "Delay::contentsAt: argument (" << tapDelay << ") too big!";
    handleError( StkError::WARNING );
    return 0.0;
  }

  intptr_t tap = inPoint_ - i;
  if (tap < 0) // Check for wraparound.
    tap += inputs_.size();

  return inputs_[tap];
}

StkFloat Delay :: lastOut(void) const
{
  return Filter::lastOut();
}

StkFloat Delay :: nextOut(void)
{
  return inputs_[outPoint_];
}

StkFloat Delay :: computeSample( StkFloat input )
{
  inputs_[inPoint_++] = input;

  // Check for end condition
  if (inPoint_ == inputs_.size())
    inPoint_ = 0;

  // Read out next value
  outputs_[0] = inputs_[outPoint_++];

  if (outPoint_ == inputs_.size())
    outPoint_ = 0;

  return outputs_[0];
}

StkFloat Delay :: tick( StkFloat input )
{
  return computeSample( input );
}

StkFrames& Delay :: tick( StkFrames& frames, unsigned int channel )
{
  return Filter::tick( frames, channel );
}
