/***************************************************/
/*! \class PitShift
    \brief STK simple pitch shifter effect class.

    This class implements a simple pitch shifter
    using delay lines.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "PitShift.h"
#include <cmath>

using namespace Nyq;

const int maxDelay = 5024;

PitShift :: PitShift()
{
  delayLength = maxDelay - 24;
  halfLength = delayLength / 2;
  delay_[0] = 12;
  delay_[1] = maxDelay / 2;

  delayLine_[0].setMaximumDelay( maxDelay );
  delayLine_[0].setDelay( delay_[0] );
  delayLine_[1].setMaximumDelay( maxDelay );
  delayLine_[1].setDelay( delay_[1] );
  effectMix_ = 0.5;
  rate_ = 1.0;
}

PitShift :: ~PitShift()
{
}

void PitShift :: clear()
{
  delayLine_[0].clear();
  delayLine_[1].clear();
  lastOutput_[0] = 0.0;
  lastOutput_[1] = 0.0;
}

void PitShift :: setShift(StkFloat shift)
{
  if (shift < 1.0) {
    rate_ = 1.0 - shift; 
  }
  else if (shift > 1.0) {
    rate_ = 1.0 - shift;
  }
  else {
    rate_ = 0.0;
    delay_[0] = halfLength+12;
  }
}

StkFloat PitShift :: computeSample(StkFloat input)
{
  // Calculate the two delay length values, keeping them within the
  // range 12 to maxDelay-12.
  delay_[0] += rate_;
  while (delay_[0] > maxDelay-12) delay_[0] -= delayLength;
  while (delay_[0] < 12) delay_[0] += delayLength;

  delay_[1] = delay_[0] + halfLength;
  while (delay_[1] > maxDelay-12) delay_[1] -= delayLength;
  while (delay_[1] < 12) delay_[1] += delayLength;

  // Set the new delay line lengths.
  delayLine_[0].setDelay((long)delay_[0]);
  delayLine_[1].setDelay((long)delay_[1]);

  // Calculate a triangular envelope.
  env_[1] = fabs( (delay_[0] - halfLength + 12) * (1.0 / (halfLength+12) ) );
  env_[0] = 1.0 - env_[1];

  // Delay input and apply envelope.
  lastOutput_[0] =  env_[0] * delayLine_[0].tick(input);
  lastOutput_[0] += env_[1] * delayLine_[1].tick(input);

  // Compute effect mix and output.
  lastOutput_[0] *= effectMix_;
  lastOutput_[0] += (1.0 - effectMix_) * input;
  lastOutput_[1] = lastOutput_[0];

  return lastOutput_[0];
}
