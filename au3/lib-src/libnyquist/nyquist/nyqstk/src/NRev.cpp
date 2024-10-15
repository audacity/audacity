/***************************************************/
/*! \class NRev
    \brief CCRMA's NRev reverberator class.

    This class is derived from the CLM NRev
    function, which is based on the use of
    networks of simple allpass and comb delay
    filters.  This particular arrangement consists
    of 6 comb filters in parallel, followed by 3
    allpass filters, a lowpass filter, and another
    allpass in series, followed by two allpass
    filters in parallel with corresponding right
    and left outputs.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "NRev.h"
#include <math.h>

using namespace Nyq;

NRev :: NRev(StkFloat T60)
{
  int lengths[15] = {1433, 1601, 1867, 2053, 2251, 2399, 347, 113, 37, 59, 53, 43, 37, 29, 19};
  double scaler = Stk::sampleRate() / 25641.0;

  int delay, i;
  for (i=0; i<15; i++) {
    delay = (int) floor(scaler * lengths[i]);
    if ( (delay & 1) == 0) delay++;
    while ( !this->isPrime(delay) ) delay += 2;
    lengths[i] = delay;
  }

  for (i=0; i<6; i++) {
    combDelays_[i].setMaximumDelay( lengths[i] );
    combDelays_[i].setDelay( lengths[i] );
    combCoefficient_[i] = pow(10.0, (-3 * lengths[i] / (T60 * Stk::sampleRate())));
  }

  for (i=0; i<8; i++) {
	  allpassDelays_[i].setMaximumDelay( lengths[i+6] );
	  allpassDelays_[i].setDelay( lengths[i+6] );
  }

  this->setT60( T60 );
  allpassCoefficient_ = 0.7;
  effectMix_ = 0.3;
  this->clear();
}

NRev :: ~NRev()
{
}

void NRev :: clear()
{
  int i;
  for (i=0; i<6; i++) combDelays_[i].clear();
  for (i=0; i<8; i++) allpassDelays_[i].clear();
  lastOutput_[0] = 0.0;
  lastOutput_[1] = 0.0;
  lowpassState_ = 0.0;
}

void NRev :: setT60( StkFloat T60 )
{
  for ( int i=0; i<6; i++ )
    combCoefficient_[i] = pow(10.0, (-3.0 * combDelays_[i].getDelay() / (T60 * Stk::sampleRate())));
}

StkFloat NRev :: computeSample(StkFloat input)
{
  StkFloat temp, temp0, temp1, temp2, temp3;
  int i;

  temp0 = 0.0;
  for (i=0; i<6; i++) {
    temp = input + (combCoefficient_[i] * combDelays_[i].lastOut());
    temp0 += combDelays_[i].tick(temp);
  }
  for (i=0; i<3; i++)	{
    temp = allpassDelays_[i].lastOut();
    temp1 = allpassCoefficient_ * temp;
    temp1 += temp0;
    allpassDelays_[i].tick(temp1);
    temp0 = -(allpassCoefficient_ * temp1) + temp;
  }

	// One-pole lowpass filter.
  lowpassState_ = 0.7*lowpassState_ + 0.3*temp0;
  temp = allpassDelays_[3].lastOut();
  temp1 = allpassCoefficient_ * temp;
  temp1 += lowpassState_;
  allpassDelays_[3].tick(temp1);
  temp1 = -(allpassCoefficient_ * temp1) + temp;
    
  temp = allpassDelays_[4].lastOut();
  temp2 = allpassCoefficient_ * temp;
  temp2 += temp1;
  allpassDelays_[4].tick(temp2);
  lastOutput_[0] = effectMix_*(-(allpassCoefficient_ * temp2) + temp);
    
  temp = allpassDelays_[5].lastOut();
  temp3 = allpassCoefficient_ * temp;
  temp3 += temp1;
  allpassDelays_[5].tick(temp3);
  lastOutput_[1] = effectMix_*(-(allpassCoefficient_ * temp3) + temp);

  temp = (1.0 - effectMix_) * input;
  lastOutput_[0] += temp;
  lastOutput_[1] += temp;
    
  return Effect::lastOut();
}
