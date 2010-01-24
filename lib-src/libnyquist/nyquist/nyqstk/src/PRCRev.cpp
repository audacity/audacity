/***************************************************/
/*! \class PRCRev
    \brief Perry's simple reverberator class.

    This class is based on some of the famous
    Stanford/CCRMA reverbs (NRev, KipRev), which
    were based on the Chowning/Moorer/Schroeder
    reverberators using networks of simple allpass
    and comb delay filters.  This class implements
    two series allpass units and two parallel comb
    filters.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "PRCRev.h"
#include <math.h>

using namespace Nyq;

PRCRev :: PRCRev(StkFloat T60)
{
  // Delay lengths for 44100 Hz sample rate.
  int lengths[4]= {353, 1097, 1777, 2137};
  double scaler = Stk::sampleRate() / 44100.0;

  // Scale the delay lengths if necessary.
  int delay, i;
  if ( scaler != 1.0 ) {
    for (i=0; i<4; i++)	{
      delay = (int) floor(scaler * lengths[i]);
      if ( (delay & 1) == 0) delay++;
      while ( !this->isPrime(delay) ) delay += 2;
      lengths[i] = delay;
    }
  }

  for (i=0; i<2; i++)	{
	  allpassDelays_[i].setMaximumDelay( lengths[i] );
	  allpassDelays_[i].setDelay( lengths[i] );

    combDelays_[i].setMaximumDelay( lengths[i+2] );
    combDelays_[i].setDelay( lengths[i+2] );
  }

  this->setT60( T60 );
  allpassCoefficient_ = 0.7;
  effectMix_ = 0.5;
  this->clear();
}

PRCRev :: ~PRCRev()
{
}

void PRCRev :: clear()
{
  allpassDelays_[0].clear();
  allpassDelays_[1].clear();
  combDelays_[0].clear();
  combDelays_[1].clear();
  lastOutput_[0] = 0.0;
  lastOutput_[1] = 0.0;
}

void PRCRev :: setT60( StkFloat T60 )
{
  combCoefficient_[0] = pow(10.0, (-3.0 * combDelays_[0].getDelay() / (T60 * Stk::sampleRate())));
  combCoefficient_[1] = pow(10.0, (-3.0 * combDelays_[1].getDelay() / (T60 * Stk::sampleRate())));
}

StkFloat PRCRev :: computeSample(StkFloat input)
{
  StkFloat temp, temp0, temp1, temp2, temp3;

  temp = allpassDelays_[0].lastOut();
  temp0 = allpassCoefficient_ * temp;
  temp0 += input;
  allpassDelays_[0].tick(temp0);
  temp0 = -(allpassCoefficient_ * temp0) + temp;
    
  temp = allpassDelays_[1].lastOut();
  temp1 = allpassCoefficient_ * temp;
  temp1 += temp0;
  allpassDelays_[1].tick(temp1);
  temp1 = -(allpassCoefficient_ * temp1) + temp;
    
  temp2 = temp1 + (combCoefficient_[0] * combDelays_[0].lastOut());
  temp3 = temp1 + (combCoefficient_[1] * combDelays_[1].lastOut());

  lastOutput_[0] = effectMix_ * (combDelays_[0].tick(temp2));
  lastOutput_[1] = effectMix_ * (combDelays_[1].tick(temp3));
  temp = (1.0 - effectMix_) * input;
  lastOutput_[0] += temp;
  lastOutput_[1] += temp;
    
  return Effect::lastOut();
}

