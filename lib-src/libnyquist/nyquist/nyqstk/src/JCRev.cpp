/***************************************************/
/*! \class JCRev
    \brief John Chowning's reverberator class.

    This class is derived from the CLM JCRev
    function, which is based on the use of
    networks of simple allpass and comb delay
    filters.  This class implements three series
    allpass units, followed by four parallel comb
    filters, and two decorrelation delay lines in
    parallel at the output.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "JCRev.h"
#include <math.h>

using namespace Nyq;

JCRev :: JCRev(StkFloat T60)
{
  // Delay lengths for 44100 Hz sample rate.
  int lengths[9] = {1777, 1847, 1993, 2137, 389, 127, 43, 211, 179};
  double scaler = Stk::sampleRate() / 44100.0;

  int delay, i;
  if ( scaler != 1.0 ) {
    for (i=0; i<9; i++) {
      delay = (int) floor(scaler * lengths[i]);
      if ( (delay & 1) == 0) delay++;
      while ( !this->isPrime(delay) ) delay += 2;
      lengths[i] = delay;
    }
  }

  for (i=0; i<3; i++) {
	  allpassDelays_[i].setMaximumDelay( lengths[i+4] );
	  allpassDelays_[i].setDelay( lengths[i+4] );
  }

  for ( i=0; i<4; i++ ) {
    combDelays_[i].setMaximumDelay( lengths[i] );
    combDelays_[i].setDelay( lengths[i] );
  }

  this->setT60( T60 );
  outLeftDelay_.setMaximumDelay( lengths[7] );
  outLeftDelay_.setDelay( lengths[7] );
  outRightDelay_.setMaximumDelay( lengths[8] );
  outRightDelay_.setDelay( lengths[8] );
  allpassCoefficient_ = 0.7;
  effectMix_ = 0.3;
  this->clear();
}

JCRev :: ~JCRev()
{
}

void JCRev :: clear()
{
  allpassDelays_[0].clear();
  allpassDelays_[1].clear();
  allpassDelays_[2].clear();
  combDelays_[0].clear();
  combDelays_[1].clear();
  combDelays_[2].clear();
  combDelays_[3].clear();
  outRightDelay_.clear();
  outLeftDelay_.clear();
  lastOutput_[0] = 0.0;
  lastOutput_[1] = 0.0;
}

void JCRev :: setT60( StkFloat T60 )
{
  for ( int i=0; i<4; i++ )
    combCoefficient_[i] = pow(10.0, (-3.0 * combDelays_[i].getDelay() / (T60 * Stk::sampleRate())));
}

StkFloat JCRev :: computeSample(StkFloat input)
{
  StkFloat temp, temp0, temp1, temp2, temp3, temp4, temp5, temp6;
  StkFloat filtout;

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
    
  temp = allpassDelays_[2].lastOut();
  temp2 = allpassCoefficient_ * temp;
  temp2 += temp1;
  allpassDelays_[2].tick(temp2);
  temp2 = -(allpassCoefficient_ * temp2) + temp;
    
  temp3 = temp2 + (combCoefficient_[0] * combDelays_[0].lastOut());
  temp4 = temp2 + (combCoefficient_[1] * combDelays_[1].lastOut());
  temp5 = temp2 + (combCoefficient_[2] * combDelays_[2].lastOut());
  temp6 = temp2 + (combCoefficient_[3] * combDelays_[3].lastOut());

  combDelays_[0].tick(temp3);
  combDelays_[1].tick(temp4);
  combDelays_[2].tick(temp5);
  combDelays_[3].tick(temp6);

  filtout = temp3 + temp4 + temp5 + temp6;

  lastOutput_[0] = effectMix_ * (outLeftDelay_.tick(filtout));
  lastOutput_[1] = effectMix_ * (outRightDelay_.tick(filtout));
  temp = (1.0 - effectMix_) * input;
  lastOutput_[0] += temp;
  lastOutput_[1] += temp;
    
  return Effect::lastOut();
}
