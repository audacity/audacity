/***************************************************/
/*! \class BiQuad
    \brief STK biquad (two-pole, two-zero) filter class.

    This protected Filter subclass implements a
    two-pole, two-zero digital filter.  A method
    is provided for creating a resonance in the
    frequency response while maintaining a constant
    filter gain.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "BiQuad.h"
#include <cmath>

using namespace Nyq;

BiQuad :: BiQuad() : Filter()
{
  std::vector<StkFloat> b(3, 0.0);
  std::vector<StkFloat> a(3, 0.0);
  b[0] = 1.0;
  a[0] = 1.0;
  Filter::setCoefficients( b, a );
}

BiQuad :: ~BiQuad()
{
}

void BiQuad :: clear(void)
{
  Filter::clear();
}

void BiQuad :: setB0(StkFloat b0)
{
  b_[0] = b0;
}

void BiQuad :: setB1(StkFloat b1)
{
  b_[1] = b1;
}

void BiQuad :: setB2(StkFloat b2)
{
  b_[2] = b2;
}

void BiQuad :: setA1(StkFloat a1)
{
  a_[1] = a1;
}

void BiQuad :: setA2(StkFloat a2)
{
  a_[2] = a2;
}

void BiQuad :: setResonance(StkFloat frequency, StkFloat radius, bool normalize)
{
  a_[2] = radius * radius;
  a_[1] = -2.0 * radius * cos(TWO_PI * frequency / Stk::sampleRate());

  if ( normalize ) {
    // Use zeros at +- 1 and normalize the filter peak gain.
    b_[0] = 0.5 - 0.5 * a_[2];
    b_[1] = 0.0;
    b_[2] = -b_[0];
  }
}

void BiQuad :: setNotch(StkFloat frequency, StkFloat radius)
{
  // This method does not attempt to normalize the filter gain.
  b_[2] = radius * radius;
  b_[1] = (StkFloat) -2.0 * radius * cos(TWO_PI * (double) frequency / Stk::sampleRate());
}

void BiQuad :: setEqualGainZeroes()
{
  b_[0] = 1.0;
  b_[1] = 0.0;
  b_[2] = -1.0;
}

void BiQuad :: setGain(StkFloat gain)
{
  Filter::setGain(gain);
}

StkFloat BiQuad :: getGain(void) const
{
  return Filter::getGain();
}

StkFloat BiQuad :: lastOut(void) const
{
  return Filter::lastOut();
}

StkFloat BiQuad :: computeSample( StkFloat input )
{
  inputs_[0] = gain_ * input;
  outputs_[0] = b_[0] * inputs_[0] + b_[1] * inputs_[1] + b_[2] * inputs_[2];
  outputs_[0] -= a_[2] * outputs_[2] + a_[1] * outputs_[1];
  inputs_[2] = inputs_[1];
  inputs_[1] = inputs_[0];
  outputs_[2] = outputs_[1];
  outputs_[1] = outputs_[0];

  return outputs_[0];
}

StkFloat BiQuad :: tick( StkFloat input )
{
  return this->computeSample( input );
}

StkFrames& BiQuad :: tick( StkFrames& frames, unsigned int channel )
{
  return Filter::tick( frames, channel );
}
