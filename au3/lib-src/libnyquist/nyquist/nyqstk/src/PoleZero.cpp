/***************************************************/
/*! \class PoleZero
    \brief STK one-pole, one-zero filter class.

    This protected Filter subclass implements
    a one-pole, one-zero digital filter.  A
    method is provided for creating an allpass
    filter with a given coefficient.  Another
    method is provided to create a DC blocking filter.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "PoleZero.h"

using namespace Nyq;

PoleZero :: PoleZero() : Filter()
{
  // Default setting for pass-through.
  std::vector<StkFloat> b(2, 0.0);
  std::vector<StkFloat> a(2, 0.0);
  b[0] = 1.0;
  a[0] = 1.0;
  Filter::setCoefficients( b, a );
}

PoleZero :: ~PoleZero()
{
}

void PoleZero :: clear(void)
{
  Filter::clear();
}

void PoleZero :: setB0(StkFloat b0)
{
  b_[0] = b0;
}

void PoleZero :: setB1(StkFloat b1)
{
  b_[1] = b1;
}

void PoleZero :: setA1(StkFloat a1)
{
  a_[1] = a1;
}

void PoleZero :: setAllpass(StkFloat coefficient)
{
  b_[0] = coefficient;
  b_[1] = 1.0;
  a_[0] = 1.0; // just in case
  a_[1] = coefficient;
}

void PoleZero :: setBlockZero(StkFloat thePole)
{
  b_[0] = 1.0;
  b_[1] = -1.0;
  a_[0] = 1.0; // just in case
  a_[1] = -thePole;
}

void PoleZero :: setGain(StkFloat gain)
{
  Filter::setGain(gain);
}

StkFloat PoleZero :: getGain(void) const
{
  return Filter::getGain();
}

StkFloat PoleZero :: lastOut(void) const
{
  return Filter::lastOut();
}

StkFloat PoleZero :: tick( StkFloat input )
{
  inputs_[0] = gain_ * input;
  outputs_[0] = b_[0] * inputs_[0] + b_[1] * inputs_[1] - a_[1] * outputs_[1];
  inputs_[1] = inputs_[0];
  outputs_[1] = outputs_[0];

  return outputs_[0];
}

StkFrames& PoleZero :: tick( StkFrames& frames, unsigned int channel )
{
  return Filter::tick( frames, channel );
}
