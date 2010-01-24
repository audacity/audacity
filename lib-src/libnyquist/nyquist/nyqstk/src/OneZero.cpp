/***************************************************/
/*! \class OneZero
    \brief STK one-zero filter class.

    This protected Filter subclass implements
    a one-zero digital filter.  A method is
    provided for setting the zero position
    along the real axis of the z-plane while
    maintaining a constant filter gain.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "OneZero.h"

using namespace Nyq;

OneZero :: OneZero() : Filter()
{
  std::vector<StkFloat> b(2, 0.5);
  std::vector<StkFloat> a(1, 1.0);
  Filter::setCoefficients( b, a );
}

OneZero :: OneZero(StkFloat theZero) : Filter()
{
  std::vector<StkFloat> b(2);
  std::vector<StkFloat> a(1, 1.0);

  // Normalize coefficients for unity gain.
  if (theZero > 0.0)
    b[0] = 1.0 / ((StkFloat) 1.0 + theZero);
  else
    b[0] = 1.0 / ((StkFloat) 1.0 - theZero);

  b[1] = -theZero * b[0];
  Filter::setCoefficients( b, a );
}

OneZero :: ~OneZero(void)
{
}

void OneZero :: clear(void)
{
  Filter::clear();
}

void OneZero :: setB0(StkFloat b0)
{
  b_[0] = b0;
}

void OneZero :: setB1(StkFloat b1)
{
  b_[1] = b1;
}

void OneZero :: setZero(StkFloat theZero)
{
  // Normalize coefficients for unity gain.
  if (theZero > 0.0)
    b_[0] = 1.0 / ((StkFloat) 1.0 + theZero);
  else
    b_[0] = 1.0 / ((StkFloat) 1.0 - theZero);

  b_[1] = -theZero * b_[0];
}

void OneZero :: setGain(StkFloat gain)
{
  Filter::setGain(gain);
}

StkFloat OneZero :: getGain(void) const
{
  return Filter::getGain();
}

StkFloat OneZero :: lastOut(void) const
{
  return Filter::lastOut();
}

StkFloat OneZero :: tick( StkFloat input )
{
  inputs_[0] = gain_ * input;
  outputs_[0] = b_[1] * inputs_[1] + b_[0] * inputs_[0];
  inputs_[1] = inputs_[0];

  return outputs_[0];
}

StkFrames& OneZero :: tick( StkFrames& frames, unsigned int channel )
{
  return Filter::tick( frames, channel );
}
