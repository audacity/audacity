/***************************************************/
/*! \class OnePole
    \brief STK one-pole filter class.

    This protected Filter subclass implements
    a one-pole digital filter.  A method is
    provided for setting the pole position along
    the real axis of the z-plane while maintaining
    a constant peak filter gain.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "OnePole.h"

using namespace Nyq;

OnePole :: OnePole() : Filter()
{
  std::vector<StkFloat> b(1, 0.1);
  std::vector<StkFloat> a(2, 1.0);
  a[1] = -0.9;
  Filter::setCoefficients( b, a );
}

OnePole :: OnePole(StkFloat thePole) : Filter()
{
  std::vector<StkFloat> b(1);
  std::vector<StkFloat> a(2, 1.0);
  a[1] = -thePole;

  // Normalize coefficients for peak unity gain.
  if (thePole > 0.0)
    b[0] = (StkFloat) (1.0 - thePole);
  else
    b[0] = (StkFloat) (1.0 + thePole);

  Filter::setCoefficients( b, a );
}

OnePole :: ~OnePole()    
{
}

void OnePole :: clear(void)
{
  Filter::clear();
}

void OnePole :: setB0(StkFloat b0)
{
  b_[0] = b0;
}

void OnePole :: setA1(StkFloat a1)
{
  a_[1] = a1;
}

void OnePole :: setPole(StkFloat thePole)
{
  // Normalize coefficients for peak unity gain.
  if (thePole > 0.0)
    b_[0] = (StkFloat) (1.0 - thePole);
  else
    b_[0] = (StkFloat) (1.0 + thePole);

  a_[1] = -thePole;
}

void OnePole :: setGain(StkFloat gain)
{
  Filter::setGain(gain);
}

StkFloat OnePole :: getGain(void) const
{
  return Filter::getGain();
}

StkFloat OnePole :: lastOut(void) const
{
  return Filter::lastOut();
}

StkFloat OnePole :: tick( StkFloat input )
{
  inputs_[0] = gain_ * input;
  outputs_[0] = b_[0] * inputs_[0] - a_[1] * outputs_[1];
  outputs_[1] = outputs_[0];

  return outputs_[0];
}

StkFrames& OnePole :: tick( StkFrames& frames, unsigned int channel )
{
  return Filter::tick( frames, channel );
}
