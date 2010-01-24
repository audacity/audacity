/***************************************************/
/*! \class ReedTable
    \brief STK reed table class.

    This class implements a simple one breakpoint,
    non-linear reed function, as described by
    Smith (1986).  This function is based on a
    memoryless non-linear spring model of the reed
    (the reed mass is ignored) which saturates when
    the reed collides with the mouthpiece facing.

    See McIntyre, Schumacher, & Woodhouse (1983),
    Smith (1986), Hirschman, Cook, Scavone, and
    others for more information.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "ReedTable.h"

using namespace Nyq;

ReedTable :: ReedTable()
{
  offset_ = (StkFloat) 0.6;  // Offset is a bias, related to reed rest position.
  slope_ = (StkFloat) -0.8;  // Slope corresponds loosely to reed stiffness.
}

ReedTable :: ~ReedTable()
{
}

void ReedTable :: setOffset(StkFloat offset)
{
  offset_ = offset;
}

void ReedTable :: setSlope(StkFloat slope)
{
  slope_ = slope;
}

StkFloat ReedTable :: computeSample(StkFloat input)    
{
  // The input is differential pressure across the reed.
  lastOutput_ = offset_ + (slope_ * input);

  // If output is > 1, the reed has slammed shut and the
  // reflection function value saturates at 1.0.
  if (lastOutput_ > 1.0) lastOutput_ = (StkFloat) 1.0;

  // This is nearly impossible in a physical system, but
  // a reflection function value of -1.0 corresponds to
  // an open end (and no discontinuity in bore profile).
  if (lastOutput_ < -1.0) lastOutput_ = (StkFloat) -1.0;
  return lastOutput_;
}

