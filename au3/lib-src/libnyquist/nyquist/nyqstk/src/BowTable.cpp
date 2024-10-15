/***************************************************/
/*! \class BowTable
    \brief STK bowed string table class.

    This class implements a simple bowed string
    non-linear function, as described by Smith (1986).

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "BowTable.h"
#include <math.h>

using namespace Nyq;

BowTable :: BowTable()
{
  offset_ = (StkFloat) 0.0;
  slope_ = (StkFloat) 0.1;
}

BowTable :: ~BowTable()
{
}

void BowTable :: setOffset(StkFloat offset)
{
  offset_ = offset;
}

void BowTable :: setSlope(StkFloat slope)
{
  slope_ = slope;
}

StkFloat BowTable :: computeSample(StkFloat input)
{
  // The input represents differential string vs. bow velocity.
  StkFloat sample;
  sample = input + offset_;  // add bias to input
  sample *= slope_;          // then scale it
  lastOutput_ = (StkFloat) fabs( (double) sample ) + (StkFloat) 0.75;
  lastOutput_ = (StkFloat) pow( lastOutput_, (StkFloat) -4.0 );

  // Set minimum friction to 0.0
  // if (lastOutput < 0.0 ) lastOutput = 0.0;
  // Set maximum friction to 1.0.
  if (lastOutput_ > 1.0 ) lastOutput_ = (StkFloat) 1.0;

  return lastOutput_;
}

