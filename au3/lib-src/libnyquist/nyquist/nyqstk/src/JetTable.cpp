/***************************************************/
/*! \class JetTable
    \brief STK jet table class.

    This class implements a flue jet non-linear
    function, computed by a polynomial calculation.
    Contrary to the name, this is not a "table".

    Consult Fletcher and Rossing, Karjalainen,
    Cook, and others for more information.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "JetTable.h"

using namespace Nyq;

JetTable :: JetTable() : Function()
{
}

JetTable :: ~JetTable()
{
}

StkFloat JetTable :: computeSample( StkFloat input )
{
  // Perform "table lookup" using a polynomial
  // calculation (x^3 - x), which approximates
  // the jet sigmoid behavior.
  lastOutput_ = input * (input * input - (StkFloat)  1.0);

  // Saturate at +/- 1.0.
  if (lastOutput_ > 1.0) 
    lastOutput_ = (StkFloat) 1.0;
  if (lastOutput_ < -1.0)
    lastOutput_ = (StkFloat) -1.0; 
  return lastOutput_;
}
