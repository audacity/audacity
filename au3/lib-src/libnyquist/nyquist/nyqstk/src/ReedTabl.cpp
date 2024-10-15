/***************************************************/
/*! \class ReedTabl
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

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/

#include "ReedTabl.h"

ReedTabl :: ReedTabl()
{
  offSet = (MY_FLOAT) 0.6;  // Offset is a bias, related to reed rest position.
  slope = (MY_FLOAT) -0.8;  // Slope corresponds loosely to reed stiffness.
}

ReedTabl :: ~ReedTabl()
{

}

void ReedTabl :: setOffset(MY_FLOAT aValue)
{
  offSet = aValue;
}

void ReedTabl :: setSlope(MY_FLOAT aValue)
{
  slope = aValue;
}

MY_FLOAT ReedTabl :: lastOut() const
{
    return lastOutput;
}

MY_FLOAT ReedTabl :: tick(MY_FLOAT input)    
{
  // The input is differential pressure across the reed.
  lastOutput = offSet + (slope * input);

  // If output is > 1, the reed has slammed shut and the
  // reflection function value saturates at 1.0.
  if (lastOutput > 1.0) lastOutput = (MY_FLOAT) 1.0;

  // This is nearly impossible in a physical system, but
  // a reflection function value of -1.0 corresponds to
  // an open end (and no discontinuity in bore profile).
  if (lastOutput < -1.0) lastOutput = (MY_FLOAT) -1.0;
  return lastOutput;
}

MY_FLOAT *ReedTabl :: tick(MY_FLOAT *vector, unsigned int vectorSize)
{
  for (unsigned int i=0; i<vectorSize; i++)
    vector[i] = tick(vector[i]);

  return vector;
}

