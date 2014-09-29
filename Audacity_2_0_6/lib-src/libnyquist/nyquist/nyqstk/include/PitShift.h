/***************************************************/
/*! \class PitShift
    \brief STK simple pitch shifter effect class.

    This class implements a simple pitch shifter
    using delay lines.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_PITSHIFT_H
#define STK_PITSHIFT_H

#include "Effect.h" 
#include "DelayL.h" 

namespace Nyq
{

class PitShift : public Effect
{
 public:
  //! Class constructor.
  PitShift();

  //! Class destructor.
  ~PitShift();

  //! Reset and clear all internal state.
  void clear();

  //! Set the pitch shift factor (1.0 produces no shift).
  void setShift(StkFloat shift);

 protected:

  StkFloat computeSample( StkFloat input );

  DelayL delayLine_[2];
  StkFloat delay_[2];
  StkFloat env_[2];
  StkFloat rate_;
  unsigned long delayLength;
  unsigned long halfLength;

};

} // namespace Nyq

#endif

