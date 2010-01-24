/***************************************************/
/*! \class Chorus
    \brief STK chorus effect class.

    This class implements a chorus effect.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_CHORUS_H
#define STK_CHORUS_H

#include "Effect.h" 
#include "DelayL.h" 
#include "SineWave.h" 

namespace Nyq
{

class Chorus : public Effect
{
 public:
  //! Class constructor, taking the median desired delay length.
  /*!
    An StkError can be thrown if the rawwave path is incorrect.
  */
  Chorus( StkFloat baseDelay = 6000 );

  //! Class destructor.
  ~Chorus();

  //! Reset and clear all internal state.
  void clear();

  //! Set modulation depth.
  void setModDepth(StkFloat depth);

  //! Set modulation frequency.
  void setModFrequency(StkFloat frequency);

 protected:

  StkFloat computeSample( StkFloat input );

  DelayL delayLine_[2];
  SineWave mods_[2];
  StkFloat baseLength_;
  StkFloat modDepth_;

};

} // namespace Nyq

#endif

