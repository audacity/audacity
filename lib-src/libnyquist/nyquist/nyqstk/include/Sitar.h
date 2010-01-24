/***************************************************/
/*! \class Sitar
    \brief STK sitar string model class.

    This class implements a sitar plucked string
    physical model based on the Karplus-Strong
    algorithm.

    This is a digital waveguide model, making its
    use possibly subject to patents held by
    Stanford University, Yamaha, and others.
    There exist at least two patents, assigned to
    Stanford, bearing the names of Karplus and/or
    Strong.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_SITAR_H
#define STK_SITAR_H

#include "Instrmnt.h"
#include "DelayA.h"
#include "OneZero.h"
#include "Noise.h"
#include "ADSR.h"

namespace Nyq
{

class Sitar : public Instrmnt
{
 public:
  //! Class constructor, taking the lowest desired playing frequency.
  Sitar( StkFloat lowestFrequency = 20 );

  //! Class destructor.
  ~Sitar();

  //! Reset and clear all internal state.
  void clear();

  //! Set instrument parameters for a particular frequency.
  void setFrequency(StkFloat frequency);

  //! Pluck the string with the given amplitude using the current frequency.
  void pluck(StkFloat amplitude);

  //! Start a note with the given frequency and amplitude.
  void noteOn(StkFloat frequency, StkFloat amplitude);

  //! Stop a note with the given amplitude (speed of decay).
  void noteOff(StkFloat amplitude);

 protected:

  StkFloat computeSample( void );

  DelayA  delayLine_;
  OneZero loopFilter_;
  Noise   noise_;
  ADSR    envelope_;

  StkFloat loopGain_;
  StkFloat amGain_;
  StkFloat delay_;
  StkFloat targetDelay_;

};

} // namespace Nyq

#endif

