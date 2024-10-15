/***************************************************/
/*! \class PluckTwo
    \brief STK enhanced plucked string model class.

    This class implements an enhanced two-string,
    plucked physical model, a la Jaffe-Smith,
    Smith, and others.

    PluckTwo is an abstract class, with no excitation
    specified.  Therefore, it can't be directly
    instantiated.

    This is a digital waveguide model, making its
    use possibly subject to patents held by
    Stanford University, Yamaha, and others.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_PLUCKTWO_H
#define STK_PLUCKTWO_H

#include "Instrmnt.h"
#include "DelayL.h"
#include "DelayA.h"
#include "OneZero.h"

namespace Nyq
{

class PluckTwo : public Instrmnt
{
 public:
  //! Class constructor, taking the lowest desired playing frequency.
  PluckTwo(StkFloat lowestFrequency);

  //! Class destructor.
  virtual ~PluckTwo();

  //! Reset and clear all internal state.
  void clear();

  //! Set instrument parameters for a particular frequency.
  virtual void setFrequency(StkFloat frequency);

  //! Detune the two strings by the given factor.  A value of 1.0 produces unison strings.
  void setDetune(StkFloat detune);

  //! Efficient combined setting of frequency and detuning.
  void setFreqAndDetune(StkFloat frequency, StkFloat detune);

  //! Set the pluck or "excitation" position along the string (0.0 - 1.0).
  void setPluckPosition(StkFloat position);

  //! Set the base loop gain.
  /*!
    The actual loop gain is set according to the frequency.
    Because of high-frequency loop filter roll-off, higher
    frequency settings have greater loop gains.
  */
  void setBaseLoopGain(StkFloat aGain);

  //! Stop a note with the given amplitude (speed of decay).
  virtual void noteOff(StkFloat amplitude);

 protected:

  virtual StkFloat computeSample( void ) = 0;

  DelayA  delayLine_;
  DelayA  delayLine2_;
  DelayL  combDelay_;
  OneZero filter_;
  OneZero filter2_;

  unsigned long length_;
  StkFloat loopGain_;
  StkFloat baseLoopGain_;
  StkFloat lastFrequency_;
  StkFloat lastLength_;
  StkFloat detuning_;
  StkFloat pluckAmplitude_;
  StkFloat pluckPosition_;

};

} // namespace Nyq

#endif
