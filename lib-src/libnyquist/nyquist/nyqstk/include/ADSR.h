/***************************************************/
/*! \class ADSR
    \brief STK ADSR envelope class.

    This Envelope subclass implements a
    traditional ADSR (Attack, Decay,
    Sustain, Release) envelope.  It
    responds to simple keyOn and keyOff
    messages, keeping track of its state.
    The \e state = ADSR::DONE after the
    envelope value reaches 0.0 in the
    ADSR::RELEASE state.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_ADSR_H
#define STK_ADSR_H

#include "Envelope.h"

namespace Nyq
{

class ADSR : public Envelope
{
 public:

  //! Envelope states.
  enum { ATTACK, DECAY, SUSTAIN, RELEASE, DONE };

  //! Default constructor.
  ADSR(void);

  //! Class destructor.
  ~ADSR(void);

  //! Set target = 1, state = \e ADSR::ATTACK.
  void keyOn(void);

  //! Set target = 0, state = \e ADSR::RELEASE.
  void keyOff(void);

  //! Set the attack rate.
  void setAttackRate(StkFloat rate);

  //! Set the decay rate.
  void setDecayRate(StkFloat rate);

  //! Set the sustain level.
  void setSustainLevel(StkFloat level);

  //! Set the release rate.
  void setReleaseRate(StkFloat rate);

  //! Set the attack rate based on a time duration.
  void setAttackTime(StkFloat time);

  //! Set the decay rate based on a time duration.
  void setDecayTime(StkFloat time);

  //! Set the release rate based on a time duration.
  void setReleaseTime(StkFloat time);

  //! Set sustain level and attack, decay, and release time durations.
  void setAllTimes(StkFloat aTime, StkFloat dTime, StkFloat sLevel, StkFloat rTime);

  //! Set the target value.
  void setTarget(StkFloat target);

  //! Return the current envelope \e state (ATTACK, DECAY, SUSTAIN, RELEASE, DONE).
  int getState(void) const;

  //! Set to state = ADSR::SUSTAIN with current and target values of \e aValue.
  void setValue(StkFloat value);

 protected:  

  StkFloat computeSample( void );

  StkFloat attackRate_;
  StkFloat decayRate_;
  StkFloat sustainLevel_;
  StkFloat releaseRate_;
};

} // namespace Nyq

#endif
