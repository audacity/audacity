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

#include "ADSR.h"

using namespace Nyq;

ADSR :: ADSR() : Envelope()
{
  target_ = 0.0;
  value_ = 0.0;
  attackRate_ = 0.001;
  decayRate_ = 0.001;
  sustainLevel_ = 0.5;
  releaseRate_ = 0.01;
  state_ = ATTACK;
}

ADSR :: ~ADSR()
{
}

void ADSR :: keyOn()
{
  target_ = 1.0;
  rate_ = attackRate_;
  state_ = ATTACK;
}

void ADSR :: keyOff()
{
  target_ = 0.0;
  rate_ = releaseRate_;
  state_ = RELEASE;
}

void ADSR :: setAttackRate(StkFloat rate)
{
  if (rate < 0.0) {
    errorString_ << "ADSR::setAttackRate: negative rates not allowed ... correcting!";
    handleError( StkError::WARNING );
    attackRate_ = -rate;
  }
  else attackRate_ = rate;
}

void ADSR :: setDecayRate(StkFloat rate)
{
  if (rate < 0.0) {
    errorString_ << "ADSR::setDecayRate: negative rates not allowed ... correcting!";
    handleError( StkError::WARNING );
    decayRate_ = -rate;
  }
  else decayRate_ = rate;
}

void ADSR :: setSustainLevel(StkFloat level)
{
  if (level < 0.0 ) {
    errorString_ << "ADSR::setSustainLevel: level out of range ... correcting!";
    handleError( StkError::WARNING );
    sustainLevel_ = 0.0;
  }
  else sustainLevel_ = level;
}

void ADSR :: setReleaseRate(StkFloat rate)
{
  if (rate < 0.0) {
    errorString_ << "ADSR::setReleaseRate: negative rates not allowed ... correcting!";
    handleError( StkError::WARNING );
    releaseRate_ = -rate;
  }
  else releaseRate_ = rate;
}

void ADSR :: setAttackTime(StkFloat time)
{
  if (time < 0.0) {
    errorString_ << "ADSR::setAttackTime: negative times not allowed ... correcting!";
    handleError( StkError::WARNING );
    attackRate_ = 1.0 / ( -time * Stk::sampleRate() );
  }
  else attackRate_ = 1.0 / ( time * Stk::sampleRate() );
}

void ADSR :: setDecayTime(StkFloat time)
{
  if (time < 0.0) {
    errorString_ << "ADSR::setDecayTime: negative times not allowed ... correcting!";
    handleError( StkError::WARNING );
    decayRate_ = 1.0 / ( -time * Stk::sampleRate() );
  }
  else decayRate_ = 1.0 / ( time * Stk::sampleRate() );
}

void ADSR :: setReleaseTime(StkFloat time)
{
  if (time < 0.0) {
    errorString_ << "ADSR::setReleaseTime: negative times not allowed ... correcting!";
    handleError( StkError::WARNING );
    releaseRate_ = sustainLevel_ / ( -time * Stk::sampleRate() );
  }
  else releaseRate_ = sustainLevel_ / ( time * Stk::sampleRate() );
}

void ADSR :: setAllTimes(StkFloat aTime, StkFloat dTime, StkFloat sLevel, StkFloat rTime)
{
  this->setAttackTime(aTime);
  this->setDecayTime(dTime);
  this->setSustainLevel(sLevel);
  this->setReleaseTime(rTime);
}

void ADSR :: setTarget(StkFloat target)
{
  target_ = target;
  if (value_ < target_) {
    state_ = ATTACK;
    this->setSustainLevel(target_);
    rate_ = attackRate_;
  }
  if (value_ > target_) {
    this->setSustainLevel(target_);
    state_ = DECAY;
    rate_ = decayRate_;
  }
}

void ADSR :: setValue(StkFloat value)
{
  state_ = SUSTAIN;
  target_ = value;
  value_ = value;
  this->setSustainLevel(value);
  rate_ = (StkFloat)  0.0;
}

int ADSR :: getState(void) const
{
  return state_;
}

StkFloat ADSR :: computeSample()
{
  switch (state_) {

  case ATTACK:
    value_ += rate_;
    if (value_ >= target_) {
      value_ = target_;
      rate_ = decayRate_;
      target_ = sustainLevel_;
	    state_ = DECAY;
    }
    break;

  case DECAY:
    value_ -= decayRate_;
    if (value_ <= sustainLevel_) {
      value_ = sustainLevel_;
      rate_ = (StkFloat) 0.0;
      state_ = SUSTAIN;
    }
    break;

  case RELEASE:
    value_ -= releaseRate_;
    if (value_ <= 0.0)       {
      value_ = (StkFloat) 0.0;
      state_ = DONE;
    }
  }

  lastOutput_ = value_;
  return value_;
}
