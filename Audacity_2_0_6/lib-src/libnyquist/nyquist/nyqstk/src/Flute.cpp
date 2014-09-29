/***************************************************/
/*! \class Flute
    \brief STK flute physical model class.

    This class implements a simple flute
    physical model, as discussed by Karjalainen,
    Smith, Waryznyk, etc.  The jet model uses
    a polynomial, a la Cook.

    This is a digital waveguide model, making its
    use possibly subject to patents held by Stanford
    University, Yamaha, and others.

    Control Change Numbers: 
       - Jet Delay = 2
       - Noise Gain = 4
       - Vibrato Frequency = 11
       - Vibrato Gain = 1
       - Breath Pressure = 128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Flute.h"
#include "SKINI.msg"

using namespace Nyq;

Flute :: Flute(StkFloat lowestFrequency)
{
  length_ = (unsigned long) (Stk::sampleRate() / lowestFrequency + 1);
  boreDelay_.setMaximumDelay( length_ );
  boreDelay_.setDelay( 100.0 );

  length_ >>= 1;
  jetDelay_.setMaximumDelay( length_ );
  jetDelay_.setDelay( 49.0 );

  vibrato_.setFrequency( 5.925 );

  this->clear();

  filter_.setPole( 0.7 - ((StkFloat) 0.1 * 22050.0 / Stk::sampleRate() ) );
  filter_.setGain( -1.0 );

  dcBlock_.setBlockZero();

  adsr_.setAllTimes( 0.005, 0.01, 0.8, 0.010);
  endReflection_ = 0.5;
  jetReflection_ = 0.5;
  noiseGain_     = 0.15;    // Breath pressure random component.
  vibratoGain_   = 0.05;    // Breath periodic vibrato component.
  jetRatio_      = 0.32;

	maxPressure_ = 0.0;
  lastFrequency_ = 220.0;
}

Flute :: ~Flute()
{
}

void Flute :: clear()
{
  jetDelay_.clear();
  boreDelay_.clear();
  filter_.clear();
  dcBlock_.clear();
}

void Flute :: setFrequency(StkFloat frequency)
{
  lastFrequency_ = frequency;
  if ( frequency <= 0.0 ) {
    errorString_ << "Flute::setFrequency: parameter is less than or equal to zero!";
    handleError( StkError::WARNING );
    lastFrequency_ = 220.0;
  }

  // We're overblowing here.
  lastFrequency_ *= 0.66666;

  // delay = length - approximate filter delay.
  StkFloat delay = Stk::sampleRate() / lastFrequency_ - (StkFloat) 2.0;
  if ( delay <= 0.0 ) delay = 0.3;
  else if ( delay > length_ ) delay = length_;

  boreDelay_.setDelay(delay);
  jetDelay_.setDelay(delay * jetRatio_);
}

void Flute :: startBlowing(StkFloat amplitude, StkFloat rate)
{
  adsr_.setAttackRate( rate );
  maxPressure_ = amplitude / (StkFloat) 0.8;
  adsr_.keyOn();
}

void Flute :: stopBlowing(StkFloat rate)
{
  adsr_.setReleaseRate( rate );
  adsr_.keyOff();
}

void Flute :: noteOn(StkFloat frequency, StkFloat amplitude)
{
  this->setFrequency( frequency );
  this->startBlowing( 1.1 + (amplitude * 0.20), amplitude * 0.02 );
  outputGain_ = amplitude + 0.001;

#if defined(_STK_DEBUG_)
  errorString_ << "Flute::NoteOn: frequency = " << frequency << ", amplitude = " << amplitude << ".";
  handleError( StkError::DEBUG_WARNING );
#endif
}

void Flute :: noteOff(StkFloat amplitude)
{
  this->stopBlowing( amplitude * 0.02 );

#if defined(_STK_DEBUG_)
  errorString_ << "Flute::NoteOff: amplitude = " << amplitude << ".";
  handleError( StkError::DEBUG_WARNING );
#endif
}

void Flute :: setJetReflection(StkFloat coefficient)
{
  jetReflection_ = coefficient;
}

void Flute :: setEndReflection(StkFloat coefficient)
{         
  endReflection_ = coefficient;
}               

void Flute :: setJetDelay(StkFloat aRatio)
{
  // Delay = length - approximate filter delay.
  StkFloat temp = Stk::sampleRate() / lastFrequency_ - (StkFloat) 2.0;
  jetRatio_ = aRatio;
  jetDelay_.setDelay(temp * aRatio); // Scaled by ratio.
}

StkFloat Flute :: computeSample()
{
  StkFloat pressureDiff;
  StkFloat breathPressure;

  // Calculate the breath pressure (envelope + noise + vibrato)
  breathPressure = maxPressure_ * adsr_.tick();
  breathPressure += breathPressure * ( noiseGain_ * noise_.tick() + vibratoGain_ * vibrato_.tick() );
  //breathPressure += breathPressure * vibratoGain_ * vibrato_.tick();

  StkFloat temp = filter_.tick( boreDelay_.lastOut() );
  temp = dcBlock_.tick( temp ); // Block DC on reflection.

  pressureDiff = breathPressure - (jetReflection_ * temp);
  pressureDiff = jetDelay_.tick( pressureDiff );
  pressureDiff = jetTable_.tick( pressureDiff ) + (endReflection_ * temp);
  lastOutput_ = (StkFloat) 0.3 * boreDelay_.tick( pressureDiff );

  lastOutput_ *= outputGain_;
  return lastOutput_;
}

void Flute :: controlChange(int number, StkFloat value)
{
  StkFloat norm = value * ONE_OVER_128;
  if ( norm < 0 ) {
    norm = 0.0;
    errorString_ << "Flute::controlChange: control value less than zero ... setting to zero!";
    handleError( StkError::WARNING );
  }
  else if ( norm > 1.0 ) {
    norm = 1.0;
    errorString_ << "Flute::controlChange: control value greater than 128.0 ... setting to 128.0!";
    handleError( StkError::WARNING );
  }

  if (number == __SK_JetDelay_) // 2
    this->setJetDelay( (StkFloat) (0.08 + (0.48 * norm)) );
  else if (number == __SK_NoiseLevel_) // 4
    noiseGain_ = ( norm * 0.4);
  else if (number == __SK_ModFrequency_) // 11
    vibrato_.setFrequency( norm * 12.0);
  else if (number == __SK_ModWheel_) // 1
    vibratoGain_ = ( norm * 0.4 );
  else if (number == __SK_AfterTouch_Cont_) // 128
    adsr_.setTarget( norm );
  else {
    errorString_ << "Flute::controlChange: undefined control number (" << number << ")!";
    handleError( StkError::WARNING );
  }

#if defined(_STK_DEBUG_)
    errorString_ << "Flute::controlChange: number = " << number << ", value = " << value << ".";
    handleError( StkError::DEBUG_WARNING );
#endif
}
