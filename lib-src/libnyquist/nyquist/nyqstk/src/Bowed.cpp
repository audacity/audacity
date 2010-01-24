/***************************************************/
/*! \class Bowed
    \brief STK bowed string instrument class.

    This class implements a bowed string model, a
    la Smith (1986), after McIntyre, Schumacher,
    Woodhouse (1983).

    This is a digital waveguide model, making its
    use possibly subject to patents held by
    Stanford University, Yamaha, and others.

    Control Change Numbers: 
       - Bow Pressure = 2
       - Bow Position = 4
       - Vibrato Frequency = 11
       - Vibrato Gain = 1
       - Volume = 128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Bowed.h"
#include "SKINI.msg"

using namespace Nyq;

Bowed :: Bowed(StkFloat lowestFrequency)
{
  unsigned long length;
  length = (long) ( Stk::sampleRate() / lowestFrequency + 1 );
  neckDelay_.setMaximumDelay( length );
  neckDelay_.setDelay( 100.0 );

  length >>= 1;
  bridgeDelay_.setMaximumDelay( length );
  bridgeDelay_.setDelay( 29.0 );

  bowTable_.setSlope(3.0 );

  vibrato_.setFrequency( 6.12723 );
  vibratoGain_ = 0.0;

  stringFilter_.setPole( 0.6 - (0.1 * 22050.0 / Stk::sampleRate()) );
  stringFilter_.setGain( 0.95 );

  bodyFilter_.setResonance( 500.0, 0.85, true );
  bodyFilter_.setGain( 0.2 );

  adsr_.setAllTimes( 0.02, 0.005, 0.9, 0.01 );
    
  betaRatio_ = 0.127236;

  // Necessary to initialize internal variables.
  this->setFrequency( 220.0 );
}

Bowed :: ~Bowed()
{
}

void Bowed :: clear()
{
  neckDelay_.clear();
  bridgeDelay_.clear();
}

void Bowed :: setFrequency(StkFloat frequency)
{
  StkFloat freakency = frequency;
  if ( frequency <= 0.0 ) {
    errorString_ << "Bowed::setFrequency: parameter is less than or equal to zero!";
    handleError( StkError::WARNING );
    freakency = 220.0;
  }

  // Delay = length - approximate filter delay.
  baseDelay_ = Stk::sampleRate() / freakency - 4.0;
  if ( baseDelay_ <= 0.0 ) baseDelay_ = 0.3;
  bridgeDelay_.setDelay( baseDelay_ * betaRatio_ ); 	     // bow to bridge length
  neckDelay_.setDelay( baseDelay_ * (1.0 - betaRatio_) );  // bow to nut (finger) length
}

void Bowed :: startBowing(StkFloat amplitude, StkFloat rate)
{
  adsr_.setRate( rate );
  adsr_.keyOn();
  maxVelocity_ = 0.03 + ( 0.2 * amplitude ); 
}

void Bowed :: stopBowing(StkFloat rate)
{
  adsr_.setRate( rate );
  adsr_.keyOff();
}

void Bowed :: noteOn(StkFloat frequency, StkFloat amplitude)
{
  this->startBowing( amplitude, amplitude * 0.001 );
  this->setFrequency( frequency );

#if defined(_STK_DEBUG_)
  errorString_ << "Bowed::NoteOn: frequency = " << frequency << ", amplitude = " << amplitude << ".";
  handleError( StkError::DEBUG_WARNING );
#endif
}

void Bowed :: noteOff(StkFloat amplitude)
{
  this->stopBowing( (1.0 - amplitude) * 0.005 );

#if defined(_STK_DEBUG_)
  errorString_ << "Bowed::NoteOff: amplitude = " << amplitude << ".";
  handleError( StkError::DEBUG_WARNING );
#endif
}

void Bowed :: setVibrato(StkFloat gain)
{
  vibratoGain_ = gain;
}

StkFloat Bowed :: computeSample()
{
  StkFloat bowVelocity;
  StkFloat bridgeRefl;
  StkFloat nutRefl;
  StkFloat newVel;
  StkFloat velDiff;
  StkFloat stringVel;
    
  bowVelocity = maxVelocity_ * adsr_.tick();

  bridgeRefl = -stringFilter_.tick( bridgeDelay_.lastOut() );
  nutRefl = -neckDelay_.lastOut();
  stringVel = bridgeRefl + nutRefl;               // Sum is String Velocity
  velDiff = bowVelocity - stringVel;              // Differential Velocity
  newVel = velDiff * bowTable_.tick( velDiff );   // Non-Linear Bow Function
  neckDelay_.tick(bridgeRefl + newVel);           // Do string propagations
  bridgeDelay_.tick(nutRefl + newVel);
    
  if ( vibratoGain_ > 0.0 )  {
    neckDelay_.setDelay( (baseDelay_ * (1.0 - betaRatio_) ) + 
                         (baseDelay_ * vibratoGain_ * vibrato_.tick()) );
  }

  lastOutput_ = bodyFilter_.tick( bridgeDelay_.lastOut() );

  return lastOutput_;
}

void Bowed :: controlChange(int number, StkFloat value)
{
  StkFloat norm = value * ONE_OVER_128;
  if ( norm < 0 ) {
    norm = 0.0;
    errorString_ << "Bowed::controlChange: control value less than zero ... setting to zero!";
    handleError( StkError::WARNING );
  }
  else if ( norm > 1.0 ) {
    norm = 1.0;
    errorString_ << "Bowed::controlChange: control value greater than 128.0 ... setting to 128.0!";
    handleError( StkError::WARNING );
  }

  if (number == __SK_BowPressure_) // 2
		bowTable_.setSlope( 5.0 - (4.0 * norm) );
  else if (number == __SK_BowPosition_) { // 4
		betaRatio_ = 0.027236 + (0.2 * norm);
    bridgeDelay_.setDelay( baseDelay_ * betaRatio_ );
    neckDelay_.setDelay( baseDelay_ * (1.0 - betaRatio_) );
  }
  else if (number == __SK_ModFrequency_) // 11
    vibrato_.setFrequency( norm * 12.0 );
  else if (number == __SK_ModWheel_) // 1
    vibratoGain_ = ( norm * 0.4 );
  else if (number == __SK_AfterTouch_Cont_) // 128
    adsr_.setTarget(norm);
  else {
    errorString_ << "Bowed::controlChange: undefined control number (" << number << ")!";
    handleError( StkError::WARNING );
  }

#if defined(_STK_DEBUG_)
    errorString_ << "Bowed::controlChange: number = " << number << ", value = " << value << ".";
    handleError( StkError::DEBUG_WARNING );
#endif
}
