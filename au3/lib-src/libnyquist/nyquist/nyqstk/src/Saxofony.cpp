/***************************************************/
/*! \class Saxofony
    \brief STK faux conical bore reed instrument class.

    This class implements a "hybrid" digital
    waveguide instrument that can generate a
    variety of wind-like sounds.  It has also been
    referred to as the "blowed string" model.  The
    waveguide section is essentially that of a
    string, with one rigid and one lossy
    termination.  The non-linear function is a
    reed table.  The string can be "blown" at any
    point between the terminations, though just as
    with strings, it is impossible to excite the
    system at either end.  If the excitation is
    placed at the string mid-point, the sound is
    that of a clarinet.  At points closer to the
    "bridge", the sound is closer to that of a
    saxophone.  See Scavone (2002) for more details.

    This is a digital waveguide model, making its
    use possibly subject to patents held by Stanford
    University, Yamaha, and others.

    Control Change Numbers: 
       - Reed Stiffness = 2
       - Reed Aperture = 26
       - Noise Gain = 4
       - Blow Position = 11
       - Vibrato Frequency = 29
       - Vibrato Gain = 1
       - Breath Pressure = 128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Saxofony.h"
#include "SKINI.msg"

using namespace Nyq;

Saxofony :: Saxofony(StkFloat lowestFrequency)
{
  length_ = (unsigned long) (Stk::sampleRate() / lowestFrequency + 1);
  // Initialize blowing position to 0.2 of length / 2.
  position_ = 0.2;
  delays_[0].setMaximumDelay( length_ );
  delays_[0].setDelay( (1.0-position_) * (length_ >> 1) );
  delays_[1].setMaximumDelay( length_ );
  delays_[1].setDelay( (1.0-position_) * (length_ >> 1) );

  reedTable_.setOffset( 0.7 );
  reedTable_.setSlope( 0.3 );

  vibrato_.setFrequency((StkFloat) 5.735);

  outputGain_ = 0.3;
  noiseGain_ = 0.2;
  vibratoGain_ = 0.1;
}

Saxofony :: ~Saxofony()
{
}

void Saxofony :: clear()
{
  delays_[0].clear();
  delays_[1].clear();
  filter_.clear();
}

void Saxofony :: setFrequency(StkFloat frequency)
{
  StkFloat freakency = frequency;
  if ( frequency <= 0.0 ) {
    errorString_ << "Saxofony::setFrequency: parameter is less than or equal to zero!";
    handleError( StkError::WARNING );
    freakency = 220.0;
  }

  StkFloat delay = (Stk::sampleRate() / freakency) - (StkFloat) 3.0;
  if (delay <= 0.0) delay = 0.3;
  else if (delay > length_) delay = length_;

  delays_[0].setDelay( (1.0-position_) * delay );
  delays_[1].setDelay( position_ * delay );
}

void Saxofony :: setBlowPosition(StkFloat position)
{
  if ( position_ == position ) return;

  if ( position < 0.0 ) position_ = 0.0;
  else if ( position > 1.0 ) position_ = 1.0;
  else position_ = position;

  StkFloat totalDelay = delays_[0].getDelay();
  totalDelay += delays_[1].getDelay();

  delays_[0].setDelay( (1.0-position_) * totalDelay );
  delays_[1].setDelay( position_ * totalDelay );
}

void Saxofony :: startBlowing(StkFloat amplitude, StkFloat rate)
{
  envelope_.setRate( rate );
  envelope_.setTarget( amplitude );
}

void Saxofony :: stopBlowing(StkFloat rate)
{
  envelope_.setRate( rate );
  envelope_.setTarget( 0.0 );
}

void Saxofony :: noteOn(StkFloat frequency, StkFloat amplitude)
{
  this->setFrequency( frequency );
  this->startBlowing( 0.55 + (amplitude * 0.30), amplitude * 0.005 );
  outputGain_ = amplitude + 0.001;

#if defined(_STK_DEBUG_)
  errorString_ << "Saxofony::NoteOn: frequency = " << frequency << ", amplitude = " << amplitude << ".";
  handleError( StkError::DEBUG_WARNING );
#endif
}

void Saxofony :: noteOff(StkFloat amplitude)
{
  this->stopBlowing( amplitude * 0.01 );

#if defined(_STK_DEBUG_)
  errorString_ << "Saxofony::NoteOff: amplitude = " << amplitude << ".";
  handleError( StkError::DEBUG_WARNING );
#endif
}

StkFloat Saxofony :: computeSample()
{
  StkFloat pressureDiff;
  StkFloat breathPressure;
  StkFloat temp;

  // Calculate the breath pressure (envelope + noise + vibrato)
  breathPressure = envelope_.tick(); 
  breathPressure += breathPressure * noiseGain_ * noise_.tick();
  breathPressure += breathPressure * vibratoGain_ * vibrato_.tick();

  temp = -0.95 * filter_.tick( delays_[0].lastOut() );
  lastOutput_ = temp - delays_[1].lastOut();
  pressureDiff = breathPressure - lastOutput_;
  delays_[1].tick( temp );
  delays_[0].tick( breathPressure - (pressureDiff * reedTable_.tick(pressureDiff)) - temp );

  lastOutput_ *= outputGain_;
  return lastOutput_;
}

void Saxofony :: controlChange(int number, StkFloat value)
{
  StkFloat norm = value * ONE_OVER_128;
  if ( norm < 0 ) {
    norm = 0.0;
    errorString_ << "Saxofony::controlChange: control value less than zero ... setting to zero!";
    handleError( StkError::WARNING );
  }
  else if ( norm > 1.0 ) {
    norm = 1.0;
    errorString_ << "Saxofony::controlChange: control value greater than 128.0 ... setting to 128.0!";
    handleError( StkError::WARNING );
  }

  if (number == __SK_ReedStiffness_) // 2
    reedTable_.setSlope( 0.1 + (0.4 * norm) );
  else if (number == __SK_NoiseLevel_) // 4
    noiseGain_ = ( norm * 0.4 );
  else if (number == 29) // 29
    vibrato_.setFrequency( norm * 12.0 );
  else if (number == __SK_ModWheel_) // 1
    vibratoGain_ = ( norm * 0.5 );
  else if (number == __SK_AfterTouch_Cont_) // 128
    envelope_.setValue( norm );
  else if (number == 11) // 11
    this->setBlowPosition( norm );
  else if (number == 26) // reed table offset
    reedTable_.setOffset(0.4 + ( norm * 0.6));
  else {
    errorString_ << "Saxofony::controlChange: undefined control number (" << number << ")!";
    handleError( StkError::WARNING );
  }

#if defined(_STK_DEBUG_)
    errorString_ << "Saxofony::controlChange: number = " << number << ", value = " << value << ".";
    handleError( StkError::DEBUG_WARNING );
#endif
}
