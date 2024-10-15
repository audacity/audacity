/***************************************************/
/*! \class Modal
    \brief STK resonance model instrument.

    This class contains an excitation wavetable,
    an envelope, an oscillator, and N resonances
    (non-sweeping BiQuad filters), where N is set
    during instantiation.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Modal.h"
#include <stdlib.h>

using namespace Nyq;

Modal :: Modal(unsigned int modes)
  : nModes_(modes)
{
  if ( nModes_ == 0 ) {
    errorString_ << "Modal: 'modes' argument to constructor is zero!";
    handleError( StkError::FUNCTION_ARGUMENT );
  }

  // We don't make the excitation wave here yet, because we don't know
  // what it's going to be.

  ratios_.resize( nModes_ );
  radii_.resize( nModes_ );
  filters_ = (BiQuad **) calloc( nModes_, sizeof(BiQuad *) );
  for (unsigned int i=0; i<nModes_; i++ ) {
    filters_[i] = new BiQuad;
    filters_[i]->setEqualGainZeroes();
  }

  // Set some default values.
  vibrato_.setFrequency( 6.0 );
  vibratoGain_ = 0.0;
  directGain_ = 0.0;
  masterGain_ = 1.0;
  baseFrequency_ = 440.0;

  this->clear();

  stickHardness_ =  0.5;
  strikePosition_ = 0.561;
}  

Modal :: ~Modal()
{
  for (unsigned int i=0; i<nModes_; i++ ) {
    delete filters_[i];
  }
  free(filters_);
}

void Modal :: clear()
{    
  onepole_.clear();
  for (unsigned int i=0; i<nModes_; i++ )
    filters_[i]->clear();
}

void Modal :: setFrequency(StkFloat frequency)
{
  baseFrequency_ = frequency;
  for (unsigned int i=0; i<nModes_; i++ )
    this->setRatioAndRadius( i, ratios_[i], radii_[i] );
}

void Modal :: setRatioAndRadius(unsigned int modeIndex, StkFloat ratio, StkFloat radius)
{
  if ( modeIndex >= nModes_ ) {
    errorString_ << "Modal::setRatioAndRadius: modeIndex parameter is greater than number of modes!";
    handleError( StkError::WARNING );
    return;
  }

  StkFloat nyquist = Stk::sampleRate() / 2.0;
  StkFloat temp;

  if ( ratio * baseFrequency_ < nyquist ) {
    ratios_[modeIndex] = ratio;
  }
  else {
    temp = ratio;
    while (temp * baseFrequency_ > nyquist) temp *= 0.5;
    ratios_[modeIndex] = temp;
#if defined(_STK_DEBUG_)
    errorString_ << "Modal::setRatioAndRadius: aliasing would occur here ... correcting.";
    handleError( StkError::DEBUG_WARNING );
#endif
  }
  radii_[modeIndex] = radius;
  if (ratio < 0) 
    temp = -ratio;
  else
    temp = ratio * baseFrequency_;

  filters_[modeIndex]->setResonance(temp, radius);
}

void Modal :: setMasterGain(StkFloat aGain)
{
  masterGain_ = aGain;
}

void Modal :: setDirectGain(StkFloat aGain)
{
  directGain_ = aGain;
}

void Modal :: setModeGain(unsigned int modeIndex, StkFloat gain)
{
  if ( modeIndex >= nModes_ ) {
    errorString_ << "Modal::setModeGain: modeIndex parameter is greater than number of modes!";
    handleError( StkError::WARNING );
    return;
  }

  filters_[modeIndex]->setGain(gain);
}

void Modal :: strike(StkFloat amplitude)
{
  StkFloat gain = amplitude;
  if ( amplitude < 0.0 ) {
    errorString_ << "Modal::strike: amplitude is less than zero ... setting to zero!";
    handleError( StkError::WARNING );
    gain = 0.0;
  }
  else if ( amplitude > 1.0 ) {
    errorString_ << "Modal::strike: amplitude is greater than one ... setting to 1.0!";
    handleError( StkError::WARNING );
    gain = 1.0;
  }

  envelope_.setRate( 1.0 );
  envelope_.setTarget( gain );
  onepole_.setPole( 1.0 - gain );
  envelope_.tick();
  wave_->reset();

  StkFloat temp;
  for (unsigned int i=0; i<nModes_; i++) {
    if (ratios_[i] < 0)
      temp = -ratios_[i];
    else
      temp = ratios_[i] * baseFrequency_;
    filters_[i]->setResonance(temp, radii_[i]);
  }
}

void Modal :: noteOn(StkFloat frequency, StkFloat amplitude)
{
  this->strike(amplitude);
  this->setFrequency(frequency);

#if defined(_STK_DEBUG_)
  errorString_ << "Modal::NoteOn: frequency = " << frequency << ", amplitude = " << amplitude << '.';
  handleError( StkError::DEBUG_WARNING );
#endif
}

void Modal :: noteOff(StkFloat amplitude)
{
  // This calls damp, but inverts the meaning of amplitude (high
  // amplitude means fast damping).
  this->damp( 1.0 - (amplitude * 0.03) );

#if defined(_STK_DEBUG_)
  errorString_ << "Modal::NoteOff: amplitude = " << amplitude << '.';
  handleError( StkError::DEBUG_WARNING );
#endif
}

void Modal :: damp(StkFloat amplitude)
{
  StkFloat temp;
  for (unsigned int i=0; i<nModes_; i++) {
    if (ratios_[i] < 0)
      temp = -ratios_[i];
    else
      temp = ratios_[i] * baseFrequency_;
    filters_[i]->setResonance(temp, radii_[i]*amplitude);
  }
}

StkFloat Modal :: computeSample()
{
  StkFloat temp = masterGain_ * onepole_.tick( wave_->tick() * envelope_.tick() );

  StkFloat temp2 = 0.0;
  for (unsigned int i=0; i<nModes_; i++)
    temp2 += filters_[i]->tick(temp);

  temp2  -= temp2 * directGain_;
  temp2 += directGain_ * temp;

  if (vibratoGain_ != 0.0)	{
    // Calculate AM and apply to master out
    temp = 1.0 + (vibrato_.tick() * vibratoGain_);
    temp2 = temp * temp2;
  }
    
  lastOutput_ = temp2;
  return lastOutput_;
}
