/***************************************************/
/*! \class Mandolin
    \brief STK mandolin instrument model class.

    This class inherits from PluckTwo and uses
    "commuted synthesis" techniques to model a
    mandolin instrument.

    This is a digital waveguide model, making its
    use possibly subject to patents held by
    Stanford University, Yamaha, and others.
    Commuted Synthesis, in particular, is covered
    by patents, granted, pending, and/or
    applied-for.  All are assigned to the Board of
    Trustees, Stanford University.  For
    information, contact the Office of Technology
    Licensing, Stanford University.

    Control Change Numbers: 
       - Body Size = 2
       - Pluck Position = 4
       - String Sustain = 11
       - String Detuning = 1
       - Microphone Position = 128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Mandolin.h"
#include "SKINI.msg"

using namespace Nyq;

Mandolin :: Mandolin(StkFloat lowestFrequency)
  : PluckTwo(lowestFrequency)
{
  // Concatenate the STK rawwave path to the rawwave files
  soundfile_[0] = new FileWvIn( (Stk::rawwavePath() + "mand1.raw").c_str(), true );
  soundfile_[1] = new FileWvIn( (Stk::rawwavePath() + "mand2.raw").c_str(), true );
  soundfile_[2] = new FileWvIn( (Stk::rawwavePath() + "mand3.raw").c_str(), true );
  soundfile_[3] = new FileWvIn( (Stk::rawwavePath() + "mand4.raw").c_str(), true );
  soundfile_[4] = new FileWvIn( (Stk::rawwavePath() + "mand5.raw").c_str(), true );
  soundfile_[5] = new FileWvIn( (Stk::rawwavePath() + "mand6.raw").c_str(), true );
  soundfile_[6] = new FileWvIn( (Stk::rawwavePath() + "mand7.raw").c_str(), true );
  soundfile_[7] = new FileWvIn( (Stk::rawwavePath() + "mand8.raw").c_str(), true );
  soundfile_[8] = new FileWvIn( (Stk::rawwavePath() + "mand9.raw").c_str(), true );
  soundfile_[9] = new FileWvIn( (Stk::rawwavePath() + "mand10.raw").c_str(), true );
  soundfile_[10] = new FileWvIn( (Stk::rawwavePath() + "mand11.raw").c_str(), true );
  soundfile_[11] = new FileWvIn( (Stk::rawwavePath() + "mand12.raw").c_str(), true );

  mic_ = 0;
  dampTime_ = 0;
  waveDone_ = soundfile_[mic_]->isFinished();
}

Mandolin :: ~Mandolin()
{
  for ( int i=0; i<12; i++ )
    delete soundfile_[i];
}

void Mandolin :: pluck(StkFloat amplitude)
{
  // This function gets interesting, because pluck
  // may be longer than string length, so we just
  // reset the soundfile and add in the pluck in
  // the tick method.
  soundfile_[mic_]->reset();
  waveDone_ = false;
  pluckAmplitude_ = amplitude;
  if ( amplitude < 0.0 ) {
    errorString_ << "Mandolin::pluck: amplitude parameter less than zero ... setting to 0.0!";
    handleError( StkError::WARNING );
    pluckAmplitude_ = 0.0;
  }
  else if ( amplitude > 1.0 ) {
    errorString_ << "Mandolin::pluck: amplitude parameter greater than one ... setting to 1.0!";
    handleError( StkError::WARNING );
    pluckAmplitude_ = 1.0;
  }

  // Set the pick position, which puts zeroes at position * length.
  combDelay_.setDelay( 0.5 * pluckPosition_ * lastLength_ ); 
  dampTime_ = (long) lastLength_;   // See tick method below.
}

void Mandolin :: pluck(StkFloat amplitude, StkFloat position)
{
  // Pluck position puts zeroes at position * length.
  pluckPosition_ = position;
  if ( position < 0.0 ) {
    std::cerr << "Mandolin::pluck: position parameter less than zero ... setting to 0.0!";
    handleError( StkError::WARNING );
    pluckPosition_ = 0.0;
  }
  else if ( position > 1.0 ) {
    errorString_ << "Mandolin::pluck: amplitude parameter greater than one ... setting to 1.0!";
    handleError( StkError::WARNING );
    pluckPosition_ = 1.0;
  }

  this->pluck( amplitude );
}

void Mandolin :: noteOn(StkFloat frequency, StkFloat amplitude)
{
  this->setFrequency( frequency );
  this->pluck( amplitude );

#if defined(_STK_DEBUG_)
  errorString_ << "Mandolin::NoteOn: frequency = " << frequency << ", amplitude = " << amplitude << ".";
  handleError( StkError::DEBUG_WARNING );
#endif
}

void Mandolin :: setBodySize(StkFloat size)
{
  // Scale the commuted body response by its sample rate (22050).
  StkFloat rate = size * 22050.0 / Stk::sampleRate();
  for ( int i=0; i<12; i++ )
    soundfile_[i]->setRate( rate );
}

StkFloat Mandolin :: computeSample()
{
  StkFloat temp = 0.0;
  if ( !waveDone_ ) {
    // Scale the pluck excitation with comb
    // filtering for the duration of the file.
    temp = soundfile_[mic_]->tick() * pluckAmplitude_;
    temp = temp - combDelay_.tick(temp);
    waveDone_ = soundfile_[mic_]->isFinished();
  }

  // Damping hack to help avoid overflow on re-plucking.
  if ( dampTime_ >=0 ) {
    dampTime_ -= 1;
    // Calculate 1st delay filtered reflection plus pluck excitation.
    lastOutput_ = delayLine_.tick( filter_.tick( temp + (delayLine_.lastOut() * 0.7) ) );
    // Calculate 2nd delay just like the 1st.
    lastOutput_ += delayLine2_.tick( filter2_.tick( temp + (delayLine2_.lastOut() * 0.7) ) );
  }
  else { // No damping hack after 1 period.
    // Calculate 1st delay filtered reflection plus pluck excitation.
    lastOutput_ = delayLine_.tick( filter_.tick( temp + (delayLine_.lastOut() * loopGain_) ) );
    // Calculate 2nd delay just like the 1st.
    lastOutput_ += delayLine2_.tick( filter2_.tick( temp + (delayLine2_.lastOut() * loopGain_) ) );
  }

  lastOutput_ *= 0.3;
  return lastOutput_;
}

void Mandolin :: controlChange(int number, StkFloat value)
{
  StkFloat norm = value * ONE_OVER_128;
  if ( norm < 0 ) {
    norm = 0.0;
    errorString_ << "Mandolin::controlChange: control value less than zero ... setting to zero!";
    handleError( StkError::WARNING );
  }
  else if ( norm > 1.0 ) {
    norm = 1.0;
    errorString_ << "Mandolin::controlChange: control value greater than 128.0 ... setting to 128.0!";
    handleError( StkError::WARNING );
  }

  if (number == __SK_BodySize_) // 2
    this->setBodySize( norm * 2.0 );
  else if (number == __SK_PickPosition_) // 4
    this->setPluckPosition( norm );
  else if (number == __SK_StringDamping_) // 11
    this->setBaseLoopGain( 0.97 + (norm * 0.03));
  else if (number == __SK_StringDetune_) // 1
    this->setDetune( 1.0 - (norm * 0.1) );
  else if (number == __SK_AfterTouch_Cont_) // 128
    mic_ = (int) (norm * 11.0);
  else {
    errorString_ << "Mandolin::controlChange: undefined control number (" << number << ")!";
    handleError( StkError::WARNING );
  }

#if defined(_STK_DEBUG_)
    errorString_ << "Mandolin::controlChange: number = " << number << ", value = " << value << ".";
    handleError( StkError::DEBUG_WARNING );
#endif
}
