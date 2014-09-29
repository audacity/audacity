/***************************************************/
/*! \class WaveLoop
    \brief STK waveform oscillator class.

    This class inherits from FileWvIn and provides audio file looping
    functionality.  Any audio file that can be loaded by FileRead can
    be looped using this class.

    WaveLoop supports multi-channel data.  It is important to
    distinguish the tick() methods, which return samples produced by
    averaging across sample frames, from the tickFrame() methods,
    which return references or pointers to multi-channel sample
    frames.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "WaveLoop.h"
#include <cmath>

using namespace Nyq;

WaveLoop :: WaveLoop( unsigned long chunkThreshold, unsigned long chunkSize )
  : FileWvIn( chunkThreshold, chunkSize ), phaseOffset_(0.0)
{
}

WaveLoop :: WaveLoop( std::string fileName, bool raw, bool doNormalize,
                      unsigned long chunkThreshold, unsigned long chunkSize )
  : FileWvIn( chunkThreshold, chunkSize ), phaseOffset_(0.0)
{
  this->openFile( fileName, raw, doNormalize );
}

WaveLoop :: ~WaveLoop()
{
}

void WaveLoop :: openFile( std::string fileName, bool raw, bool doNormalize )
{
  // Call close() in case another file is already open.
  this->closeFile();

  // Attempt to open the file ... an error might be thrown here.
  file_.open( fileName, raw );

  // Determine whether chunking or not.
  if ( file_.fileSize() > chunkThreshold_ ) {
    chunking_ = true;
    chunkPointer_ = 0;
    data_.resize( chunkSize_, file_.channels() );
    if ( doNormalize ) normalizing_ = true;
    else normalizing_ = false;
  }
  else {
    chunking_ = false;
    data_.resize( file_.fileSize() + 1, file_.channels() );
  }

  // Load all or part of the data.
  file_.read( data_, 0, doNormalize );

  if ( chunking_ ) { // If chunking, save the first sample frame for later.
    firstFrame_.resize( 1, data_.channels() );
    for ( unsigned int i=0; i<data_.channels(); i++ )
      firstFrame_[i] = data_[i];
  }
  else {  // If not chunking, copy the first sample frame to the last.
    for ( unsigned int i=0; i<data_.channels(); i++ )
      data_( data_.frames() - 1, i ) = data_[i];
  }

  // Resize our lastOutputs container.
  lastOutputs_.resize( 1, file_.channels() );

  // Set default rate based on file sampling rate.
  this->setRate( data_.dataRate() / Stk::sampleRate() );

  if ( doNormalize & !chunking_ ) this->normalize();

  this->reset();
}

void WaveLoop :: setRate( StkFloat rate )
{
  rate_ = rate;

  if ( fmod( rate_, 1.0 ) != 0.0 ) interpolate_ = true;
  else interpolate_ = false;
}

void WaveLoop :: setFrequency( StkFloat frequency )
{
  // This is a looping frequency.
  this->setRate( file_.fileSize() * frequency / Stk::sampleRate() );
}

void WaveLoop :: addTime( StkFloat time )
{
  // Add an absolute time in samples.
  time_ += time;

  StkFloat fileSize = file_.fileSize();
  while ( time_ < 0.0 )
    time_ += fileSize;
  while ( time_ >= fileSize )
    time_ -= fileSize;
}

void WaveLoop :: addPhase( StkFloat angle )
{
  // Add a time in cycles (one cycle = fileSize).
  StkFloat fileSize = file_.fileSize();
  time_ += fileSize * angle;

  while ( time_ < 0.0 )
    time_ += fileSize;
  while ( time_ >= fileSize )
    time_ -= fileSize;
}

void WaveLoop :: addPhaseOffset( StkFloat angle )
{
  // Add a phase offset in cycles, where 1.0 = fileSize.
  phaseOffset_ = file_.fileSize() * angle;
}

void WaveLoop :: computeFrame( void )
{
  // Check limits of time address ... if necessary, recalculate modulo
  // fileSize.
  StkFloat fileSize = file_.fileSize();
  while ( time_ < 0.0 )
    time_ += fileSize;
  while ( time_ >= fileSize )
    time_ -= fileSize;

  StkFloat tyme;
  if ( phaseOffset_ ) {
    tyme = time_ + phaseOffset_;
    while ( tyme < 0.0 )
      tyme += fileSize;
    while ( tyme >= fileSize )
      tyme -= fileSize;
  }
  else {
    tyme = time_;
  }

  if (chunking_) {

    // Check the time address vs. our current buffer limits.
    if ( ( time_ < (StkFloat) chunkPointer_ ) ||
         ( time_ > (StkFloat) ( chunkPointer_ + chunkSize_ - 1 ) ) ) {

      while ( time_ < (StkFloat) chunkPointer_ ) { // negative rate
        chunkPointer_ -= chunkSize_ - 1; // overlap chunks by one frame
        if ( chunkPointer_ < 0 ) chunkPointer_ = 0;
      }
      while ( time_ > (StkFloat) ( chunkPointer_ + chunkSize_ - 1 ) ) { // positive rate
        chunkPointer_ += chunkSize_ - 1; // overlap chunks by one frame
        if ( chunkPointer_ + chunkSize_ > file_.fileSize() ) { // at end of file
          chunkPointer_ = file_.fileSize() - chunkSize_ + 1; // leave extra frame at end of buffer
          // Now fill extra frame with first frame data.
          for ( unsigned int j=0; j<firstFrame_.channels(); j++ )
            data_( data_.frames() - 1, j ) = firstFrame_[j];
        }
      }

      // Load more data.
      file_.read( data_, chunkPointer_, normalizing_ );
    }

    // Adjust index for the current buffer.
    tyme -= chunkPointer_;
  }

  if ( interpolate_ ) {
    for ( unsigned int i=0; i<lastOutputs_.size(); i++ )
    lastOutputs_[i] = data_.interpolate( tyme, i );
  }
  else {
    for ( unsigned int i=0; i<lastOutputs_.size(); i++ )
      lastOutputs_[i] = data_( (size_t) tyme, i );
  }

  // Increment time, which can be negative.
  time_ += rate_;
}
