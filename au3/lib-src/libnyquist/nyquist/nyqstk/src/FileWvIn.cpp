/***************************************************/
/*! \class FileWvIn
    \brief STK audio file input class.

    This class inherits from WvIn.  It provides a "tick-level"
    interface to the FileRead class.  It also provides variable-rate
    "playback" functionality.  Audio file support is provided by the
    FileRead class.  Linear interpolation is used for fractional "read
    rates".

    FileWvIn supports multi-channel data.  It is important to distinguish
    the tick() methods, which return samples produced by averaging
    across sample frames, from the tickFrame() methods, which return
    references to multi-channel sample frames.

    FileWvIn will either load the entire content of an audio file into
    local memory or incrementally read file data from disk in chunks.
    This behavior is controlled by the optional constructor arguments
    \e chunkThreshold and \e chunkSize.  File sizes greater than \e
    chunkThreshold (in sample frames) will be read incrementally in
    chunks of \e chunkSize each (also in sample frames).

    When the end of a file is reached, subsequent calls to the tick()
    functions return zero-valued data.

    See the FileRead class for a description of the supported audio
    file formats.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "FileWvIn.h"
#include <cmath>

using namespace Nyq;

FileWvIn :: FileWvIn( unsigned long chunkThreshold, unsigned long chunkSize )
  : finished_(true), interpolate_(false), time_(0.0),
    chunkThreshold_(chunkThreshold), chunkSize_(chunkSize)
{
}

FileWvIn :: FileWvIn( std::string fileName, bool raw, bool doNormalize,
                      unsigned long chunkThreshold, unsigned long chunkSize )
  : finished_(true), interpolate_(false), time_(0.0),
    chunkThreshold_(chunkThreshold), chunkSize_(chunkSize)
{
  openFile( fileName, raw, doNormalize );
}

FileWvIn :: ~FileWvIn()
{
  this->closeFile();
}

void FileWvIn :: closeFile( void )
{
  if ( file_.isOpen() ) file_.close();
  finished_ = true;
}

void FileWvIn :: openFile( std::string fileName, bool raw, bool doNormalize )
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
    data_.resize( (size_t) file_.fileSize(), file_.channels() );
  }

  // Load all or part of the data.
  file_.read( data_, 0, doNormalize );

  // Resize our lastOutputs container.
  lastOutputs_.resize( 1, file_.channels() );

  // Set default rate based on file sampling rate.
  this->setRate( data_.dataRate() / Stk::sampleRate() );

  if ( doNormalize & !chunking_ ) this->normalize();

  this->reset();
}

void FileWvIn :: reset(void)
{
  time_ = (StkFloat) 0.0;
  for ( unsigned int i=0; i<lastOutputs_.size(); i++ )
    lastOutputs_[i] = 0.0;
  finished_ = false;
}

void FileWvIn :: normalize(void)
{
  this->normalize( 1.0 );
}

// Normalize all channels equally by the greatest magnitude in all of the data.
void FileWvIn :: normalize( StkFloat peak )
{
  // When chunking, the "normalization" scaling is performed by FileRead.
  if ( chunking_ ) return;

  size_t i;
  StkFloat max = 0.0;

  for ( i=0; i<data_.size(); i++ ) {
    if ( fabs( data_[i] ) > max )
      max = (StkFloat) fabs((double) data_[i]);
  }

  if (max > 0.0) {
    max = 1.0 / max;
    max *= peak;
    for ( i=0; i<data_.size(); i++ )
	    data_[i] *= max;
  }
}

void FileWvIn :: setRate( StkFloat rate )
{
  rate_ = rate;

  // If negative rate and at beginning of sound, move pointer to end
  // of sound.
  if ( (rate_ < 0) && (time_ == 0.0) ) time_ = file_.fileSize() - 1.0;

  if ( fmod( rate_, 1.0 ) != 0.0 ) interpolate_ = true;
  else interpolate_ = false;
}

void FileWvIn :: addTime( StkFloat time )   
{
  // Add an absolute time in samples 
  time_ += time;

  if ( time_ < 0.0 ) time_ = 0.0;
  if ( time_ > file_.fileSize() - 1.0 ) {
    time_ = file_.fileSize() - 1.0;
    for ( unsigned int i=0; i<lastOutputs_.size(); i++ )
      lastOutputs_[i] = 0.0;
    finished_ = true;
  }
}

StkFloat FileWvIn :: lastOut( void ) const
{
  if ( finished_ ) return 0.0;
  return WvIn :: lastOut();
}

void FileWvIn :: computeFrame( void )
{
  if ( finished_ ) return;

  if ( time_ < 0.0 || time_ > (StkFloat) ( file_.fileSize() - 1.0 ) ) {
    for ( unsigned int i=0; i<lastOutputs_.size(); i++ )
      lastOutputs_[i] = 0.0;
    finished_ = true;
    return;
  }

  StkFloat tyme = time_;
  if ( chunking_ ) {

    // Check the time address vs. our current buffer limits.
    if ( ( time_ < (StkFloat) chunkPointer_ ) ||
         ( time_ > (StkFloat) ( chunkPointer_ + chunkSize_ - 1 ) ) ) {

      while ( time_ < (StkFloat) chunkPointer_ ) { // negative rate
        chunkPointer_ -= chunkSize_ - 1; // overlap chunks by one frame
        if ( chunkPointer_ < 0 ) chunkPointer_ = 0;
      }
      while ( time_ > (StkFloat) ( chunkPointer_ + chunkSize_ - 1 ) ) { // positive rate
        chunkPointer_ += chunkSize_ - 1; // overlap chunks by one frame
        if ( chunkPointer_ + chunkSize_ > file_.fileSize() ) // at end of file
          chunkPointer_ = file_.fileSize() - chunkSize_;
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

