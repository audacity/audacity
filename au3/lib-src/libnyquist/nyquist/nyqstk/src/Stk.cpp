/***************************************************/
/*! \class Stk
    \brief STK base class

    Nearly all STK classes inherit from this class.
    The global sample rate can be queried and
    modified via Stk.  In addition, this class
    provides error handling and byte-swapping
    functions.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Stk.h"
#include <stdlib.h>

using namespace Nyq;

StkFloat Stk :: srate_ = (StkFloat) SRATE;
std::string Stk :: rawwavepath_ = RAWWAVE_PATH;
const Stk::StkFormat Stk :: STK_SINT8   = 0x1;
const Stk::StkFormat Stk :: STK_SINT16  = 0x2;
const Stk::StkFormat Stk :: STK_SINT24  = 0x4;
const Stk::StkFormat Stk :: STK_SINT32  = 0x8;
const Stk::StkFormat Stk :: STK_FLOAT32 = 0x10;
const Stk::StkFormat Stk :: STK_FLOAT64 = 0x20;
bool Stk :: showWarnings_ = false;
bool Stk :: printErrors_ = true;

Stk :: Stk(void)
{
}

Stk :: ~Stk(void)
{
}

void Stk :: setRawwavePath( std::string path )
{
  if ( !path.empty() )
    rawwavepath_ = path;

  // Make sure the path includes a "/"
  if ( rawwavepath_[rawwavepath_.length()-1] != '/' )
    rawwavepath_ += "/";
}

void Stk :: byteSwap16(unsigned char *ptr)
{
  unsigned char val;

  // Swap 1st and 2nd bytes
  val = *(ptr);
  *(ptr) = *(ptr+1);
  *(ptr+1) = val;
}

void Stk :: byteSwap32(unsigned char *ptr)
{
  unsigned char val;

  // Swap 1st and 4th bytes
  val = *(ptr);
  *(ptr) = *(ptr+3);
  *(ptr+3) = val;

  //Swap 2nd and 3rd bytes
  ptr += 1;
  val = *(ptr);
  *(ptr) = *(ptr+1);
  *(ptr+1) = val;
}

void Stk :: byteSwap64(unsigned char *ptr)
{
  unsigned char val;

  // Swap 1st and 8th bytes
  val = *(ptr);
  *(ptr) = *(ptr+7);
  *(ptr+7) = val;

  // Swap 2nd and 7th bytes
  ptr += 1;
  val = *(ptr);
  *(ptr) = *(ptr+5);
  *(ptr+5) = val;

  // Swap 3rd and 6th bytes
  ptr += 1;
  val = *(ptr);
  *(ptr) = *(ptr+3);
  *(ptr+3) = val;

  // Swap 4th and 5th bytes
  ptr += 1;
  val = *(ptr);
  *(ptr) = *(ptr+1);
  *(ptr+1) = val;
}

#if (defined(__OS_IRIX__) || defined(__OS_LINUX__) || defined(__OS_MACOSX__))
  #include <unistd.h>
#elif defined(__OS_WINDOWS__)
  #include <windows.h>
#endif

void Stk :: sleep(unsigned long milliseconds)
{
#if defined(__OS_WINDOWS__)
  Sleep((DWORD) milliseconds);
#elif (defined(__OS_IRIX__) || defined(__OS_LINUX__) || defined(__OS_MACOSX__))
  usleep( (unsigned long) (milliseconds * 1000.0) );
#endif
}

void Stk :: handleError( StkError::Type type )
{
  handleError( errorString_.str(), type );
  errorString_.str( std::string() ); // reset the ostringstream buffer
}

void Stk :: handleError( const char *message, StkError::Type type )
{
  std::string msg( message );
  handleError( msg, type );
}

void Stk :: handleError( std::string message, StkError::Type type )
{
  if ( type == StkError::WARNING || type == StkError::STATUS ) {
    if ( !showWarnings_ ) return;
    std::cerr << '\n' << message << '\n' << std::endl;
  }
  else if (type == StkError::DEBUG_WARNING) {
#if defined(_STK_DEBUG_)
    std::cerr << '\n' << message << '\n' << std::endl;
#endif
  }
  else {
    if ( printErrors_ ) {
      // Print error message before throwing.
      std::cerr << '\n' << message << '\n' << std::endl;
    }
    throw StkError(message, type);
  }
}

//
// StkFrames definitions
//

StkFrames :: StkFrames( unsigned int nFrames, unsigned int nChannels, bool interleaved )
  : nFrames_( nFrames ), nChannels_( nChannels ), interleaved_( interleaved )
{
  size_ = nFrames_ * nChannels_;
  bufferSize_ = size_;

  if ( size_ > 0 ) {
    data_ = (StkFloat *) calloc( size_, sizeof( StkFloat ) );
#if defined(_STK_DEBUG_)
    if ( data_ == NULL ) {
      std::string error = "StkFrames: memory allocation error in constructor!";
      Stk::handleError( error, StkError::MEMORY_ALLOCATION );
    }
#endif
  }
  else data_ = 0;

  dataRate_ = Stk::sampleRate();
}

StkFrames :: StkFrames( const StkFloat& value, unsigned int nFrames, unsigned int nChannels, bool interleaved )
  : nFrames_( nFrames ), nChannels_( nChannels ), interleaved_( interleaved )
{
  size_ = nFrames_ * nChannels_;
  bufferSize_ = size_;
  if ( size_ > 0 ) {
    data_ = (StkFloat *) malloc( size_ * sizeof( StkFloat ) );
#if defined(_STK_DEBUG_)
    if ( data_ == NULL ) {
      std::string error = "StkFrames: memory allocation error in constructor!";
      Stk::handleError( error, StkError::MEMORY_ALLOCATION );
    }
#endif
    for ( long i=0; i<(long)size_; i++ ) data_[i] = value;
  }
  else data_ = 0;

  dataRate_ = Stk::sampleRate();
}

StkFrames :: ~StkFrames()
{
  if ( data_ ) free( data_ );
}

bool StkFrames :: empty() const
{
  if ( size_ > 0 ) return false;
  else return true;
}

void StkFrames :: resize( size_t nFrames, unsigned int nChannels )
{
  nFrames_ = nFrames;
  nChannels_ = nChannels;

  size_ = nFrames_ * nChannels_;
  if ( size_ > bufferSize_ ) {
    if ( data_ ) free( data_ );
    data_ = (StkFloat *) malloc( size_ * sizeof( StkFloat ) );
#if defined(_STK_DEBUG_)
    if ( data_ == NULL ) {
      std::string error = "StkFrames::resize: memory allocation error!";
      Stk::handleError( error, StkError::MEMORY_ALLOCATION );
    }
#endif
    bufferSize_ = size_;
  }
}

void StkFrames :: resize( size_t nFrames, unsigned int nChannels, StkFloat value )
{
  this->resize( nFrames, nChannels );

  for ( size_t i=0; i<size_; i++ ) data_[i] = value;
}

StkFloat& StkFrames :: operator[] ( size_t n )
{
#if defined(_STK_DEBUG_)
    if ( n >= size_ ) {
      std::ostringstream error;
      error << "StkFrames::operator[]: invalid index (" << n << ") value!";
      Stk::handleError( error.str(), StkError::MEMORY_ACCESS );
    }
#endif

  return data_[n];
}

StkFloat StkFrames :: operator[] ( size_t n ) const
{
#if defined(_STK_DEBUG_)
    if ( n >= size_ ) {
      std::ostringstream error;
      error << "StkFrames::operator[]: invalid index (" << n << ") value!";
      Stk::handleError( error.str(), StkError::MEMORY_ACCESS );
    }
#endif

  return data_[n];
}

StkFloat& StkFrames :: operator() ( size_t frame, unsigned int channel )
{
#if defined(_STK_DEBUG_)
    if ( frame >= nFrames_ || channel >= nChannels_ ) {
      std::ostringstream error;
      error << "StkFrames::operator(): invalid frame (" << frame << ") or channel (" << channel << ") value!";
      Stk::handleError( error.str(), StkError::MEMORY_ACCESS );
    }
#endif

  if ( interleaved_ )
    return data_[ frame * nChannels_ + channel ];
  else
    return data_[ channel * nFrames_ + frame ];
}

StkFloat StkFrames :: operator() ( size_t frame, unsigned int channel ) const
{
#if defined(_STK_DEBUG_)
    if ( frame >= nFrames_ || channel >= nChannels_ ) {
      std::ostringstream error;
      error << "StkFrames::operator(): invalid frame (" << frame << ") or channel (" << channel << ") value!";
      Stk::handleError( error.str(), StkError::MEMORY_ACCESS );
    }
#endif

  if ( interleaved_ )
    return data_[ frame * nChannels_ + channel ];
  else
    return data_[ channel * nFrames_ + frame ];
}

StkFloat StkFrames :: interpolate( StkFloat frame, unsigned int channel ) const
{
#if defined(_STK_DEBUG_)
    if ( frame >= (StkFloat) nFrames_ || channel >= nChannels_ ) {
      std::ostringstream error;
      error << "StkFrames::interpolate: invalid frame (" << frame << ") or channel (" << channel << ") value!";
      Stk::handleError( error.str(), StkError::MEMORY_ACCESS );
    }
#endif

  size_t iIndex = ( size_t ) frame;                    // integer part of index
  StkFloat output, alpha = frame - (StkFloat) iIndex;  // fractional part of index

  if ( interleaved_ ) {
    iIndex = iIndex * nChannels_ + channel;
    output = data_[ iIndex ];
    output += ( alpha * ( data_[ iIndex + nChannels_ ] - output ) );
  }
  else {
    iIndex += channel * nFrames_;
    output = data_[ iIndex ];
    output += ( alpha * ( data_[ iIndex++ ] - output ) );
  }

  return output;
}
