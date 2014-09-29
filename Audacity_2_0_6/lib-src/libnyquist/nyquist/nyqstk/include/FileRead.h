/***************************************************/
/*! \class FileRead
    \brief STK audio file input class.

    This class provides input support for various
    audio file formats.  Multi-channel (>2)
    soundfiles are supported.  The file data is
    returned via an external StkFrames object
    passed to the read() function.  This class
    does not store its own copy of the file data,
    rather the data is read directly from disk.

    FileRead currently supports uncompressed WAV,
    AIFF/AIFC, SND (AU), MAT-file (Matlab), and
    STK RAW file formats.  Signed integer (8-,
    16-, and 32-bit) and floating-point (32- and
    64-bit) data types are supported.  Compressed
    data types are not supported.

    STK RAW files have no header and are assumed
    to contain a monophonic stream of 16-bit
    signed integers in big-endian byte order at a
    sample rate of 22050 Hz.  MAT-file data should
    be saved in an array with each data channel
    filling a matrix row.  The sample rate for
    MAT-files is assumed to be 44100 Hz.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_FILEREAD_H
#define STK_FILEREAD_H

#include "Stk.h"

namespace Nyq
{

class FileRead : public Stk
{
public:
  //! Default constructor.
  FileRead();

  //! Overloaded constructor that opens a file during instantiation.
  /*!
    An StkError will be thrown if the file is not found or its
    format is unknown or unsupported.
  */
  FileRead( std::string fileName, bool typeRaw = false );

  //! Class destructor.
  ~FileRead();

  //! Open the specified file and determine its formatting.
  /*!
    An StkError will be thrown if the file is not found or its
    format is unknown or unsupported.  An optional parameter is
    provided to specify whether the input file is of type STK RAW
    (default = false).
  */
  void open( std::string fileName, bool typeRaw = false );

  //! If a file is open, close it.
  void close( void );

  //! Returns \e true if a file is currently open.
  bool isOpen( void );

  //! Return the file size in sample frames.
  unsigned long fileSize( void ) const { return fileSize_; };

  //! Return the number of audio channels in the file.
  unsigned int channels( void ) const { return channels_; };

  //! Return the file sample rate in Hz.
  /*!
    WAV, SND, and AIF formatted files specify a sample rate in
    their headers.  By definition, STK RAW files have a sample rate of
    22050 Hz.  MAT-files are assumed to have a rate of 44100 Hz.
  */
  StkFloat fileRate( void ) const { return fileRate_; };

  //! Read sample frames from the file into an StkFrames object.
  /*!
    The number of sample frames to read will be determined from the
    number of frames of the StkFrames argument.  If this size is
    larger than the available data in the file (given the file size
    and starting frame index), the extra frames will be unaffected
    (the StkFrames object will not be resized).  Optional parameters
    are provided to specify the starting sample frame within the file
    (default = 0) and whether to normalize the data with respect to
    fixed-point limits (default = true).  An StkError will be thrown
    if a file error occurs or if the number of channels in the
    StkFrames argument is not equal to that in the file.
   */
  void read( StkFrames& buffer, unsigned long startFrame = 0, bool doNormalize = true );

protected:

  // Get STK RAW file information.
  bool getRawInfo( const char *fileName );

  // Get WAV file header information.
  bool getWavInfo( const char *fileName );

  // Get SND (AU) file header information.
  bool getSndInfo( const char *fileName );

  // Get AIFF file header information.
  bool getAifInfo( const char *fileName );

  // Get MAT-file header information.
  bool getMatInfo( const char *fileName );

  FILE *fd_;
  bool byteswap_;
  bool wavFile_;
  unsigned long fileSize_;
  unsigned long dataOffset_;
  unsigned int channels_;
  StkFormat dataType_;
  StkFloat fileRate_;
};

} // namespace Nyq

#endif
