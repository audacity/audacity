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

#include "string.h"
#include "FileRead.h"
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <cmath>
#include <cstdio>

/* this is defined in xlisp.h, but it seems a bad idea
 * to create an stk dependency on xlisp, or to add a new
 * security.h file to share between xlisp.h and stk
 */
extern "C" {
    int ok_to_open(const char *filename, const char *mode);
}

using namespace Nyq;

FileRead :: FileRead()
  : fd_(0)
{
}

FileRead :: FileRead( std::string fileName, bool typeRaw )
  : fd_(0)
{
    open( fileName, typeRaw );
}

FileRead :: ~FileRead()
{
  if ( fd_ )
    fclose( fd_ );
}

void FileRead :: close( void )
{
  if ( fd_ ) fclose( fd_ );
  fd_ = 0;
  wavFile_ = false;
}

bool FileRead :: isOpen( void )
{
  if ( fd_ ) return true;
  else return false;
}

void FileRead :: open( std::string fileName, bool typeRaw )
{
  // If another file is open, close it.
  close();

  // Try to open the file.
  fd_ = NULL;
  if (ok_to_open(fileName.c_str(), "rb"))
    fd_ = fopen( fileName.c_str(), "rb" );
  if ( !fd_ ) {
    errorString_ << "FileRead::open: could not open or find file (" << fileName << ")!";
    handleError( StkError::FILE_NOT_FOUND );
  }

  // Attempt to determine file type from header (unless RAW).
  bool result = false;
  if ( typeRaw )
    result = getRawInfo( fileName.c_str() );
  else {
    char header[12];
    if ( fread( &header, 4, 3, fd_ ) != 3 ) goto error;
    if ( !strncmp( header, "RIFF", 4 ) &&
         !strncmp( &header[8], "WAVE", 4 ) )
      result = getWavInfo( fileName.c_str() );
    else if ( !strncmp( header, ".snd", 4 ) )
      result = getSndInfo( fileName.c_str() );
    else if ( !strncmp( header, "FORM", 4 ) &&
              ( !strncmp( &header[8], "AIFF", 4 ) || !strncmp(&header[8], "AIFC", 4) ) )
      result = getAifInfo( fileName.c_str() );
    else {
      if ( fseek( fd_, 126, SEEK_SET ) == -1 ) goto error;
      if ( fread( &header, 2, 1, fd_ ) != 1 ) goto error;
      if ( !strncmp( header, "MI", 2 ) ||
           !strncmp( header, "IM", 2 ) )
        result = getMatInfo( fileName.c_str() );
      else {
        errorString_ << "FileRead::open: file (" << fileName << ") format unknown.";
        handleError( StkError::FILE_UNKNOWN_FORMAT );
      }
    }
  }

  // If here, we had a file type candidate but something else went wrong.
  if ( result == false )
    handleError( StkError::FILE_ERROR );

  // Check for empty files.
  if ( fileSize_ == 0 ) {
    errorString_ << "FileRead::open: file (" << fileName << ") data size is zero!";
    handleError( StkError::FILE_ERROR );
  }

  return;

 error:
  errorString_ << "FileRead::open: error reading file (" << fileName << ")!";
  handleError( StkError::FILE_ERROR );
}

bool FileRead :: getRawInfo( const char *fileName )
{
  // Use the system call "stat" to determine the file length.
  struct stat filestat;
  if ( stat(fileName, &filestat) == -1 ) {
    errorString_ << "FileRead: Could not stat RAW file (" << fileName << ").";
    return false;
  }

  // STK rawwave files have no header and are assumed to contain a
  // monophonic stream of 16-bit signed integers in big-endian byte
  // order at a sample rate of 22050 Hz.
  channels_ = 1;
  fileSize_ = (long) filestat.st_size / 2;  // length in 2-byte samples
  dataOffset_ = 0;
  fileRate_ = 22050.0;
  dataType_ = STK_SINT16;
  byteswap_ = false;
#ifdef __LITTLE_ENDIAN__
  byteswap_ = true;
#endif

  return true;
}

bool FileRead :: getWavInfo( const char *fileName )
{
  // Find "format" chunk ... it must come before the "data" chunk.
  char id[4];
  SINT32 chunkSize;
  if ( fread(&id, 4, 1, fd_) != 1 ) goto error;
  while ( strncmp(id, "fmt ", 4) ) {
    if ( fread(&chunkSize, 4, 1, fd_) != 1 ) goto error;
#ifndef __LITTLE_ENDIAN__
    byteSwap32((unsigned char *)&chunkSize);
#endif
    if ( fseek(fd_, chunkSize, SEEK_CUR) == -1 ) goto error;
    if ( fread(&id, 4, 1, fd_) != 1 ) goto error;
  }

  // Check that the data is not compressed.
  unsigned short format_tag;
  if ( fread(&chunkSize, 4, 1, fd_) != 1 ) goto error; // Read fmt chunk size.
  if ( fread(&format_tag, 2, 1, fd_) != 1 ) goto error;
#ifndef __LITTLE_ENDIAN__
  byteSwap16((unsigned char *)&format_tag);
  byteSwap32((unsigned char *)&chunkSize);
#endif
  if ( format_tag == 0xFFFE ) { // WAVE_FORMAT_EXTENSIBLE
    dataOffset_ = ftell(fd_);
    if ( fseek(fd_, 14, SEEK_CUR) == -1 ) goto error;
    unsigned short extSize;
    if ( fread(&extSize, 2, 1, fd_) != 1 ) goto error;
#ifndef __LITTLE_ENDIAN__
    byteSwap16((unsigned char *)&extSize);
#endif
    if ( extSize == 0 ) goto error;
    if ( fseek(fd_, 6, SEEK_CUR) == -1 ) goto error;
    if ( fread(&format_tag, 2, 1, fd_) != 1 ) goto error;
#ifndef __LITTLE_ENDIAN__
    byteSwap16((unsigned char *)&format_tag);
#endif
    if ( fseek(fd_, dataOffset_, SEEK_SET) == -1 ) goto error;
  }
  if (format_tag != 1 && format_tag != 3 ) { // PCM = 1, FLOAT = 3
    errorString_ << "FileRead: "<< fileName << " contains an unsupported data format type (" << format_tag << ").";
    return false;
  }

  // Get number of channels from the header.
  SINT16 temp;
  if ( fread(&temp, 2, 1, fd_) != 1 ) goto error;
#ifndef __LITTLE_ENDIAN__
  byteSwap16((unsigned char *)&temp);
#endif
  channels_ = (unsigned int ) temp;

  // Get file sample rate from the header.
  SINT32 srate;
  if ( fread(&srate, 4, 1, fd_) != 1 ) goto error;
#ifndef __LITTLE_ENDIAN__
  byteSwap32((unsigned char *)&srate);
#endif
  fileRate_ = (StkFloat) srate;

  // Determine the data type.
  dataType_ = 0;
  if ( fseek(fd_, 6, SEEK_CUR) == -1 ) goto error;   // Locate bits_per_sample info.
  if ( fread(&temp, 2, 1, fd_) != 1 ) goto error;
#ifndef __LITTLE_ENDIAN__
  byteSwap16((unsigned char *)&temp);
#endif
  if ( format_tag == 1 ) {
    if (temp == 8)
      dataType_ = STK_SINT8;
    else if (temp == 16)
      dataType_ = STK_SINT16;
    else if (temp == 32)
      dataType_ = STK_SINT32;
  }
  else if ( format_tag == 3 ) {
    if (temp == 32)
      dataType_ = STK_FLOAT32;
    else if (temp == 64)
      dataType_ = STK_FLOAT64;
  }
  if ( dataType_ == 0 ) {
    errorString_ << "FileRead: " << temp << " bits per sample with data format " << format_tag << " are not supported (" << fileName << ").";
    return false;
  }

  // Jump over any remaining part of the "fmt" chunk.
  if ( fseek(fd_, chunkSize-16, SEEK_CUR) == -1 ) goto error;

  // Find "data" chunk ... it must come after the "fmt" chunk.
  if ( fread(&id, 4, 1, fd_) != 1 ) goto error;

  while ( strncmp(id, "data", 4) ) {
    if ( fread(&chunkSize, 4, 1, fd_) != 1 ) goto error;
#ifndef __LITTLE_ENDIAN__
    byteSwap32((unsigned char *)&chunkSize);
#endif
    chunkSize += chunkSize % 2; // chunk sizes must be even
    if ( fseek(fd_, chunkSize, SEEK_CUR) == -1 ) goto error;
    if ( fread(&id, 4, 1, fd_) != 1 ) goto error;
  }

  // Get length of data from the header.
  SINT32 bytes;
  if ( fread(&bytes, 4, 1, fd_) != 1 ) goto error;
#ifndef __LITTLE_ENDIAN__
  byteSwap32((unsigned char *)&bytes);
#endif
  fileSize_ = 8 * bytes / temp / channels_;  // sample frames

  dataOffset_ = ftell(fd_);
  byteswap_ = false;
#ifndef __LITTLE_ENDIAN__
  byteswap_ = true;
#endif

  wavFile_ = true;
  return true;

 error:
  errorString_ << "FileRead: error reading WAV file (" << fileName << ").";
  return false;
}

bool FileRead :: getSndInfo( const char *fileName )
{
  // Determine the data type.
  UINT32 format;
  if ( fseek(fd_, 12, SEEK_SET) == -1 ) goto error;   // Locate format
  if ( fread(&format, 4, 1, fd_) != 1 ) goto error;
#ifdef __LITTLE_ENDIAN__
    byteSwap32((unsigned char *)&format);
#endif
  if (format == 2) dataType_ = STK_SINT8;
  else if (format == 3) dataType_ = STK_SINT16;
  else if (format == 4) dataType_ = STK_SINT24;
  else if (format == 5) dataType_ = STK_SINT32;
  else if (format == 6) dataType_ = STK_FLOAT32;
  else if (format == 7) dataType_ = STK_FLOAT64;
  else {
    errorString_ << "FileRead: data format in file " << fileName << " is not supported.";
    return false;
  }

  // Get file sample rate from the header.
  UINT32 srate;
  if ( fread(&srate, 4, 1, fd_) != 1 ) goto error;
#ifdef __LITTLE_ENDIAN__
  byteSwap32((unsigned char *)&srate);
#endif
  fileRate_ = (StkFloat) srate;

  // Get number of channels from the header.
  UINT32 chans;
  if ( fread(&chans, 4, 1, fd_) != 1 ) goto error;
#ifdef __LITTLE_ENDIAN__
  byteSwap32((unsigned char *)&chans);
#endif
  channels_ = chans;

  if ( fseek(fd_, 4, SEEK_SET) == -1 ) goto error;
  if ( fread(&dataOffset_, 4, 1, fd_) != 1 ) goto error;
#ifdef __LITTLE_ENDIAN__
  byteSwap32((unsigned char *)&dataOffset_);
#endif

  // Get length of data from the header.
  if ( fread(&fileSize_, 4, 1, fd_) != 1 ) goto error;
#ifdef __LITTLE_ENDIAN__
  byteSwap32((unsigned char *)&fileSize_);
#endif
  // Convert to sample frames.
  if ( dataType_ == STK_SINT8 )
    fileSize_ /= channels_;
  if ( dataType_ == STK_SINT16 )
    fileSize_ /= 2 * channels_;
  else if ( dataType_ == STK_SINT24 )
    fileSize_ /= 3 * channels_;
  else if ( dataType_ == STK_SINT32 || dataType_ == STK_FLOAT32 )
    fileSize_ /= 4 * channels_;
  else if ( dataType_ == STK_FLOAT64 )
    fileSize_ /= 8 * channels_;

  byteswap_ = false;
#ifdef __LITTLE_ENDIAN__
  byteswap_ = true;
#endif

  return true;

 error:
  errorString_ << "FileRead: Error reading SND file (" << fileName << ").";
  return false;
}

bool FileRead :: getAifInfo( const char *fileName )
{
  bool aifc = false;
  char id[4];

  // Determine whether this is AIFF or AIFC.
  if ( fseek(fd_, 8, SEEK_SET) == -1 ) goto error;
  if ( fread(&id, 4, 1, fd_) != 1 ) goto error;
  if ( !strncmp(id, "AIFC", 4) ) aifc = true;

  // Find "common" chunk
  SINT32 chunkSize;
  if ( fread(&id, 4, 1, fd_) != 1) goto error;
  while ( strncmp(id, "COMM", 4) ) {
    if ( fread(&chunkSize, 4, 1, fd_) != 1 ) goto error;
#ifdef __LITTLE_ENDIAN__
    byteSwap32((unsigned char *)&chunkSize);
#endif
    chunkSize += chunkSize % 2; // chunk sizes must be even
    if ( fseek(fd_, chunkSize, SEEK_CUR) == -1 ) goto error;
    if ( fread(&id, 4, 1, fd_) != 1 ) goto error;
  }

  // Get number of channels from the header.
  SINT16 temp;
  if ( fseek(fd_, 4, SEEK_CUR) == -1 ) goto error; // Jump over chunk size
  if ( fread(&temp, 2, 1, fd_) != 1 ) goto error;
#ifdef __LITTLE_ENDIAN__
  byteSwap16((unsigned char *)&temp);
#endif
  channels_ = temp;

  // Get length of data from the header.
  SINT32 frames;
  if ( fread(&frames, 4, 1, fd_) != 1 ) goto error;
#ifdef __LITTLE_ENDIAN__
  byteSwap32((unsigned char *)&frames);
#endif
  fileSize_ = frames; // sample frames

  // Read the number of bits per sample.
  if ( fread(&temp, 2, 1, fd_) != 1 ) goto error;
#ifdef __LITTLE_ENDIAN__
  byteSwap16((unsigned char *)&temp);
#endif

  // Get file sample rate from the header.  For AIFF files, this value
  // is stored in a 10-byte, IEEE Standard 754 floating point number,
  // so we need to convert it first.
  unsigned char srate[10];
  unsigned char exp;
  unsigned long mantissa;
  unsigned long last;
  if ( fread(&srate, 10, 1, fd_) != 1 ) goto error;
  mantissa = (unsigned long) *(unsigned long *)(srate+2);
#ifdef __LITTLE_ENDIAN__
  byteSwap32((unsigned char *)&mantissa);
#endif
  exp = 30 - *(srate+1);
  last = 0;
  while (exp--) {
    last = mantissa;
    mantissa >>= 1;
  }
  if (last & 0x00000001) mantissa++;
  fileRate_ = (StkFloat) mantissa;

  // Determine the data format.
  dataType_ = 0;
  if ( aifc == false ) {
    if ( temp <= 8 ) dataType_ = STK_SINT8;
    else if ( temp <= 16 ) dataType_ = STK_SINT16;
    else if ( temp <= 24 ) dataType_ = STK_SINT24;
    else if ( temp <= 32 ) dataType_ = STK_SINT32;
  }
  else {
    if ( fread(&id, 4, 1, fd_) != 1 ) goto error;
    if ( !strncmp(id, "NONE", 4) ) {
      if ( temp <= 8 ) dataType_ = STK_SINT8;
      else if ( temp <= 16 ) dataType_ = STK_SINT16;
      else if ( temp <= 24 ) dataType_ = STK_SINT24;
      else if ( temp <= 32 ) dataType_ = STK_SINT32;
    }
    else if ( (!strncmp(id, "fl32", 4) || !strncmp(id, "FL32", 4)) && temp == 32 ) dataType_ = STK_FLOAT32;
    else if ( (!strncmp(id, "fl64", 4) || !strncmp(id, "FL64", 4)) && temp == 64 ) dataType_ = STK_FLOAT64;
  }
  if ( dataType_ == 0 ) {
    errorString_ << "FileRead: AIFF/AIFC file (" << fileName << ") has unsupported data type (" << id << ").";
    return false;
  }

  // Start at top to find data (SSND) chunk ... chunk order is undefined.
  if ( fseek(fd_, 12, SEEK_SET) == -1 ) goto error;

  // Find data (SSND) chunk
  if ( fread(&id, 4, 1, fd_) != 1 ) goto error;
  while ( strncmp(id, "SSND", 4) ) {
    if ( fread(&chunkSize, 4, 1, fd_) != 1 ) goto error;
#ifdef __LITTLE_ENDIAN__
    byteSwap32((unsigned char *)&chunkSize);
#endif
    chunkSize += chunkSize % 2; // chunk sizes must be even
    if ( fseek(fd_, chunkSize, SEEK_CUR) == -1 ) goto error;
    if ( fread(&id, 4, 1, fd_) != 1 ) goto error;
  }

  // Skip over chunk size, offset, and blocksize fields
  if ( fseek(fd_, 12, SEEK_CUR) == -1 ) goto error;

  dataOffset_ = ftell(fd_);
  byteswap_ = false;
#ifdef __LITTLE_ENDIAN__
  byteswap_ = true;
#endif

  return true;

 error:
  errorString_ << "FileRead: Error reading AIFF file (" << fileName << ").";
  return false;
}

bool FileRead :: getMatInfo( const char *fileName )
{
  // MAT-file formatting information is available at:
  // http://www.mathworks.com/access/helpdesk/help/pdf_doc/matlab/matfile_format.pdf

  // Verify this is a version 5 MAT-file format.
  char head[4];
  if ( fseek(fd_, 0, SEEK_SET) == -1 ) goto error;
  if ( fread(&head, 4, 1, fd_) != 1 ) goto error;
  // If any of the first 4 characters of the header = 0, then this is
  // a Version 4 MAT-file.
  if ( strstr(head, "0") ) {
    errorString_ << "FileRead: " << fileName << " appears to be a Version 4 MAT-file, which is not currently supported.";
    return false;
  }

  // Determine the endian-ness of the file.
  char mi[2];
  byteswap_ = false;
  // Locate "M" and "I" characters in header.
  if ( fseek(fd_, 126, SEEK_SET) == -1 ) goto error;
  if ( fread(&mi, 2, 1, fd_) != 1) goto error;
#ifdef __LITTLE_ENDIAN__
  if ( !strncmp(mi, "MI", 2) )
    byteswap_ = true;
  else if ( strncmp(mi, "IM", 2) ) goto error;
#else
  if ( !strncmp(mi, "IM", 2))
    byteswap_ = true;
  else if ( strncmp(mi, "MI", 2) ) goto error;
#endif

  // Check the data element type
  SINT32 datatype;
  if ( fread(&datatype, 4, 1, fd_) != 1 ) goto error;
  if ( byteswap_ ) byteSwap32((unsigned char *)&datatype);
  if (datatype != 14) {
    errorString_ << "FileRead: The file does not contain a single Matlab array (or matrix) data element.";
    return false;
  }

  // Determine the array data type.
  SINT32 tmp;
  SINT32 size;
  if ( fseek(fd_, 168, SEEK_SET) == -1 ) goto error;
  if ( fread(&tmp, 4, 1, fd_) != 1 ) goto error;
  if (byteswap_) byteSwap32((unsigned char *)&tmp);
  if (tmp == 1) {  // array name > 4 characters
    if ( fread(&tmp, 4, 1, fd_) != 1 ) goto error;  // get array name length
    if (byteswap_) byteSwap32((unsigned char *)&tmp);
    size = (SINT32) ceil((float)tmp / 8);
    if ( fseek(fd_, size*8, SEEK_CUR) == -1 ) goto error;  // jump over array name
  }
  else { // array name <= 4 characters, compressed data element
    if ( fseek(fd_, 4, SEEK_CUR) == -1 ) goto error;
  }
  if ( fread(&tmp, 4, 1, fd_) != 1 ) goto error;
  if (byteswap_) byteSwap32((unsigned char *)&tmp);
  if ( tmp == 1 ) dataType_ = STK_SINT8;
  else if ( tmp == 3 ) dataType_ = STK_SINT16;
  else if ( tmp == 5 ) dataType_ = STK_SINT32;
  else if ( tmp == 7 ) dataType_ = STK_FLOAT32;
  else if ( tmp == 9 ) dataType_ = STK_FLOAT64;
  else {
    errorString_ << "FileRead: The MAT-file array data format (" << tmp << ") is not supported.";
    return false;
  }

  // Get number of rows from the header.
  SINT32 rows;
  if ( fseek(fd_, 160, SEEK_SET) == -1 ) goto error;
  if ( fread(&rows, 4, 1, fd_) != 1 ) goto error;
  if (byteswap_) byteSwap32((unsigned char *)&rows);

  // Get number of columns from the header.
  SINT32 columns;
  if ( fread(&columns, 4, 1, fd_) != 1 ) goto error;
  if (byteswap_) byteSwap32((unsigned char *)&columns);

  // Assume channels = smaller of rows or columns.
  if (rows < columns) {
    channels_ = rows;
    fileSize_ = columns;
  }
  else {
    errorString_ << "FileRead: Transpose the MAT-file array so that audio channels fill matrix rows (not columns).";
    return false;
  }

  // Move read pointer to the data in the file.
  SINT32 headsize;
  if ( fseek(fd_, 132, SEEK_SET) == -1 ) goto error;
  if ( fread(&headsize, 4, 1, fd_) != 1 ) goto error; // file size from 132nd byte
  if (byteswap_) byteSwap32((unsigned char *)&headsize);
  headsize -= fileSize_ * 8 * channels_;
  if ( fseek(fd_, headsize, SEEK_CUR) == -1 ) goto error;
  dataOffset_ = ftell(fd_);

  // Assume MAT-files have 44100 Hz sample rate.
  fileRate_ = 44100.0;

  return true;

 error:
  errorString_ << "FileRead: Error reading MAT-file (" << fileName << ").";
  return false;
}

void FileRead :: read( StkFrames& buffer, unsigned long startFrame, bool doNormalize )
{
  // Make sure we have an open file.
  if ( fd_ == 0 ) {
    errorString_ << "FileRead::read: a file is not open!";
    Stk::handleError( StkError::WARNING );
    return;
  }

  // Check the buffer size.
  unsigned int nFrames = buffer.frames();
  if ( nFrames == 0 ) {
    errorString_ << "FileRead::read: StkFrames buffer size is zero ... no data read!";
    Stk::handleError( StkError::WARNING );
    return;
  }

  if ( buffer.channels() != channels_ ) {
    errorString_ << "FileRead::read: StkFrames argument has incompatible number of channels!";
    Stk::handleError( StkError::FUNCTION_ARGUMENT );
  }

  // Check for file end.
  if ( startFrame + nFrames >= fileSize_ )
    nFrames = fileSize_ - startFrame;

  long i, nSamples = (long) ( nFrames * channels_ );
  unsigned long offset = startFrame * channels_;

  // Read samples into StkFrames data buffer.
  if ( dataType_ == STK_SINT16 ) {
    SINT16 *buf = (SINT16 *) &buffer[0];
    if ( fseek( fd_, dataOffset_+(offset*2), SEEK_SET ) == -1 ) goto error;
    if ( fread( buf, nSamples * 2, 1, fd_ ) != 1 ) goto error;
    if ( byteswap_ ) {
      SINT16 *ptr = buf;
      for ( i=nSamples-1; i>=0; i-- )
        byteSwap16( (unsigned char *) ptr++ );
    }
    if ( doNormalize ) {
      StkFloat gain = 1.0 / 32768.0;
      for ( i=nSamples-1; i>=0; i-- )
        buffer[i] = buf[i] * gain;
    }
    else {
      for ( i=nSamples-1; i>=0; i-- )
        buffer[i] = buf[i];
    }
  }
  else if ( dataType_ == STK_SINT32 ) {
    SINT32 *buf = (SINT32 *) &buffer[0];
    if ( fseek( fd_, dataOffset_+(offset*4 ), SEEK_SET ) == -1 ) goto error;
    if ( fread( buf, nSamples * 4, 1, fd_ ) != 1 ) goto error;
    if ( byteswap_ ) {
      SINT32 *ptr = buf;
      for ( i=nSamples-1; i>=0; i-- )
        byteSwap32( (unsigned char *) ptr++ );
    }
    if ( doNormalize ) {
      StkFloat gain = 1.0 / 2147483648.0;
      for ( i=nSamples-1; i>=0; i-- )
        buffer[i] = buf[i] * gain;
    }
    else {
      for ( i=nSamples-1; i>=0; i-- )
        buffer[i] = buf[i];
    }
  }
  else if ( dataType_ == STK_FLOAT32 ) {
    FLOAT32 *buf = (FLOAT32 *) &buffer[0];
    if ( fseek( fd_, dataOffset_+(offset*4), SEEK_SET ) == -1 ) goto error;
    if ( fread( buf, nSamples * 4, 1, fd_ ) != 1 ) goto error;
    if ( byteswap_ ) {
      FLOAT32 *ptr = buf;
      for ( i=nSamples-1; i>=0; i-- )
        byteSwap32( (unsigned char *) ptr++ );
    }
    for ( i=nSamples-1; i>=0; i-- )
      buffer[i] = buf[i];
  }
  else if ( dataType_ == STK_FLOAT64 ) {
    FLOAT64 *buf = (FLOAT64 *) &buffer[0];
    if ( fseek( fd_, dataOffset_+(offset*8), SEEK_SET ) == -1 ) goto error;
    if ( fread( buf, nSamples * 8, 1, fd_ ) != 1 ) goto error;
    if ( byteswap_ ) {
      FLOAT64 *ptr = buf;
      for ( i=nSamples-1; i>=0; i-- )
        byteSwap64( (unsigned char *) ptr++ );
    }
    for ( i=nSamples-1; i>=0; i-- )
      buffer[i] = buf[i];
  }
  else if ( dataType_ == STK_SINT8 && wavFile_ ) { // 8-bit WAV data is unsigned!
    unsigned char *buf = (unsigned char *) &buffer[0];
    if ( fseek( fd_, dataOffset_+offset, SEEK_SET ) == -1 ) goto error;
    if ( fread( buf, nSamples, 1, fd_) != 1 ) goto error;
    if ( doNormalize ) {
      StkFloat gain = 1.0 / 128.0;
      for ( i=nSamples-1; i>=0; i-- )
        buffer[i] = ( buf[i] - 128 ) * gain;
    }
    else {
      for ( i=nSamples-1; i>=0; i-- )
        buffer[i] = buf[i] - 128.0;
    }
  }
  else if ( dataType_ == STK_SINT8 ) { // signed 8-bit data
    char *buf = (char *) &buffer[0];
    if ( fseek( fd_, dataOffset_+offset, SEEK_SET ) == -1 ) goto error;
    if ( fread( buf, nSamples, 1, fd_ ) != 1 ) goto error;
    if ( doNormalize ) {
      StkFloat gain = 1.0 / 128.0;
      for ( i=nSamples-1; i>=0; i-- )
        buffer[i] = buf[i] * gain;
    }
    else {
      for ( i=nSamples-1; i>=0; i-- )
        buffer[i] = buf[i];
    }
  }
  else if ( dataType_ == STK_SINT24 ) {
    // 24-bit values are harder to import efficiently since there is
    // no native 24-bit type.  The following routine works but is much
    // less efficient that that used for the other data types.
    SINT32 buf;
    StkFloat gain = 1.0 / 8388608.0;
    if ( fseek(fd_, dataOffset_+(offset*3), SEEK_SET ) == -1 ) goto error;
    for ( i=0; i<nSamples; i++ ) {
      if ( fread( &buf, 3, 1, fd_ ) != 1 ) goto error;
      buf >>= 8;
      if ( byteswap_ )
        byteSwap32( (unsigned char *) &buf );
      if ( doNormalize )
        buffer[i] = buf * gain;
      else
        buffer[i] = buf;
    }
  }

  buffer.setDataRate( fileRate_ );

  return;

 error:
  errorString_ << "FileRead: Error reading file data.";
  handleError( StkError::FILE_ERROR);
}

