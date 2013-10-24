/***************************************************************************
    copyright            : (C) 2002 - 2008 by Scott Wheeler
    email                : wheeler@kde.org
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License version   *
 *   2.1 as published by the Free Software Foundation.                     *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful, but   *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *
 *   02110-1301  USA                                                       *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#include "tfile.h"
#include "tfilestream.h"
#include "tstring.h"
#include "tdebug.h"
#include "tpropertymap.h"

#ifdef _WIN32
# include <windows.h>
# include <io.h>
#else
# include <stdio.h>
# include <unistd.h>
#endif

#ifndef R_OK
# define R_OK 4
#endif
#ifndef W_OK
# define W_OK 2
#endif

#include "asffile.h"
#include "mpegfile.h"
#include "vorbisfile.h"
#include "flacfile.h"
#include "oggflacfile.h"
#include "mpcfile.h"
#include "mp4file.h"
#include "wavpackfile.h"
#include "speexfile.h"
#include "opusfile.h"
#include "trueaudiofile.h"
#include "aifffile.h"
#include "wavfile.h"
#include "apefile.h"
#include "modfile.h"
#include "s3mfile.h"
#include "itfile.h"
#include "xmfile.h"
#include "mp4file.h"

using namespace TagLib;

namespace
{
#ifdef _WIN32
  const TagLib::uint BufferSize = 8192;
#else
  const TagLib::uint BufferSize = 1024;
#endif
}

class File::FilePrivate
{
public:
  FilePrivate(IOStream *stream, bool owner);

  IOStream *stream;
  bool streamOwner;
  bool valid;
};

File::FilePrivate::FilePrivate(IOStream *stream, bool owner) :
  stream(stream),
  streamOwner(owner),
  valid(true)
{
}

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

File::File(FileName fileName)
{
  IOStream *stream = new FileStream(fileName);
  d = new FilePrivate(stream, true);
}

File::File(IOStream *stream)
{
  d = new FilePrivate(stream, false);
}

File::~File()
{
  if(d->stream && d->streamOwner)
    delete d->stream;
  delete d;
}

FileName File::name() const
{
  return d->stream->name();
}

PropertyMap File::properties() const
{
  // ugly workaround until this method is virtual
  if(dynamic_cast<const APE::File* >(this))
    return dynamic_cast<const APE::File* >(this)->properties();
  if(dynamic_cast<const FLAC::File* >(this))
    return dynamic_cast<const FLAC::File* >(this)->properties();
  if(dynamic_cast<const IT::File* >(this))
    return dynamic_cast<const IT::File* >(this)->properties();
  if(dynamic_cast<const Mod::File* >(this))
    return dynamic_cast<const Mod::File* >(this)->properties();
  if(dynamic_cast<const MPC::File* >(this))
    return dynamic_cast<const MPC::File* >(this)->properties();
  if(dynamic_cast<const MPEG::File* >(this))
    return dynamic_cast<const MPEG::File* >(this)->properties();
  if(dynamic_cast<const Ogg::FLAC::File* >(this))
    return dynamic_cast<const Ogg::FLAC::File* >(this)->properties();
  if(dynamic_cast<const Ogg::Speex::File* >(this))
    return dynamic_cast<const Ogg::Speex::File* >(this)->properties();
  if(dynamic_cast<const Ogg::Opus::File* >(this))
    return dynamic_cast<const Ogg::Opus::File* >(this)->properties();
  if(dynamic_cast<const Ogg::Vorbis::File* >(this))
    return dynamic_cast<const Ogg::Vorbis::File* >(this)->properties();
  if(dynamic_cast<const RIFF::AIFF::File* >(this))
    return dynamic_cast<const RIFF::AIFF::File* >(this)->properties();
  if(dynamic_cast<const RIFF::WAV::File* >(this))
    return dynamic_cast<const RIFF::WAV::File* >(this)->properties();
  if(dynamic_cast<const S3M::File* >(this))
    return dynamic_cast<const S3M::File* >(this)->properties();
  if(dynamic_cast<const TrueAudio::File* >(this))
    return dynamic_cast<const TrueAudio::File* >(this)->properties();
  if(dynamic_cast<const WavPack::File* >(this))
    return dynamic_cast<const WavPack::File* >(this)->properties();
  if(dynamic_cast<const XM::File* >(this))
    return dynamic_cast<const XM::File* >(this)->properties();
  if(dynamic_cast<const MP4::File* >(this))
    return dynamic_cast<const MP4::File* >(this)->properties();
  if(dynamic_cast<const ASF::File* >(this))
    return dynamic_cast<const ASF::File* >(this)->properties();
  return tag()->properties();
}

void File::removeUnsupportedProperties(const StringList &properties)
{
  // here we only consider those formats that could possibly contain
  // unsupported properties
  if(dynamic_cast<APE::File* >(this))
    dynamic_cast<APE::File* >(this)->removeUnsupportedProperties(properties);
  else if(dynamic_cast<FLAC::File* >(this))
    dynamic_cast<FLAC::File* >(this)->removeUnsupportedProperties(properties);
  else if(dynamic_cast<MPC::File* >(this))
    dynamic_cast<MPC::File* >(this)->removeUnsupportedProperties(properties);
  else if(dynamic_cast<MPEG::File* >(this))
    dynamic_cast<MPEG::File* >(this)->removeUnsupportedProperties(properties);
  else if(dynamic_cast<Ogg::Vorbis::File* >(this))
    dynamic_cast<Ogg::Vorbis::File* >(this)->removeUnsupportedProperties(properties);
  else if(dynamic_cast<RIFF::AIFF::File* >(this))
    dynamic_cast<RIFF::AIFF::File* >(this)->removeUnsupportedProperties(properties);
  else if(dynamic_cast<RIFF::WAV::File* >(this))
    dynamic_cast<RIFF::WAV::File* >(this)->removeUnsupportedProperties(properties);
  else if(dynamic_cast<TrueAudio::File* >(this))
    dynamic_cast<TrueAudio::File* >(this)->removeUnsupportedProperties(properties);
  else if(dynamic_cast<WavPack::File* >(this))
    dynamic_cast<WavPack::File* >(this)->removeUnsupportedProperties(properties);
  else if(dynamic_cast<MP4::File* >(this))
    dynamic_cast<MP4::File* >(this)->removeUnsupportedProperties(properties);
  else if(dynamic_cast<ASF::File* >(this))
    dynamic_cast<ASF::File* >(this)->removeUnsupportedProperties(properties);
  else
    tag()->removeUnsupportedProperties(properties);
}

PropertyMap File::setProperties(const PropertyMap &properties)
{
  if(dynamic_cast<APE::File* >(this))
    return dynamic_cast<APE::File* >(this)->setProperties(properties);
  else if(dynamic_cast<FLAC::File* >(this))
    return dynamic_cast<FLAC::File* >(this)->setProperties(properties);
  else if(dynamic_cast<IT::File* >(this))
    return dynamic_cast<IT::File* >(this)->setProperties(properties);
  else if(dynamic_cast<Mod::File* >(this))
    return dynamic_cast<Mod::File* >(this)->setProperties(properties);
  else if(dynamic_cast<MPC::File* >(this))
    return dynamic_cast<MPC::File* >(this)->setProperties(properties);
  else if(dynamic_cast<MPEG::File* >(this))
    return dynamic_cast<MPEG::File* >(this)->setProperties(properties);
  else if(dynamic_cast<Ogg::FLAC::File* >(this))
    return dynamic_cast<Ogg::FLAC::File* >(this)->setProperties(properties);
  else if(dynamic_cast<Ogg::Speex::File* >(this))
    return dynamic_cast<Ogg::Speex::File* >(this)->setProperties(properties);
  else if(dynamic_cast<Ogg::Opus::File* >(this))
    return dynamic_cast<Ogg::Opus::File* >(this)->setProperties(properties);
  else if(dynamic_cast<Ogg::Vorbis::File* >(this))
    return dynamic_cast<Ogg::Vorbis::File* >(this)->setProperties(properties);
  else if(dynamic_cast<RIFF::AIFF::File* >(this))
    return dynamic_cast<RIFF::AIFF::File* >(this)->setProperties(properties);
  else if(dynamic_cast<RIFF::WAV::File* >(this))
    return dynamic_cast<RIFF::WAV::File* >(this)->setProperties(properties);
  else if(dynamic_cast<S3M::File* >(this))
    return dynamic_cast<S3M::File* >(this)->setProperties(properties);
  else if(dynamic_cast<TrueAudio::File* >(this))
    return dynamic_cast<TrueAudio::File* >(this)->setProperties(properties);
  else if(dynamic_cast<WavPack::File* >(this))
    return dynamic_cast<WavPack::File* >(this)->setProperties(properties);
  else if(dynamic_cast<XM::File* >(this))
    return dynamic_cast<XM::File* >(this)->setProperties(properties);
  else if(dynamic_cast<MP4::File* >(this))
    return dynamic_cast<MP4::File* >(this)->setProperties(properties);
  else if(dynamic_cast<ASF::File* >(this))
    return dynamic_cast<ASF::File* >(this)->setProperties(properties);
  else
    return tag()->setProperties(properties);
}

ByteVector File::readBlock(ulong length)
{
  return d->stream->readBlock(length);
}

void File::writeBlock(const ByteVector &data)
{
  d->stream->writeBlock(data);
}

long File::find(const ByteVector &pattern, long fromOffset, const ByteVector &before)
{
  if(!d->stream || pattern.size() > bufferSize())
      return -1;

  // The position in the file that the current buffer starts at.

  long bufferOffset = fromOffset;
  ByteVector buffer;

  // These variables are used to keep track of a partial match that happens at
  // the end of a buffer.

  int previousPartialMatch = -1;
  int beforePreviousPartialMatch = -1;

  // Save the location of the current read pointer.  We will restore the
  // position using seek() before all returns.

  long originalPosition = tell();

  // Start the search at the offset.

  seek(fromOffset);

  // This loop is the crux of the find method.  There are three cases that we
  // want to account for:
  //
  // (1) The previously searched buffer contained a partial match of the search
  // pattern and we want to see if the next one starts with the remainder of
  // that pattern.
  //
  // (2) The search pattern is wholly contained within the current buffer.
  //
  // (3) The current buffer ends with a partial match of the pattern.  We will
  // note this for use in the next itteration, where we will check for the rest
  // of the pattern.
  //
  // All three of these are done in two steps.  First we check for the pattern
  // and do things appropriately if a match (or partial match) is found.  We
  // then check for "before".  The order is important because it gives priority
  // to "real" matches.

  for(buffer = readBlock(bufferSize()); buffer.size() > 0; buffer = readBlock(bufferSize())) {

    // (1) previous partial match

    if(previousPartialMatch >= 0 && int(bufferSize()) > previousPartialMatch) {
      const int patternOffset = (bufferSize() - previousPartialMatch);
      if(buffer.containsAt(pattern, 0, patternOffset)) {
        seek(originalPosition);
        return bufferOffset - bufferSize() + previousPartialMatch;
      }
    }

    if(!before.isNull() && beforePreviousPartialMatch >= 0 && int(bufferSize()) > beforePreviousPartialMatch) {
      const int beforeOffset = (bufferSize() - beforePreviousPartialMatch);
      if(buffer.containsAt(before, 0, beforeOffset)) {
        seek(originalPosition);
        return -1;
      }
    }

    // (2) pattern contained in current buffer

    long location = buffer.find(pattern);
    if(location >= 0) {
      seek(originalPosition);
      return bufferOffset + location;
    }

    if(!before.isNull() && buffer.find(before) >= 0) {
      seek(originalPosition);
      return -1;
    }

    // (3) partial match

    previousPartialMatch = buffer.endsWithPartialMatch(pattern);

    if(!before.isNull())
      beforePreviousPartialMatch = buffer.endsWithPartialMatch(before);

    bufferOffset += bufferSize();
  }

  // Since we hit the end of the file, reset the status before continuing.

  clear();

  seek(originalPosition);

  return -1;
}


long File::rfind(const ByteVector &pattern, long fromOffset, const ByteVector &before)
{
  if(!d->stream || pattern.size() > bufferSize())
      return -1;

  // The position in the file that the current buffer starts at.

  ByteVector buffer;

  // These variables are used to keep track of a partial match that happens at
  // the end of a buffer.

  /*
  int previousPartialMatch = -1;
  int beforePreviousPartialMatch = -1;
  */

  // Save the location of the current read pointer.  We will restore the
  // position using seek() before all returns.

  long originalPosition = tell();

  // Start the search at the offset.

  long bufferOffset;
  if(fromOffset == 0) {
    seek(-1 * int(bufferSize()), End);
    bufferOffset = tell();
  }
  else {
    seek(fromOffset + -1 * int(bufferSize()), Beginning);
    bufferOffset = tell();
  }

  // See the notes in find() for an explanation of this algorithm.

  for(buffer = readBlock(bufferSize()); buffer.size() > 0; buffer = readBlock(bufferSize())) {

    // TODO: (1) previous partial match

    // (2) pattern contained in current buffer

    long location = buffer.rfind(pattern);
    if(location >= 0) {
      seek(originalPosition);
      return bufferOffset + location;
    }

    if(!before.isNull() && buffer.find(before) >= 0) {
      seek(originalPosition);
      return -1;
    }

    // TODO: (3) partial match

    bufferOffset -= bufferSize();
    seek(bufferOffset);
  }

  // Since we hit the end of the file, reset the status before continuing.

  clear();

  seek(originalPosition);

  return -1;
}

void File::insert(const ByteVector &data, ulong start, ulong replace)
{
  d->stream->insert(data, start, replace);
}

void File::removeBlock(ulong start, ulong length)
{
  d->stream->removeBlock(start, length);
}

bool File::readOnly() const
{
  return d->stream->readOnly();
}

bool File::isOpen() const
{
  return d->stream->isOpen();
}

bool File::isValid() const
{
  return isOpen() && d->valid;
}

void File::seek(long offset, Position p)
{
  d->stream->seek(offset, IOStream::Position(p));
}

void File::truncate(long length)
{
  d->stream->truncate(length);
}

void File::clear()
{
  d->stream->clear();
}

long File::tell() const
{
  return d->stream->tell();
}

long File::length()
{
  return d->stream->length();
}

bool File::isReadable(const char *file)
{

#if defined(_MSC_VER) && (_MSC_VER >= 1400)  // VC++2005 or later

  return _access_s(file, R_OK) == 0;

#else

  return access(file, R_OK) == 0;

#endif

}

bool File::isWritable(const char *file)
{

#if defined(_MSC_VER) && (_MSC_VER >= 1400)  // VC++2005 or later

  return _access_s(file, W_OK) == 0;

#else

  return access(file, W_OK) == 0;

#endif

}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

TagLib::uint File::bufferSize()
{
  return BufferSize;
}

void File::setValid(bool valid)
{
  d->valid = valid;
}

