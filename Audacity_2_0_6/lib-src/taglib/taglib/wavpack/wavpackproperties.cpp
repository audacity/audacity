/***************************************************************************
    copyright            : (C) 2006 by Lukáš Lalinský
    email                : lalinsky@gmail.com

    copyright            : (C) 2004 by Allan Sandfeld Jensen
    email                : kde@carewolf.org
                           (original MPC implementation)
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

#include <tstring.h>
#include <tdebug.h>

#include "wavpackproperties.h"
#include "wavpackfile.h"

using namespace TagLib;

class WavPack::Properties::PropertiesPrivate
{
public:
  PropertiesPrivate(const ByteVector &d, long length, ReadStyle s) :
    data(d),
    streamLength(length),
    style(s),
    length(0),
    bitrate(0),
    sampleRate(0),
    channels(0),
    version(0),
    bitsPerSample(0),
    sampleFrames(0),
    file(0) {}

  ByteVector data;
  long streamLength;
  ReadStyle style;
  int length;
  int bitrate;
  int sampleRate;
  int channels;
  int version;
  int bitsPerSample;
  uint sampleFrames;
  File *file;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

WavPack::Properties::Properties(const ByteVector &data, long streamLength, ReadStyle style) : AudioProperties(style)
{
  d = new PropertiesPrivate(data, streamLength, style);
  read();
}

WavPack::Properties::Properties(File *file, long streamLength, ReadStyle style) : AudioProperties(style)
{
  ByteVector data = file->readBlock(32);
  d = new PropertiesPrivate(data, streamLength, style);
  d->file = file;
  read();
}

WavPack::Properties::~Properties()
{
  delete d;
}

int WavPack::Properties::length() const
{
  return d->length;
}

int WavPack::Properties::bitrate() const
{
  return d->bitrate;
}

int WavPack::Properties::sampleRate() const
{
  return d->sampleRate;
}

int WavPack::Properties::channels() const
{
  return d->channels;
}

int WavPack::Properties::version() const
{
  return d->version;
}

int WavPack::Properties::bitsPerSample() const
{
  return d->bitsPerSample;
}

TagLib::uint WavPack::Properties::sampleFrames() const
{
  return d->sampleFrames;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

static const unsigned int sample_rates[] = { 
   6000,  8000,  9600, 11025, 12000, 16000,  22050, 24000, 
  32000, 44100, 48000, 64000, 88200, 96000, 192000,     0 };

#define BYTES_STORED    3
#define MONO_FLAG       4

#define SHIFT_LSB       13
#define SHIFT_MASK      (0x1fL << SHIFT_LSB)

#define SRATE_LSB       23
#define SRATE_MASK      (0xfL << SRATE_LSB)

#define MIN_STREAM_VERS     0x402
#define MAX_STREAM_VERS     0x410

#define FINAL_BLOCK     0x1000

void WavPack::Properties::read()
{
  if(!d->data.startsWith("wvpk"))
    return;

  d->version = d->data.toShort(8, false);
  if(d->version < MIN_STREAM_VERS || d->version > MAX_STREAM_VERS)
    return;

  const unsigned int flags = d->data.toUInt(24, false);
  d->bitsPerSample = ((flags & BYTES_STORED) + 1) * 8 -
    ((flags & SHIFT_MASK) >> SHIFT_LSB);
  d->sampleRate = sample_rates[(flags & SRATE_MASK) >> SRATE_LSB];
  d->channels = (flags & MONO_FLAG) ? 1 : 2;

  unsigned int samples = d->data.toUInt(12, false);
  if(samples == ~0u) {
    if(d->file && d->style != Fast) {
      samples = seekFinalIndex();
    }
    else {
      samples = 0;
    }
  }
  d->length = d->sampleRate > 0 ? (samples + (d->sampleRate / 2)) / d->sampleRate : 0;
  d->sampleFrames = samples;

  d->bitrate = d->length > 0 ? ((d->streamLength * 8L) / d->length) / 1000 : 0;
}

unsigned int WavPack::Properties::seekFinalIndex()
{
  ByteVector blockID("wvpk", 4);

  long offset = d->streamLength;
  while(offset > 0) {
    offset = d->file->rfind(blockID, offset);
    if(offset == -1)
      return 0;
    d->file->seek(offset);
    ByteVector data = d->file->readBlock(32);
    if(data.size() != 32)
      return 0;
    const int version = data.toShort(8, false);
    if(version < MIN_STREAM_VERS || version > MAX_STREAM_VERS)
      continue;
    const unsigned int flags = data.toUInt(24, false);
    if(!(flags & FINAL_BLOCK))
      return 0;
    const unsigned int blockIndex   = data.toUInt(16, false);
    const unsigned int blockSamples = data.toUInt(20, false);
    return blockIndex + blockSamples;
  }

  return 0;
}

