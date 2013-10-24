/***************************************************************************
    copyright            : (C) 2004 by Allan Sandfeld Jensen
    email                : kde@carewolf.org
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
#include <bitset>
#include <math.h>

#include "mpcproperties.h"
#include "mpcfile.h"

using namespace TagLib;

class MPC::Properties::PropertiesPrivate
{
public:
  PropertiesPrivate(long length, ReadStyle s) :
    streamLength(length),
    style(s),
    version(0),
    length(0),
    bitrate(0),
    sampleRate(0),
    channels(0),
    totalFrames(0),
    sampleFrames(0),
    trackGain(0),
    trackPeak(0),
    albumGain(0),
    albumPeak(0) {}

  long streamLength;
  ReadStyle style;
  int version;
  int length;
  int bitrate;
  int sampleRate;
  int channels;
  uint totalFrames;
  uint sampleFrames;
  uint trackGain;
  uint trackPeak;
  uint albumGain;
  uint albumPeak;
  String flags;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

MPC::Properties::Properties(const ByteVector &data, long streamLength, ReadStyle style) : AudioProperties(style)
{
  d = new PropertiesPrivate(streamLength, style);
  readSV7(data);
}

MPC::Properties::Properties(File *file, long streamLength, ReadStyle style) : AudioProperties(style)
{
  d = new PropertiesPrivate(streamLength, style);
  ByteVector magic = file->readBlock(4);
  if(magic == "MPCK") {
    // Musepack version 8
    readSV8(file);
  }
  else {
    // Musepack version 7 or older, fixed size header
    readSV7(magic + file->readBlock(MPC::HeaderSize - 4));
  }
}

MPC::Properties::~Properties()
{
  delete d;
}

int MPC::Properties::length() const
{
  return d->length;
}

int MPC::Properties::bitrate() const
{
  return d->bitrate;
}

int MPC::Properties::sampleRate() const
{
  return d->sampleRate;
}

int MPC::Properties::channels() const
{
  return d->channels;
}

int MPC::Properties::mpcVersion() const
{
  return d->version;
}

TagLib::uint MPC::Properties::totalFrames() const
{
  return d->totalFrames;
}

TagLib::uint MPC::Properties::sampleFrames() const
{
  return d->sampleFrames;
}

int MPC::Properties::trackGain() const
{
  return d->trackGain;
}

int MPC::Properties::trackPeak() const
{
  return d->trackPeak;
}

int MPC::Properties::albumGain() const
{
  return d->albumGain;
}

int MPC::Properties::albumPeak() const
{
  return d->albumPeak;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

unsigned long readSize(File *file, TagLib::uint &sizelength)
{
  unsigned char tmp;
  unsigned long size = 0;

  do {
    ByteVector b = file->readBlock(1);
    tmp = b[0];
    size = (size << 7) | (tmp & 0x7F);
    sizelength++;
  } while((tmp & 0x80));
  return size;
}

unsigned long readSize(const ByteVector &data, TagLib::uint &sizelength)
{
  unsigned char tmp;
  unsigned long size = 0;
  unsigned long pos = 0;

  do {
    tmp = data[pos++];
    size = (size << 7) | (tmp & 0x7F);
    sizelength++;
  } while((tmp & 0x80) && (pos < data.size()));
  return size;
}

static const unsigned short sftable [4] = { 44100, 48000, 37800, 32000 };

void MPC::Properties::readSV8(File *file)
{
  bool readSH = false, readRG = false;

  while(!readSH && !readRG) {
    ByteVector packetType = file->readBlock(2);
    uint packetSizeLength = 0;
    unsigned long packetSize = readSize(file, packetSizeLength);
    unsigned long dataSize = packetSize - 2 - packetSizeLength;

    if(packetType == "SH") {
      // Stream Header
      // http://trac.musepack.net/wiki/SV8Specification#StreamHeaderPacket
      ByteVector data = file->readBlock(dataSize);
      readSH = true;

      TagLib::uint pos = 4;
      d->version = data[pos];
      pos += 1;
      d->sampleFrames = readSize(data.mid(pos), pos);
      ulong begSilence = readSize(data.mid(pos), pos);

      std::bitset<16> flags(TAGLIB_CONSTRUCT_BITSET(data.toUShort(pos, true)));
      pos += 2;

      d->sampleRate = sftable[flags[15] * 4 + flags[14] * 2 + flags[13]];
      d->channels = flags[7] * 8 + flags[6] * 4 + flags[5] * 2 + flags[4] + 1;

      if((d->sampleFrames - begSilence) != 0)
        d->bitrate = (int)(d->streamLength * 8.0 * d->sampleRate / (d->sampleFrames - begSilence));
      d->bitrate = d->bitrate / 1000;

      d->length = (d->sampleFrames - begSilence) / d->sampleRate;
    }

    else if (packetType == "RG") {
      // Replay Gain
      // http://trac.musepack.net/wiki/SV8Specification#ReplaygainPacket
      ByteVector data = file->readBlock(dataSize);
      readRG = true;

      int replayGainVersion = data[0];
      if(replayGainVersion == 1) {
        d->trackGain = data.toShort(1, true);
        d->trackPeak = data.toShort(3, true);
        d->albumGain = data.toShort(5, true);
        d->albumPeak = data.toShort(7, true);
      }
    }

    else if(packetType == "SE") {
      break;
    }

    else {
      file->seek(dataSize, File::Current);
    }
  }
}

void MPC::Properties::readSV7(const ByteVector &data)
{
  if(data.startsWith("MP+")) {
    d->version = data[3] & 15;
    if(d->version < 7)
      return;

    d->totalFrames = data.toUInt(4, false);

    std::bitset<32> flags(TAGLIB_CONSTRUCT_BITSET(data.toUInt(8, false)));
    d->sampleRate = sftable[flags[17] * 2 + flags[16]];
    d->channels = 2;

    uint gapless = data.toUInt(5, false);

    d->trackGain = data.toShort(14, false);
    d->trackPeak = data.toShort(12, false);
    d->albumGain = data.toShort(18, false);
    d->albumPeak = data.toShort(16, false);

    // convert gain info
    if(d->trackGain != 0) {
      int tmp = (int)((64.82 - (short)d->trackGain / 100.) * 256. + .5);
      if(tmp >= (1 << 16) || tmp < 0) tmp = 0;
      d->trackGain = tmp;
    }

    if(d->albumGain != 0) {
      int tmp = (int)((64.82 - d->albumGain / 100.) * 256. + .5);
      if(tmp >= (1 << 16) || tmp < 0) tmp = 0;
      d->albumGain = tmp;
    }

    if (d->trackPeak != 0)
      d->trackPeak = (int)(log10((double)d->trackPeak) * 20 * 256 + .5);

    if (d->albumPeak != 0)
      d->albumPeak = (int)(log10((double)d->albumPeak) * 20 * 256 + .5);

    bool trueGapless = (gapless >> 31) & 0x0001;
    if(trueGapless) {
      uint lastFrameSamples = (gapless >> 20) & 0x07FF;
      d->sampleFrames = d->totalFrames * 1152 - lastFrameSamples;
    }
    else
      d->sampleFrames = d->totalFrames * 1152 - 576;
  }
  else {
    uint headerData = data.toUInt(0, false);

    d->bitrate = (headerData >> 23) & 0x01ff;
    d->version = (headerData >> 11) & 0x03ff;
    d->sampleRate = 44100;
    d->channels = 2;

    if(d->version >= 5)
      d->totalFrames = data.toUInt(4, false);
    else
      d->totalFrames = data.toUShort(6, false);

    d->sampleFrames = d->totalFrames * 1152 - 576;
  }

  d->length = d->sampleRate > 0 ? (d->sampleFrames + (d->sampleRate / 2)) / d->sampleRate : 0;

  if(!d->bitrate)
    d->bitrate = d->length > 0 ? ((d->streamLength * 8L) / d->length) / 1000 : 0;
}

