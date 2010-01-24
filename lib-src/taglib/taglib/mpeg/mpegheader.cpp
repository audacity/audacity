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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#include <bitset>

#include <tbytevector.h>
#include <tstring.h>
#include <tdebug.h>

#include "mpegheader.h"

using namespace TagLib;

class MPEG::Header::HeaderPrivate : public RefCounter
{
public:
  HeaderPrivate() :
    isValid(false),
    version(Version1),
    layer(0),
    protectionEnabled(false),
    sampleRate(0),
    isPadded(false),
    channelMode(Stereo),
    isCopyrighted(false),
    isOriginal(false),
    frameLength(0),
    samplesPerFrame(0) {}

  bool isValid;
  Version version;
  int layer;
  bool protectionEnabled;
  int bitrate;
  int sampleRate;
  bool isPadded;
  ChannelMode channelMode;
  bool isCopyrighted;
  bool isOriginal;
  int frameLength;
  int samplesPerFrame;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

MPEG::Header::Header(const ByteVector &data)
{
  d = new HeaderPrivate;
  parse(data);
}

MPEG::Header::Header(const Header &h) : d(h.d)
{
  d->ref();
}

MPEG::Header::~Header()
{
  if (d->deref())
    delete d;
}

bool MPEG::Header::isValid() const
{
  return d->isValid;
}

MPEG::Header::Version MPEG::Header::version() const
{
  return d->version;
}

int MPEG::Header::layer() const
{
  return d->layer;
}

bool MPEG::Header::protectionEnabled() const
{
  return d->protectionEnabled;
}

int MPEG::Header::bitrate() const
{
  return d->bitrate;
}

int MPEG::Header::sampleRate() const
{
  return d->sampleRate;
}

bool MPEG::Header::isPadded() const
{
  return d->isPadded;
}

MPEG::Header::ChannelMode MPEG::Header::channelMode() const
{
  return d->channelMode;
}

bool MPEG::Header::isCopyrighted() const
{
  return d->isCopyrighted;
}

bool MPEG::Header::isOriginal() const
{
  return d->isOriginal;
}

int MPEG::Header::frameLength() const
{
  return d->frameLength;
}

int MPEG::Header::samplesPerFrame() const
{
  return d->samplesPerFrame;
}

MPEG::Header &MPEG::Header::operator=(const Header &h)
{
  if(&h == this)
    return *this;

  if(d->deref())
    delete d;

  d = h.d;
  d->ref();
  return *this;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void MPEG::Header::parse(const ByteVector &data)
{
  if(data.size() < 4 || uchar(data[0]) != 0xff) {
    debug("MPEG::Header::parse() -- First byte did not match MPEG synch.");
    return;
  }

  std::bitset<32> flags(data.toUInt());

  // Check for the second byte's part of the MPEG synch

  if(!flags[23] || !flags[22] || !flags[21]) {
    debug("MPEG::Header::parse() -- Second byte did not match MPEG synch.");
    return;
  }

  // Set the MPEG version

  if(!flags[20] && !flags[19])
    d->version = Version2_5;
  else if(flags[20] && !flags[19])
    d->version = Version2;
  else if(flags[20] && flags[19])
    d->version = Version1;

  // Set the MPEG layer

  if(!flags[18] && flags[17])
    d->layer = 3;
  else if(flags[18] && !flags[17])
    d->layer = 2;
  else if(flags[18] && flags[17])
    d->layer = 1;

  d->protectionEnabled = !flags[16];

  // Set the bitrate

  static const int bitrates[2][3][16] = {
    { // Version 1
      { 0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 0 }, // layer 1
      { 0, 32, 48, 56, 64,  80,  96,  112, 128, 160, 192, 224, 256, 320, 384, 0 }, // layer 2
      { 0, 32, 40, 48, 56,  64,  80,  96,  112, 128, 160, 192, 224, 256, 320, 0 }  // layer 3
    },
    { // Version 2 or 2.5
      { 0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0 }, // layer 1
      { 0, 8,  16, 24, 32, 40, 48, 56,  64,  80,  96,  112, 128, 144, 160, 0 }, // layer 2
      { 0, 8,  16, 24, 32, 40, 48, 56,  64,  80,  96,  112, 128, 144, 160, 0 }  // layer 3
    }
  };

  const int versionIndex = d->version == Version1 ? 0 : 1;
  const int layerIndex = d->layer > 0 ? d->layer - 1 : 0;

  // The bitrate index is encoded as the first 4 bits of the 3rd byte,
  // i.e. 1111xxxx

  int i = uchar(data[2]) >> 4;

  d->bitrate = bitrates[versionIndex][layerIndex][i];

  // Set the sample rate

  static const int sampleRates[3][4] = {
    { 44100, 48000, 32000, 0 }, // Version 1
    { 22050, 24000, 16000, 0 }, // Version 2
    { 11025, 12000, 8000,  0 }  // Version 2.5
  };

  // The sample rate index is encoded as two bits in the 3nd byte, i.e. xxxx11xx

  i = uchar(data[2]) >> 2 & 0x03;

  d->sampleRate = sampleRates[d->version][i];

  if(d->sampleRate == 0) {
    debug("MPEG::Header::parse() -- Invalid sample rate.");
    return;
  }

  // The channel mode is encoded as a 2 bit value at the end of the 3nd byte,
  // i.e. xxxxxx11

  d->channelMode = ChannelMode((uchar(data[3]) & 0xC0) >> 6);

  // TODO: Add mode extension for completeness

  d->isOriginal = flags[2];
  d->isCopyrighted = flags[3];
  d->isPadded = flags[9];

  // Calculate the frame length

  if(d->layer == 1)
    d->frameLength = 24000 * 2 * d->bitrate / d->sampleRate + int(d->isPadded);
  else
    d->frameLength = 72000 * d->bitrate / d->sampleRate + int(d->isPadded);

  // Samples per frame

  static const int samplesPerFrame[3][2] = {
    // MPEG1, 2/2.5
    {  384,   384 }, // Layer I
    { 1152,  1152 }, // Layer II
    { 1152,   576 }  // Layer III
  };

  d->samplesPerFrame = samplesPerFrame[layerIndex][versionIndex];

  // Now that we're done parsing, set this to be a valid frame.

  d->isValid = true;
}
