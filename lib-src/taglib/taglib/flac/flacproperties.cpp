/***************************************************************************
    copyright            : (C) 2003 by Allan Sandfeld Jensen
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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#include <tstring.h>
#include <tdebug.h>

#include "flacproperties.h"
#include "flacfile.h"

using namespace TagLib;

class FLAC::Properties::PropertiesPrivate
{
public:
  PropertiesPrivate(ByteVector d, long st, ReadStyle s) :
    data(d),
    streamLength(st),
    style(s),
    length(0),
    bitrate(0),
    sampleRate(0),
    sampleWidth(0),
    channels(0) {}

  ByteVector data;
  long streamLength;
  ReadStyle style;
  int length;
  int bitrate;
  int sampleRate;
  int sampleWidth;
  int channels;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

FLAC::Properties::Properties(ByteVector data, long streamLength, ReadStyle style) : AudioProperties(style)
{
  d = new PropertiesPrivate(data, streamLength, style);
  read();
}

FLAC::Properties::Properties(File *file, ReadStyle style) : AudioProperties(style)
{
  d = new PropertiesPrivate(file->streamInfoData(), file->streamLength(), style);
  read();
}

FLAC::Properties::~Properties()
{
  delete d;
}

int FLAC::Properties::length() const
{
  return d->length;
}

int FLAC::Properties::bitrate() const
{
  return d->bitrate;
}

int FLAC::Properties::sampleRate() const
{
  return d->sampleRate;
}

int FLAC::Properties::sampleWidth() const
{
  return d->sampleWidth;
}

int FLAC::Properties::channels() const
{
  return d->channels;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void FLAC::Properties::read()
{
  if(d->data.size() < 18) {
    debug("FLAC::Properties::read() - FLAC properties must contain at least 18 bytes.");
    return;
  }

  int pos = 0;

  // Minimum block size (in samples)
  pos += 2;

  // Maximum block size (in samples)
  pos += 2;

  // Minimum frame size (in bytes)
  pos += 3;

  // Maximum frame size (in bytes)
  pos += 3;

  uint flags = d->data.mid(pos, 4).toUInt(true);
  d->sampleRate = flags >> 12;
  d->channels = ((flags >> 9) & 7) + 1;
  d->sampleWidth = ((flags >> 4) & 31) + 1;

  // The last 4 bits are the most significant 4 bits for the 36 bit
  // stream length in samples. (Audio files measured in days)

  uint highLength =d->sampleRate > 0 ? (((flags & 0xf) << 28) / d->sampleRate) << 4 : 0;
  pos += 4;

  d->length = d->sampleRate > 0 ?
      (d->data.mid(pos, 4).toUInt(true)) / d->sampleRate + highLength : 0;
  pos += 4;

  // Uncompressed bitrate:

  //d->bitrate = ((d->sampleRate * d->channels) / 1000) * d->sampleWidth;

  // Real bitrate:

  d->bitrate = d->length > 0 ? ((d->streamLength * 8UL) / d->length) / 1000 : 0;
}
