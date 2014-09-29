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

#include <tdebug.h>
#include <tmap.h>

#include "relativevolumeframe.h"

using namespace TagLib;
using namespace ID3v2;

static inline int bitsToBytes(int i)
{
  return i % 8 == 0 ? i / 8 : (i - i % 8) / 8 + 1;
}

struct ChannelData
{
  ChannelData() : channelType(RelativeVolumeFrame::Other), volumeAdjustment(0) {}

  RelativeVolumeFrame::ChannelType channelType;
  short volumeAdjustment;
  RelativeVolumeFrame::PeakVolume peakVolume;
};

class RelativeVolumeFrame::RelativeVolumeFramePrivate
{
public:
  String identification;
  Map<ChannelType, ChannelData> channels;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

RelativeVolumeFrame::RelativeVolumeFrame() : Frame("RVA2")
{
  d = new RelativeVolumeFramePrivate;
}

RelativeVolumeFrame::RelativeVolumeFrame(const ByteVector &data) : Frame(data)
{
  d = new RelativeVolumeFramePrivate;
  setData(data);
}

RelativeVolumeFrame::~RelativeVolumeFrame()
{
  delete d;
}

String RelativeVolumeFrame::toString() const
{
  return d->identification;
}

List<RelativeVolumeFrame::ChannelType> RelativeVolumeFrame::channels() const
{
  List<ChannelType> l;

  Map<ChannelType, ChannelData>::ConstIterator it = d->channels.begin();
  for(; it != d->channels.end(); ++it)
    l.append((*it).first);

  return l;
}

// deprecated

RelativeVolumeFrame::ChannelType RelativeVolumeFrame::channelType() const
{
  return MasterVolume;
}

// deprecated

void RelativeVolumeFrame::setChannelType(ChannelType)
{

}

short RelativeVolumeFrame::volumeAdjustmentIndex(ChannelType type) const
{
  return d->channels.contains(type) ? d->channels[type].volumeAdjustment : 0;
}

short RelativeVolumeFrame::volumeAdjustmentIndex() const
{
  return volumeAdjustmentIndex(MasterVolume);
}

void RelativeVolumeFrame::setVolumeAdjustmentIndex(short index, ChannelType type)
{
  d->channels[type].volumeAdjustment = index;
}

void RelativeVolumeFrame::setVolumeAdjustmentIndex(short index)
{
  setVolumeAdjustmentIndex(index, MasterVolume);
}

float RelativeVolumeFrame::volumeAdjustment(ChannelType type) const
{
  return d->channels.contains(type) ? float(d->channels[type].volumeAdjustment) / float(512) : 0;
}

float RelativeVolumeFrame::volumeAdjustment() const
{
  return volumeAdjustment(MasterVolume);
}

void RelativeVolumeFrame::setVolumeAdjustment(float adjustment, ChannelType type)
{
  d->channels[type].volumeAdjustment = short(adjustment * float(512));
}

void RelativeVolumeFrame::setVolumeAdjustment(float adjustment)
{
  setVolumeAdjustment(adjustment, MasterVolume);
}

RelativeVolumeFrame::PeakVolume RelativeVolumeFrame::peakVolume(ChannelType type) const
{
  return d->channels.contains(type) ? d->channels[type].peakVolume : PeakVolume();
}

RelativeVolumeFrame::PeakVolume RelativeVolumeFrame::peakVolume() const
{
  return peakVolume(MasterVolume);
}

void RelativeVolumeFrame::setPeakVolume(const PeakVolume &peak, ChannelType type)
{
  d->channels[type].peakVolume = peak;
}

void RelativeVolumeFrame::setPeakVolume(const PeakVolume &peak)
{
  setPeakVolume(peak, MasterVolume);
}

String RelativeVolumeFrame::identification() const
{
  return d->identification;
}

void RelativeVolumeFrame::setIdentification(const String &s)
{
  d->identification = s;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

void RelativeVolumeFrame::parseFields(const ByteVector &data)
{
  int pos = 0;
  d->identification = readStringField(data, String::Latin1, &pos);

  // Each channel is at least 4 bytes.

  while(pos <= (int)data.size() - 4) {


    ChannelType type = ChannelType(data[pos]);
    pos += 1;

    ChannelData &channel = d->channels[type];

    channel.volumeAdjustment = data.toShort(static_cast<uint>(pos));
    pos += 2;

    channel.peakVolume.bitsRepresentingPeak = data[pos];
    pos += 1;

    int bytes = bitsToBytes(channel.peakVolume.bitsRepresentingPeak);
    channel.peakVolume.peakVolume = data.mid(pos, bytes);
    pos += bytes;
  }
}

ByteVector RelativeVolumeFrame::renderFields() const
{
  ByteVector data;

  data.append(d->identification.data(String::Latin1));
  data.append(textDelimiter(String::Latin1));

  Map<ChannelType, ChannelData>::ConstIterator it = d->channels.begin();

  for(; it != d->channels.end(); ++it) {
    ChannelType type = (*it).first;
    const ChannelData &channel = (*it).second;

    data.append(char(type));
    data.append(ByteVector::fromShort(channel.volumeAdjustment));
    data.append(char(channel.peakVolume.bitsRepresentingPeak));
    data.append(channel.peakVolume.peakVolume);
  }

  return data;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

RelativeVolumeFrame::RelativeVolumeFrame(const ByteVector &data, Header *h) : Frame(h)
{
  d = new RelativeVolumeFramePrivate;
  parseFields(fieldData(data));
}
