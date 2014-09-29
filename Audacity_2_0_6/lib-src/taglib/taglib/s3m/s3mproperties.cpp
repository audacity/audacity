/***************************************************************************
    copyright           : (C) 2011 by Mathias Panzenböck
    email               : grosser.meister.morti@gmx.net
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it  under the terms of the GNU Lesser General Public License version  *
 *   2.1 as published by the Free Software Foundation.                     *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful, but   *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *
 *   MA  02110-1301  USA                                                   *
 ***************************************************************************/

#include "s3mproperties.h"

using namespace TagLib;
using namespace S3M;

class S3M::Properties::PropertiesPrivate
{
public:
  PropertiesPrivate() :
    lengthInPatterns(0),
    channels(0),
    stereo(false),
    sampleCount(0),
    patternCount(0),
    flags(0),
    trackerVersion(0),
    fileFormatVersion(0),
    globalVolume(0),
    masterVolume(0),
    tempo(0),
    bpmSpeed(0)
  {
  }

  ushort lengthInPatterns;
  int    channels;
  bool   stereo;
  ushort sampleCount;
  ushort patternCount;
  ushort flags;
  ushort trackerVersion;
  ushort fileFormatVersion;
  uchar  globalVolume;
  uchar  masterVolume;
  uchar  tempo;
  uchar  bpmSpeed;
};

S3M::Properties::Properties(AudioProperties::ReadStyle propertiesStyle) :
  AudioProperties(propertiesStyle),
  d(new PropertiesPrivate)
{
}

S3M::Properties::~Properties()
{
  delete d;
}

int S3M::Properties::length() const
{
  return 0;
}

int S3M::Properties::bitrate() const
{
  return 0;
}

int S3M::Properties::sampleRate() const
{
  return 0;
}

int S3M::Properties::channels() const
{
  return d->channels;
}

TagLib::ushort S3M::Properties::lengthInPatterns() const
{
  return d->lengthInPatterns;
}

bool S3M::Properties::stereo() const
{
  return d->stereo;
}

TagLib::ushort S3M::Properties::sampleCount() const
{
  return d->sampleCount;
}

TagLib::ushort S3M::Properties::patternCount() const
{
  return d->patternCount;
}

TagLib::ushort S3M::Properties::flags() const
{
  return d->flags;
}

TagLib::ushort S3M::Properties::trackerVersion() const
{
  return d->trackerVersion;
}

TagLib::ushort S3M::Properties::fileFormatVersion() const
{
  return d->fileFormatVersion;
}

uchar S3M::Properties::globalVolume() const
{
  return d->globalVolume;
}

uchar S3M::Properties::masterVolume() const
{
  return d->masterVolume;
}

uchar S3M::Properties::tempo() const
{
  return d->tempo;
}

uchar S3M::Properties::bpmSpeed() const
{
  return d->bpmSpeed;
}

void S3M::Properties::setLengthInPatterns(ushort lengthInPatterns)
{
  d->lengthInPatterns = lengthInPatterns;
}

void S3M::Properties::setChannels(int channels)
{
  d->channels = channels;
}

void S3M::Properties::setStereo(bool stereo)
{
  d->stereo = stereo;
}

void S3M::Properties::setSampleCount(ushort sampleCount)
{
  d->sampleCount = sampleCount;
}

void S3M::Properties::setPatternCount(ushort patternCount)
{
  d->patternCount = patternCount;
}

void S3M::Properties::setFlags(ushort flags)
{
  d->flags = flags;
}

void S3M::Properties::setTrackerVersion(ushort trackerVersion)
{
  d->trackerVersion = trackerVersion;
}

void S3M::Properties::setFileFormatVersion(ushort fileFormatVersion)
{
  d->fileFormatVersion = fileFormatVersion;
}

void S3M::Properties::setGlobalVolume(uchar globalVolume)
{
  d->globalVolume = globalVolume;
}

void S3M::Properties::setMasterVolume(uchar masterVolume)
{
  d->masterVolume = masterVolume;
}

void S3M::Properties::setTempo(uchar tempo)
{
  d->tempo = tempo;
}

void S3M::Properties::setBpmSpeed(uchar bpmSpeed)
{
  d->bpmSpeed = bpmSpeed;
}
