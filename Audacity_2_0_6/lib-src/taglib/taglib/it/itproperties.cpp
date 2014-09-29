/***************************************************************************
    copyright           :(C) 2011 by Mathias Panzenböck
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

#include "itproperties.h"

using namespace TagLib;
using namespace IT;

class IT::Properties::PropertiesPrivate
{
public:
  PropertiesPrivate() :
    channels(0),
    lengthInPatterns(0),
    instrumentCount(0),
    sampleCount(0),
    patternCount(0),
    version(0),
    compatibleVersion(0),
    flags(0),
    special(0),
    globalVolume(0),
    mixVolume(0),
    tempo(0),
    bpmSpeed(0),
    panningSeparation(0),
    pitchWheelDepth(0)
  {
  }

  int    channels;
  ushort lengthInPatterns;
  ushort instrumentCount;
  ushort sampleCount;
  ushort patternCount;
  ushort version;
  ushort compatibleVersion;
  ushort flags;
  ushort special;
  uchar  globalVolume;
  uchar  mixVolume;
  uchar  tempo;
  uchar  bpmSpeed;
  uchar  panningSeparation;
  uchar  pitchWheelDepth;
};

IT::Properties::Properties(AudioProperties::ReadStyle propertiesStyle) :
  AudioProperties(propertiesStyle),
  d(new PropertiesPrivate)
{
}

IT::Properties::~Properties()
{
  delete d;
}

int IT::Properties::length() const
{
  return 0;
}

int IT::Properties::bitrate() const
{
  return 0;
}

int IT::Properties::sampleRate() const
{
  return 0;
}

int IT::Properties::channels() const
{
  return d->channels;
}

TagLib::ushort IT::Properties::lengthInPatterns() const
{
  return d->lengthInPatterns;
}

bool IT::Properties::stereo() const
{
  return d->flags & Stereo;
}

TagLib::ushort IT::Properties::instrumentCount() const
{
  return d->instrumentCount;
}

TagLib::ushort IT::Properties::sampleCount() const
{
  return d->sampleCount;
}

TagLib::ushort IT::Properties::patternCount() const
{
  return d->patternCount;
}

TagLib::ushort IT::Properties::version() const
{
  return d->version;
}

TagLib::ushort IT::Properties::compatibleVersion() const
{
  return d->compatibleVersion;
}

TagLib::ushort IT::Properties::flags() const
{
  return d->flags;
}

TagLib::ushort IT::Properties::special() const
{
  return d->special;
}

uchar IT::Properties::globalVolume() const
{
  return d->globalVolume;
}

uchar IT::Properties::mixVolume() const
{
  return d->mixVolume;
}

uchar IT::Properties::tempo() const
{
  return d->tempo;
}

uchar IT::Properties::bpmSpeed() const
{
  return d->bpmSpeed;
}

uchar IT::Properties::panningSeparation() const
{
  return d->panningSeparation;
}

uchar IT::Properties::pitchWheelDepth() const
{
  return d->pitchWheelDepth;
}

void IT::Properties::setChannels(int channels)
{
  d->channels = channels;
}

void IT::Properties::setLengthInPatterns(ushort lengthInPatterns)
{
  d->lengthInPatterns = lengthInPatterns;
}

void IT::Properties::setInstrumentCount(ushort instrumentCount)
{
  d->instrumentCount = instrumentCount;
}

void IT::Properties::setSampleCount(ushort sampleCount)
{
  d->sampleCount = sampleCount;
}

void IT::Properties::setPatternCount(ushort patternCount)
{
  d->patternCount = patternCount;
}

void IT::Properties::setFlags(ushort flags)
{
  d->flags = flags;
}

void IT::Properties::setSpecial(ushort special)
{
  d->special = special;
}

void IT::Properties::setCompatibleVersion(ushort compatibleVersion)
{
  d->compatibleVersion = compatibleVersion;
}

void IT::Properties::setVersion(ushort version)
{
  d->version = version;
}

void IT::Properties::setGlobalVolume(uchar globalVolume)
{
  d->globalVolume = globalVolume;
}

void IT::Properties::setMixVolume(uchar mixVolume)
{
  d->mixVolume = mixVolume;
}

void IT::Properties::setTempo(uchar tempo)
{
  d->tempo = tempo;
}

void IT::Properties::setBpmSpeed(uchar bpmSpeed)
{
  d->bpmSpeed = bpmSpeed;
}

void IT::Properties::setPanningSeparation(uchar panningSeparation)
{
  d->panningSeparation = panningSeparation;
}

void IT::Properties::setPitchWheelDepth(uchar pitchWheelDepth)
{
  d->pitchWheelDepth = pitchWheelDepth;
}
