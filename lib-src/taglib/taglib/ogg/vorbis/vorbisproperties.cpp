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

#include <tstring.h>
#include <tdebug.h>

#include <oggpageheader.h>

#include "vorbisproperties.h"
#include "vorbisfile.h"

using namespace TagLib;

class Vorbis::Properties::PropertiesPrivate
{
public:
  PropertiesPrivate(File *f, ReadStyle s) :
    file(f),
    style(s),
    length(0),
    bitrate(0),
    sampleRate(0),
    channels(0),
    vorbisVersion(0),
    bitrateMaximum(0),
    bitrateNominal(0),
    bitrateMinimum(0) {}

  File *file;
  ReadStyle style;
  int length;
  int bitrate;
  int sampleRate;
  int channels;
  int vorbisVersion;
  int bitrateMaximum;
  int bitrateNominal;
  int bitrateMinimum;
};

namespace TagLib {
  /*!
   * Vorbis headers can be found with one type ID byte and the string "vorbis" in
   * an Ogg stream.  0x01 indicates the setup header.
   */
  static const char vorbisSetupHeaderID[] = { 0x01, 'v', 'o', 'r', 'b', 'i', 's', 0 };
}

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

Vorbis::Properties::Properties(File *file, ReadStyle style) : AudioProperties(style)
{
  d = new PropertiesPrivate(file, style);
  read();
}

Vorbis::Properties::~Properties()
{
  delete d;
}

int Vorbis::Properties::length() const
{
  return d->length;
}

int Vorbis::Properties::bitrate() const
{
  return int(float(d->bitrate) / float(1000) + 0.5);
}

int Vorbis::Properties::sampleRate() const
{
  return d->sampleRate;
}

int Vorbis::Properties::channels() const
{
  return d->channels;
}

int Vorbis::Properties::vorbisVersion() const
{
  return d->vorbisVersion;
}

int Vorbis::Properties::bitrateMaximum() const
{
  return d->bitrateMaximum;
}

int Vorbis::Properties::bitrateNominal() const
{
  return d->bitrateNominal;
}

int Vorbis::Properties::bitrateMinimum() const
{
  return d->bitrateMinimum;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void Vorbis::Properties::read()
{
  // Get the identification header from the Ogg implementation.

  ByteVector data = d->file->packet(0);

  int pos = 0;

  if(data.mid(pos, 7) != vorbisSetupHeaderID) {
    debug("Vorbis::Properties::read() -- invalid Vorbis identification header");
    return;
  }

  pos += 7;

  d->vorbisVersion = data.mid(pos, 4).toUInt(false);
  pos += 4;

  d->channels = uchar(data[pos]);
  pos += 1;

  d->sampleRate = data.mid(pos, 4).toUInt(false);
  pos += 4;

  d->bitrateMaximum = data.mid(pos, 4).toUInt(false);
  pos += 4;

  d->bitrateNominal = data.mid(pos, 4).toUInt(false);
  pos += 4;

  d->bitrateMinimum = data.mid(pos, 4).toUInt(false);

  // TODO: Later this should be only the "fast" mode.
  d->bitrate = d->bitrateNominal;

  // Find the length of the file.  See http://wiki.xiph.org/VorbisStreamLength/
  // for my notes on the topic.

  const Ogg::PageHeader *first = d->file->firstPageHeader();
  const Ogg::PageHeader *last = d->file->lastPageHeader();

  if(first && last) {
    long long start = first->absoluteGranularPosition();
    long long end = last->absoluteGranularPosition();

    if(start >= 0 && end >= 0 && d->sampleRate > 0)
      d->length = (end - start) / (long long) d->sampleRate;
    else
      debug("Vorbis::Properties::read() -- Either the PCM values for the start or "
            "end of this file was incorrect or the sample rate is zero.");
  }
  else
    debug("Vorbis::Properties::read() -- Could not find valid first and last Ogg pages.");
}
