/**************************************************************************
    copyright            : (C) 2007 by Lukáš Lalinský
    email                : lalinsky@gmail.com
 **************************************************************************/

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef WITH_MP4

#include <tdebug.h>
#include <tstring.h>
#include "mp4file.h"
#include "mp4atom.h"
#include "mp4properties.h"

using namespace TagLib;

class MP4::Properties::PropertiesPrivate
{
public:
  PropertiesPrivate() : length(0), bitrate(0), sampleRate(0), channels(0), bitsPerSample(0) {}

  int length;
  int bitrate;
  int sampleRate;
  int channels;
  int bitsPerSample;
};

MP4::Properties::Properties(File *file, MP4::Atoms *atoms, ReadStyle style)
  : AudioProperties(style)
{
  d = new PropertiesPrivate;

  MP4::Atom *moov = atoms->find("moov");
  if(!moov) {
    debug("MP4: Atom 'moov' not found");
    return;
  }

  MP4::Atom *trak = 0;
  ByteVector data;

  MP4::AtomList trakList = moov->findall("trak");
  for (unsigned int i = 0; i < trakList.size(); i++) {
    trak = trakList[i];
    MP4::Atom *hdlr = trak->find("mdia", "hdlr");
    if(!hdlr) {
      debug("MP4: Atom 'trak.mdia.hdlr' not found");
      return;
    }
    file->seek(hdlr->offset);
    data = file->readBlock(hdlr->length);
    if(data.mid(16, 4) == "soun") {
      break;
    }
    trak = 0;
  }
  if (!trak) {
    debug("MP4: No audio tracks");
    return;
  }

  MP4::Atom *mdhd = trak->find("mdia", "mdhd");
  if(!mdhd) {
    debug("MP4: Atom 'trak.mdia.mdhd' not found");
    return;
  }

  file->seek(mdhd->offset);
  data = file->readBlock(mdhd->length);
  if(data[8] == 0) {
    unsigned int unit = data.mid(20, 4).toUInt();
    unsigned int length = data.mid(24, 4).toUInt();
    d->length = length / unit;
  }
  else {
    long long unit = data.mid(28, 8).toLongLong();
    long long length = data.mid(36, 8).toLongLong();
    d->length = int(length / unit);
  }

  MP4::Atom *atom = trak->find("mdia", "minf", "stbl", "stsd");
  if(!atom) {
    return;
  }

  file->seek(atom->offset);
  data = file->readBlock(atom->length);
  if(data.mid(20, 4) == "mp4a") {
    d->channels = data.mid(40, 2).toShort();
    d->bitsPerSample = data.mid(42, 2).toShort();
    d->sampleRate = data.mid(46, 4).toUInt();
    if(data.mid(56, 4) == "esds" && data[64] == 0x03) {
      long pos = 65;
      if(data.mid(pos, 3) == "\x80\x80\x80") {
        pos += 3;
      }
      pos += 4;
      if(data[pos] == 0x04) {
        pos += 1;
        if(data.mid(pos, 3) == "\x80\x80\x80") {
          pos += 3;
        }
        pos += 10;
        d->bitrate = (data.mid(pos, 4).toUInt() + 500) / 1000;
      }
    }
  }
}

MP4::Properties::~Properties()
{
  delete d;
}

int
MP4::Properties::channels() const
{
  return d->channels;
}

int
MP4::Properties::sampleRate() const
{
  return d->sampleRate;
}

int
MP4::Properties::length() const
{
  return d->length;
}

int
MP4::Properties::bitrate() const
{
  return d->bitrate;
}

int
MP4::Properties::bitsPerSample() const
{
  return d->bitsPerSample;
}

#endif
