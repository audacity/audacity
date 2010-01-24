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

#include <tdebug.h>
#include <tstring.h>

#include "mpegproperties.h"
#include "mpegfile.h"
#include "xingheader.h"

using namespace TagLib;

class MPEG::Properties::PropertiesPrivate
{
public:
  PropertiesPrivate(File *f, ReadStyle s) :
    file(f),
    xingHeader(0),
    style(s),
    length(0),
    bitrate(0),
    sampleRate(0),
    channels(0),
    layer(0),
    version(Header::Version1),
    channelMode(Header::Stereo),
    protectionEnabled(false),
    isCopyrighted(false),
    isOriginal(false) {}

  ~PropertiesPrivate()
  {
    delete xingHeader;
  }

  File *file;
  XingHeader *xingHeader;
  ReadStyle style;
  int length;
  int bitrate;
  int sampleRate;
  int channels;
  int layer;
  Header::Version version;
  Header::ChannelMode channelMode;
  bool protectionEnabled;
  bool isCopyrighted;
  bool isOriginal;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

MPEG::Properties::Properties(File *file, ReadStyle style) : AudioProperties(style)
{
  d = new PropertiesPrivate(file, style);

  if(file && file->isOpen())
    read();
}

MPEG::Properties::~Properties()
{
  delete d;
}

int MPEG::Properties::length() const
{
  return d->length;
}

int MPEG::Properties::bitrate() const
{
  return d->bitrate;
}

int MPEG::Properties::sampleRate() const
{
  return d->sampleRate;
}

int MPEG::Properties::channels() const
{
  return d->channels;
}

const MPEG::XingHeader *MPEG::Properties::xingHeader() const
{
  return d->xingHeader;
}

MPEG::Header::Version MPEG::Properties::version() const
{
  return d->version;
}

int MPEG::Properties::layer() const
{
  return d->layer;
}

bool MPEG::Properties::protectionEnabled() const
{
  return d->protectionEnabled;
}

MPEG::Header::ChannelMode MPEG::Properties::channelMode() const
{
  return d->channelMode;
}

bool MPEG::Properties::isCopyrighted() const
{
  return d->isCopyrighted;
}

bool MPEG::Properties::isOriginal() const
{
  return d->isOriginal;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void MPEG::Properties::read()
{
  // Since we've likely just looked for the ID3v1 tag, start at the end of the
  // file where we're least likely to have to have to move the disk head.

  long last = d->file->lastFrameOffset();

  if(last < 0) {
    debug("MPEG::Properties::read() -- Could not find a valid last MPEG frame in the stream.");
    return;
  }

  d->file->seek(last);
  Header lastHeader(d->file->readBlock(4));

  long first = d->file->firstFrameOffset();

  if(first < 0) {
    debug("MPEG::Properties::read() -- Could not find a valid first MPEG frame in the stream.");
    return;
  }

  if(!lastHeader.isValid()) {

    long pos = last;

    while(pos > first) {

      pos = d->file->previousFrameOffset(pos);

      if(pos < 0)
        break;

      d->file->seek(pos);
      Header header(d->file->readBlock(4));

      if(header.isValid()) {
        lastHeader = header;
        last = pos;
        break;
      }
    }
  }

  // Now jump back to the front of the file and read what we need from there.

  d->file->seek(first);
  Header firstHeader(d->file->readBlock(4));

  if(!firstHeader.isValid() || !lastHeader.isValid()) {
    debug("MPEG::Properties::read() -- Page headers were invalid.");
    return;
  }

  // Check for a Xing header that will help us in gathering information about a
  // VBR stream.

  int xingHeaderOffset = MPEG::XingHeader::xingHeaderOffset(firstHeader.version(),
                                                            firstHeader.channelMode());

  d->file->seek(first + xingHeaderOffset);
  d->xingHeader = new XingHeader(d->file->readBlock(16));

  // Read the length and the bitrate from the Xing header.

  if(d->xingHeader->isValid() &&
     firstHeader.sampleRate() > 0 &&
     d->xingHeader->totalFrames() > 0)
  {
      double timePerFrame =
        double(firstHeader.samplesPerFrame()) / firstHeader.sampleRate();

      d->length = int(timePerFrame * d->xingHeader->totalFrames());
      d->bitrate = d->length > 0 ? d->xingHeader->totalSize() * 8 / d->length / 1000 : 0;
  }
  else {
    // Since there was no valid Xing header found, we hope that we're in a constant
    // bitrate file.

    delete d->xingHeader;
    d->xingHeader = 0;

    // TODO: Make this more robust with audio property detection for VBR without a
    // Xing header.

    if(firstHeader.frameLength() > 0 && firstHeader.bitrate() > 0) {
      int frames = (last - first) / firstHeader.frameLength() + 1;

      d->length = int(float(firstHeader.frameLength() * frames) /
                      float(firstHeader.bitrate() * 125) + 0.5);
      d->bitrate = firstHeader.bitrate();
    }
  }


  d->sampleRate = firstHeader.sampleRate();
  d->channels = firstHeader.channelMode() == Header::SingleChannel ? 1 : 2;
  d->version = firstHeader.version();
  d->layer = firstHeader.layer();
  d->protectionEnabled = firstHeader.protectionEnabled();
  d->channelMode = firstHeader.channelMode();
  d->isCopyrighted = firstHeader.isCopyrighted();
  d->isOriginal = firstHeader.isOriginal();
}
