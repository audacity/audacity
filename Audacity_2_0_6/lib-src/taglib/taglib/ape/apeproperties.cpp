/***************************************************************************
    copyright            : (C) 2010 by Alex Novichkov
    email                : novichko@atnet.ru

    copyright            : (C) 2006 by Lukáš Lalinský
    email                : lalinsky@gmail.com
                           (original WavPack implementation)
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
#include "id3v2tag.h"
#include "apeproperties.h"
#include "apefile.h"

using namespace TagLib;

class APE::Properties::PropertiesPrivate
{
public:
  PropertiesPrivate(File *file, long streamLength) :
    length(0),
    bitrate(0),
    sampleRate(0),
    channels(0),
    version(0),
    bitsPerSample(0),
    sampleFrames(0),
    file(file),
    streamLength(streamLength) {}

  int length;
  int bitrate;
  int sampleRate;
  int channels;
  int version;
  int bitsPerSample;
  uint sampleFrames;
  File *file;
  long streamLength;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

APE::Properties::Properties(File *file, ReadStyle style) : AudioProperties(style)
{
  d = new PropertiesPrivate(file, file->length());
  read();
}

APE::Properties::~Properties()
{
  delete d;
}

int APE::Properties::length() const
{
  return d->length;
}

int APE::Properties::bitrate() const
{
  return d->bitrate;
}

int APE::Properties::sampleRate() const
{
  return d->sampleRate;
}

int APE::Properties::channels() const
{
  return d->channels;
}

int APE::Properties::version() const
{
  return d->version;
}

int APE::Properties::bitsPerSample() const
{
  return d->bitsPerSample;
}

TagLib::uint APE::Properties::sampleFrames() const
{
  return d->sampleFrames;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////


void APE::Properties::read()
{
  // First we are searching the descriptor
  long offset = findDescriptor();
  if(offset < 0)
    return;

  // Then we read the header common for all versions of APE
  d->file->seek(offset);
  ByteVector commonHeader = d->file->readBlock(6);
  if(!commonHeader.startsWith("MAC "))
    return;
  d->version = commonHeader.toUShort(4, false);

  if(d->version >= 3980) {
    analyzeCurrent();
  }
  else {
    analyzeOld();
  }
}

long APE::Properties::findDescriptor()
{
  long ID3v2Location = findID3v2();
  long ID3v2OriginalSize = 0;
  bool hasID3v2 = false;
  if(ID3v2Location >= 0) {
    ID3v2::Tag tag(d->file, ID3v2Location);
    ID3v2OriginalSize = tag.header()->completeTagSize();
    if(tag.header()->tagSize() > 0)
      hasID3v2 = true;
  }

  long offset = 0;
  if(hasID3v2)
    offset = d->file->find("MAC ", ID3v2Location + ID3v2OriginalSize);
  else
    offset = d->file->find("MAC ");

  if(offset < 0) {
    debug("APE::Properties::findDescriptor() -- APE descriptor not found");
    return -1;
  }

  return offset;
}

long APE::Properties::findID3v2()
{
  if(!d->file->isValid())
    return -1;

  d->file->seek(0);

  if(d->file->readBlock(3) == ID3v2::Header::fileIdentifier())
    return 0;

  return -1;
}

void APE::Properties::analyzeCurrent()
{
  // Read the descriptor
  d->file->seek(2, File::Current);
  ByteVector descriptor = d->file->readBlock(44);
  const uint descriptorBytes = descriptor.toUInt(0, false);

  if ((descriptorBytes - 52) > 0)
    d->file->seek(descriptorBytes - 52, File::Current);

  // Read the header
  ByteVector header = d->file->readBlock(24);

  // Get the APE info
  d->channels      = header.toShort(18, false);
  d->sampleRate    = header.toUInt(20, false);
  d->bitsPerSample = header.toShort(16, false);
  //d->compressionLevel =

  const uint totalFrames      = header.toUInt(12, false);
  const uint blocksPerFrame   = header.toUInt(4, false);
  const uint finalFrameBlocks = header.toUInt(8, false);
  d->sampleFrames = totalFrames > 0 ? (totalFrames -  1) * blocksPerFrame + finalFrameBlocks : 0;
  d->length = d->sampleRate > 0 ? d->sampleFrames / d->sampleRate : 0;
  d->bitrate = d->length > 0 ? ((d->streamLength * 8L) / d->length) / 1000 : 0;
}

void APE::Properties::analyzeOld()
{
  ByteVector header = d->file->readBlock(26);
  const uint totalFrames = header.toUInt(18, false);

  // Fail on 0 length APE files (catches non-finalized APE files)
  if(totalFrames == 0)
    return;

  const short compressionLevel = header.toShort(0, false);
  uint blocksPerFrame;
  if(d->version >= 3950)
    blocksPerFrame = 73728 * 4;
  else if(d->version >= 3900 || (d->version >= 3800 && compressionLevel == 4000))
    blocksPerFrame = 73728;
  else
    blocksPerFrame = 9216;
  d->channels   = header.toShort(4, false);
  d->sampleRate = header.toUInt(6, false);
  const uint finalFrameBlocks = header.toUInt(22, false);
  const uint totalBlocks 
    = totalFrames > 0 ? (totalFrames - 1) * blocksPerFrame + finalFrameBlocks : 0;
  d->length = totalBlocks / d->sampleRate;
  d->bitrate = d->length > 0 ? ((d->streamLength * 8L) / d->length) / 1000 : 0;
}

