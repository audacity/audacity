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

#include <stdlib.h>

#include <bitset>

#include <tstring.h>
#include <tdebug.h>
#include <taglib.h>

#include "oggpageheader.h"
#include "oggfile.h"

using namespace TagLib;

class Ogg::PageHeader::PageHeaderPrivate
{
public:
  PageHeaderPrivate(File *f, long pageOffset) :
    file(f),
    fileOffset(pageOffset),
    isValid(false),
    firstPacketContinued(false),
    lastPacketCompleted(false),
    firstPageOfStream(false),
    lastPageOfStream(false),
    absoluteGranularPosition(0),
    streamSerialNumber(0),
    pageSequenceNumber(-1),
    size(0),
    dataSize(0)
    {}

  File *file;
  long fileOffset;
  bool isValid;
  List<int> packetSizes;
  bool firstPacketContinued;
  bool lastPacketCompleted;
  bool firstPageOfStream;
  bool lastPageOfStream;
  long long absoluteGranularPosition;
  uint streamSerialNumber;
  int pageSequenceNumber;
  int size;
  int dataSize;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

Ogg::PageHeader::PageHeader(Ogg::File *file, long pageOffset)
{
  d = new PageHeaderPrivate(file, pageOffset);
  if(file && pageOffset >= 0)
      read();
}

Ogg::PageHeader::~PageHeader()
{
  delete d;
}

bool Ogg::PageHeader::isValid() const
{
  return d->isValid;
}

List<int> Ogg::PageHeader::packetSizes() const
{
  return d->packetSizes;
}

void Ogg::PageHeader::setPacketSizes(const List<int> &sizes)
{
  d->packetSizes = sizes;
}

bool Ogg::PageHeader::firstPacketContinued() const
{
  return d->firstPacketContinued;
}

void Ogg::PageHeader::setFirstPacketContinued(bool continued)
{
  d->firstPacketContinued = continued;
}

bool Ogg::PageHeader::lastPacketCompleted() const
{
  return d->lastPacketCompleted;
}

void Ogg::PageHeader::setLastPacketCompleted(bool completed)
{
  d->lastPacketCompleted = completed;
}

bool Ogg::PageHeader::firstPageOfStream() const
{
  return d->firstPageOfStream;
}

void Ogg::PageHeader::setFirstPageOfStream(bool first)
{
  d->firstPageOfStream = first;
}

bool Ogg::PageHeader::lastPageOfStream() const
{
  return d->lastPageOfStream;
}

void Ogg::PageHeader::setLastPageOfStream(bool last)
{
  d->lastPageOfStream = last;
}

long long Ogg::PageHeader::absoluteGranularPosition() const
{
  return d->absoluteGranularPosition;
}

void Ogg::PageHeader::setAbsoluteGranularPosition(long long agp)
{
  d->absoluteGranularPosition = agp;
}

int Ogg::PageHeader::pageSequenceNumber() const
{
  return d->pageSequenceNumber;
}

void Ogg::PageHeader::setPageSequenceNumber(int sequenceNumber)
{
  d->pageSequenceNumber = sequenceNumber;
}

TagLib::uint Ogg::PageHeader::streamSerialNumber() const
{
  return d->streamSerialNumber;
}

void Ogg::PageHeader::setStreamSerialNumber(uint n)
{
  d->streamSerialNumber = n;
}

int Ogg::PageHeader::size() const
{
  return d->size;
}

int Ogg::PageHeader::dataSize() const
{
  return d->dataSize;
}

ByteVector Ogg::PageHeader::render() const
{
  ByteVector data;

  // capture patern

  data.append("OggS");

  // stream structure version

  data.append(char(0));

  // header type flag

  std::bitset<8> flags;
  flags[0] = d->firstPacketContinued;
  flags[1] = d->pageSequenceNumber == 0;
  flags[2] = d->lastPageOfStream;

  data.append(char(flags.to_ulong()));

  // absolute granular position

  data.append(ByteVector::fromLongLong(d->absoluteGranularPosition, false));

  // stream serial number

  data.append(ByteVector::fromUInt(d->streamSerialNumber, false));

  // page sequence number

  data.append(ByteVector::fromUInt(d->pageSequenceNumber, false));

  // checksum -- this is left empty and should be filled in by the Ogg::Page
  // class

  data.append(ByteVector(4, 0));

  // page segment count and page segment table

  ByteVector pageSegments = lacingValues();

  data.append(char(uchar(pageSegments.size())));
  data.append(pageSegments);

  return data;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void Ogg::PageHeader::read()
{
  d->file->seek(d->fileOffset);

  // An Ogg page header is at least 27 bytes, so we'll go ahead and read that
  // much and then get the rest when we're ready for it.

  ByteVector data = d->file->readBlock(27);

  // Sanity check -- make sure that we were in fact able to read as much data as
  // we asked for and that the page begins with "OggS".

  if(data.size() != 27 || !data.startsWith("OggS")) {
    debug("Ogg::PageHeader::read() -- error reading page header");
    return;
  }

  std::bitset<8> flags(data[5]);

  d->firstPacketContinued = flags.test(0);
  d->firstPageOfStream = flags.test(1);
  d->lastPageOfStream = flags.test(2);

  d->absoluteGranularPosition = data.toLongLong(6, false);
  d->streamSerialNumber = data.toUInt(14, false);
  d->pageSequenceNumber = data.toUInt(18, false);

  // Byte number 27 is the number of page segments, which is the only variable
  // length portion of the page header.  After reading the number of page
  // segments we'll then read in the corresponding data for this count.

  int pageSegmentCount = uchar(data[26]);

  ByteVector pageSegments = d->file->readBlock(pageSegmentCount);

  // Another sanity check.

  if(pageSegmentCount < 1 || int(pageSegments.size()) != pageSegmentCount)
    return;

  // The base size of an Ogg page 27 bytes plus the number of lacing values.

  d->size = 27 + pageSegmentCount;

  int packetSize = 0;

  for(int i = 0; i < pageSegmentCount; i++) {
    d->dataSize += uchar(pageSegments[i]);
    packetSize += uchar(pageSegments[i]);

    if(uchar(pageSegments[i]) < 255) {
      d->packetSizes.append(packetSize);
      packetSize = 0;
    }
  }

  if(packetSize > 0) {
    d->packetSizes.append(packetSize);
    d->lastPacketCompleted = false;
  }
  else
    d->lastPacketCompleted = true;

  d->isValid = true;
}

ByteVector Ogg::PageHeader::lacingValues() const
{
  ByteVector data;

  List<int> sizes = d->packetSizes;
  for(List<int>::ConstIterator it = sizes.begin(); it != sizes.end(); ++it) {

    // The size of a packet in an Ogg page is indicated by a series of "lacing
    // values" where the sum of the values is the packet size in bytes.  Each of
    // these values is a byte.  A value of less than 255 (0xff) indicates the end
    // of the packet.

    div_t n = div(*it, 255);

    for(int i = 0; i < n.quot; i++)
      data.append(char(uchar(255)));

    if(it != --sizes.end() || d->lastPacketCompleted)
      data.append(char(uchar(n.rem)));
  }

  return data;
}
