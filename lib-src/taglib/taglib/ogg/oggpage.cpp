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

#include "oggpage.h"
#include "oggpageheader.h"
#include "oggfile.h"

using namespace TagLib;

class Ogg::Page::PagePrivate
{
public:
  PagePrivate(File *f = 0, long pageOffset = -1) :
    file(f),
    fileOffset(pageOffset),
    packetOffset(0),
    header(f, pageOffset),
    firstPacketIndex(-1)
  {
    if(file) {
      packetOffset = fileOffset + header.size();
      packetSizes = header.packetSizes();
      dataSize = header.dataSize();
    }
  }

  File *file;
  long fileOffset;
  long packetOffset;
  int dataSize;
  List<int> packetSizes;
  PageHeader header;
  int firstPacketIndex;
  ByteVectorList packets;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

Ogg::Page::Page(Ogg::File *file, long pageOffset)
{
  d = new PagePrivate(file, pageOffset);
}

Ogg::Page::~Page()
{
  delete d;
}

long Ogg::Page::fileOffset() const
{
  return d->fileOffset;
}

const Ogg::PageHeader *Ogg::Page::header() const
{
  return &d->header;
}

int Ogg::Page::firstPacketIndex() const
{
  return d->firstPacketIndex;
}

void Ogg::Page::setFirstPacketIndex(int index)
{
  d->firstPacketIndex = index;
}

Ogg::Page::ContainsPacketFlags Ogg::Page::containsPacket(int index) const
{
  int lastPacketIndex = d->firstPacketIndex + packetCount() - 1;
  if(index < d->firstPacketIndex || index > lastPacketIndex)
    return DoesNotContainPacket;

  ContainsPacketFlags flags = DoesNotContainPacket;

  if(index == d->firstPacketIndex)
    flags = ContainsPacketFlags(flags | BeginsWithPacket);

  if(index == lastPacketIndex)
    flags = ContainsPacketFlags(flags | EndsWithPacket);

  // If there's only one page and it's complete:

  if(packetCount() == 1 &&
     !d->header.firstPacketContinued() &&
     d->header.lastPacketCompleted())
  {
    flags = ContainsPacketFlags(flags | CompletePacket);
  }

  // Or if the page is (a) the first page and it's complete or (b) the last page
  // and it's complete or (c) a page in the middle.

  else if((flags & BeginsWithPacket && !d->header.firstPacketContinued()) ||
          (flags & EndsWithPacket && d->header.lastPacketCompleted()) ||
          (!(flags & BeginsWithPacket) && !(flags & EndsWithPacket)))
  {
    flags = ContainsPacketFlags(flags | CompletePacket);
  }

  return flags;
}

TagLib::uint Ogg::Page::packetCount() const
{
  return d->header.packetSizes().size();
}

ByteVectorList Ogg::Page::packets() const
{
  if(!d->packets.isEmpty())
    return d->packets;

  ByteVectorList l;

  if(d->file && d->header.isValid()) {

    d->file->seek(d->packetOffset);

    List<int> packetSizes = d->header.packetSizes();

    List<int>::ConstIterator it = packetSizes.begin();
    for(; it != packetSizes.end(); ++it)
      l.append(d->file->readBlock(*it));
  }
  else
    debug("Ogg::Page::packets() -- attempting to read packets from an invalid page.");

  return l;
}

int Ogg::Page::size() const
{
  return d->header.size() + d->header.dataSize();
}

ByteVector Ogg::Page::render() const
{
  ByteVector data;

  data.append(d->header.render());

  if(d->packets.isEmpty()) {
    if(d->file) {
      d->file->seek(d->packetOffset);
      data.append(d->file->readBlock(d->dataSize));
    }
    else
      debug("Ogg::Page::render() -- this page is empty!");
  }
  else {
    ByteVectorList::ConstIterator it = d->packets.begin();
    for(; it != d->packets.end(); ++it)
      data.append(*it);
  }

  // Compute and set the checksum for the Ogg page.  The checksum is taken over
  // the entire page with the 4 bytes reserved for the checksum zeroed and then
  // inserted in bytes 22-25 of the page header.

  ByteVector checksum = ByteVector::fromUInt(data.checksum(), false);
  for(int i = 0; i < 4; i++)
    data[i + 22] = checksum[i];

  return data;
}

List<Ogg::Page *> Ogg::Page::paginate(const ByteVectorList &packets,
                                      PaginationStrategy strategy,
                                      uint streamSerialNumber,
                                      int firstPage,
                                      bool firstPacketContinued,
                                      bool lastPacketCompleted,
                                      bool containsLastPacket)
{
  List<Page *> l;

  int totalSize = 0;

  for(ByteVectorList::ConstIterator it = packets.begin(); it != packets.end(); ++it)
    totalSize += (*it).size();

  if(strategy == Repaginate || totalSize + packets.size() > 255 * 256) {
    debug("Ogg::Page::paginate() -- Sorry!  Repagination is not yet implemented.");
    return l;
  }

  // TODO: Handle creation of multiple pages here with appropriate pagination.

  Page *p = new Page(packets, streamSerialNumber, firstPage, firstPacketContinued,
                     lastPacketCompleted, containsLastPacket);
  l.append(p);

  return l;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

Ogg::Page::Page(const ByteVectorList &packets,
                uint streamSerialNumber,
                int pageNumber,
                bool firstPacketContinued,
                bool lastPacketCompleted,
                bool containsLastPacket)
{
  d = new PagePrivate;

  ByteVector data;
  List<int> packetSizes;

  d->header.setFirstPageOfStream(pageNumber == 0 && !firstPacketContinued);
  d->header.setLastPageOfStream(containsLastPacket);
  d->header.setFirstPacketContinued(firstPacketContinued);
  d->header.setLastPacketCompleted(lastPacketCompleted);
  d->header.setStreamSerialNumber(streamSerialNumber);
  d->header.setPageSequenceNumber(pageNumber);

  // Build a page from the list of packets.

  for(ByteVectorList::ConstIterator it = packets.begin(); it != packets.end(); ++it) {
    packetSizes.append((*it).size());
    data.append(*it);
  }
  d->packets = packets;
  d->header.setPacketSizes(packetSizes);
}
