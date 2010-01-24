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

#include <tbytevector.h>
#include <tdebug.h>
#include <tstring.h>

#include "rifffile.h"
#include <vector>

using namespace TagLib;

class RIFF::File::FilePrivate
{
public:
  FilePrivate() :
    endianness(BigEndian),
    size(0)
  {

  }
  Endianness endianness;
  ByteVector type;
  uint size;
  ByteVector format;

  std::vector<ByteVector> chunkNames;
  std::vector<uint> chunkOffsets;
  std::vector<uint> chunkSizes;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

RIFF::File::~File()
{
  delete d;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

RIFF::File::File(FileName file, Endianness endianness) : TagLib::File(file)
{
  d = new FilePrivate;
  d->endianness = endianness;

  if(isOpen())
    read();
}

TagLib::uint RIFF::File::chunkCount() const
{
  return d->chunkNames.size();
}

TagLib::uint RIFF::File::chunkOffset(uint i) const
{
  return d->chunkOffsets[i];
}

ByteVector RIFF::File::chunkName(uint i) const
{
  if(i >= chunkCount())
    return ByteVector::null;

  return d->chunkNames[i];
}

ByteVector RIFF::File::chunkData(uint i)
{
  if(i >= chunkCount())
    return ByteVector::null;

  // Offset for the first subchunk's data

  long begin = 12 + 8;

  for(uint it = 0; it < i; it++)
    begin += 8 + d->chunkSizes[it];

  seek(begin);

  return readBlock(d->chunkSizes[i]);
}

void RIFF::File::setChunkData(const ByteVector &name, const ByteVector &data)
{
  if(d->chunkNames.size() == 0)
  {
    debug("RIFF::File::setChunkData - No valid chunks found.");
    return;
  }

  for(uint i = 0; i < d->chunkNames.size(); i++) {
    if(d->chunkNames[i] == name) {

      int sizeDifference = data.size() - d->chunkSizes[i];

      // First we update the global size

      insert(ByteVector::fromUInt(d->size + sizeDifference,
                                  d->endianness == BigEndian), 4, 4);

      // Now update the specific chunk

      writeChunk(name, data, d->chunkOffsets[i] - 8, d->chunkSizes[i] + 8);

      // Now update the internal offsets

      for(i++; i < d->chunkNames.size(); i++)
        d->chunkOffsets[i] += sizeDifference;

      return;
    }
  }

  // Couldn't find an existing chunk, so let's create a new one.  First update
  // the global size:

  insert(ByteVector::fromUInt(d->size + data.size() + 8, d->endianness == BigEndian), 4, 4);
  writeChunk(name, data, d->chunkOffsets.back() + d->chunkSizes.back());
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void RIFF::File::read()
{
  bool bigEndian = (d->endianness == BigEndian);

  d->type = readBlock(4);
  d->size = readBlock(4).toUInt(bigEndian);
  d->format = readBlock(4);

  while(tell() < length()) {
    ByteVector chunkName = readBlock(4);
    uint chunkSize = readBlock(4).toUInt(bigEndian);

    d->chunkNames.push_back(chunkName);
    d->chunkSizes.push_back(chunkSize);

    d->chunkOffsets.push_back(tell());

    seek(chunkSize, Current);
  }
}

void RIFF::File::writeChunk(const ByteVector &name, const ByteVector &data,
                            ulong offset, ulong replace)
{
  ByteVector combined = name;
  combined.append(ByteVector::fromUInt(data.size(), d->endianness == BigEndian));
  combined.append(data);
  insert(combined, offset, replace);
}
