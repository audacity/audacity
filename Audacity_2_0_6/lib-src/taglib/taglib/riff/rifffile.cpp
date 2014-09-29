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

#include <tbytevector.h>
#include <tdebug.h>
#include <tstring.h>

#include "rifffile.h"
#include <algorithm>
#include <vector>

using namespace TagLib;

struct Chunk
{
  ByteVector name;
  TagLib::uint offset;
  TagLib::uint size;
  char padding;
};

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
  TagLib::uint size;
  ByteVector format;

  std::vector<Chunk> chunks;
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

RIFF::File::File(IOStream *stream, Endianness endianness) : TagLib::File(stream)
{
  d = new FilePrivate;
  d->endianness = endianness;

  if(isOpen())
    read();
}

TagLib::uint RIFF::File::riffSize() const
{
  return d->size;
}

TagLib::uint RIFF::File::chunkCount() const
{
  return d->chunks.size();
}

TagLib::uint RIFF::File::chunkDataSize(uint i) const
{
  return d->chunks[i].size;
}

TagLib::uint RIFF::File::chunkOffset(uint i) const
{
  return d->chunks[i].offset;
}

TagLib::uint RIFF::File::chunkPadding(uint i) const
{
  return d->chunks[i].padding;
}

ByteVector RIFF::File::chunkName(uint i) const
{
  if(i >= chunkCount())
    return ByteVector::null;

  return d->chunks[i].name;
}

ByteVector RIFF::File::chunkData(uint i)
{
  if(i >= chunkCount())
    return ByteVector::null;

  // Offset for the first subchunk's data

  long begin = 12 + 8;

  for(uint it = 0; it < i; it++)
    begin += 8 + d->chunks[it].size + d->chunks[it].padding;

  seek(begin);

  return readBlock(d->chunks[i].size);
}

void RIFF::File::setChunkData(uint i, const ByteVector &data)
{
  // First we update the global size

  d->size += ((data.size() + 1) & ~1) - (d->chunks[i].size + d->chunks[i].padding);
  insert(ByteVector::fromUInt(d->size, d->endianness == BigEndian), 4, 4);

  // Now update the specific chunk

  writeChunk(chunkName(i), data, d->chunks[i].offset - 8, d->chunks[i].size + d->chunks[i].padding + 8);

  d->chunks[i].size = data.size();
  d->chunks[i].padding = (data.size() & 0x01) ? 1 : 0;

  // Now update the internal offsets

  for(i++; i < d->chunks.size(); i++)
    d->chunks[i].offset = d->chunks[i-1].offset + 8 + d->chunks[i-1].size + d->chunks[i-1].padding;
}

void RIFF::File::setChunkData(const ByteVector &name, const ByteVector &data)
{
  setChunkData(name, data, false);
}

void RIFF::File::setChunkData(const ByteVector &name, const ByteVector &data, bool alwaysCreate)
{
  if(d->chunks.size() == 0) {
    debug("RIFF::File::setChunkData - No valid chunks found.");
    return;
  }

  if(alwaysCreate && name != "LIST") {
    debug("RIFF::File::setChunkData - alwaysCreate should be used for only \"LIST\" chunks.");
    return;
  }

  if(!alwaysCreate) {
    for(uint i = 0; i < d->chunks.size(); i++) {
      if(d->chunks[i].name == name) {
        setChunkData(i, data);
        return;
      }
    }
  }

  // Couldn't find an existing chunk, so let's create a new one.

  uint i =  d->chunks.size() - 1;
  ulong offset = d->chunks[i].offset + d->chunks[i].size;

  // First we update the global size

  d->size += (offset & 1) + data.size() + 8;
  insert(ByteVector::fromUInt(d->size, d->endianness == BigEndian), 4, 4);

  // Now add the chunk to the file

  writeChunk(name, data, offset, std::max<long>(0, length() - offset), (offset & 1) ? 1 : 0);

  // And update our internal structure

  if (offset & 1) {
    d->chunks[i].padding = 1;
    offset++;
  }

  Chunk chunk;
  chunk.name = name;
  chunk.size = data.size();
  chunk.offset = offset + 8;
  chunk.padding = (data.size() & 0x01) ? 1 : 0;

  d->chunks.push_back(chunk);
}

void RIFF::File::removeChunk(uint i)
{
  if(i >= d->chunks.size())
    return;
  
  removeBlock(d->chunks[i].offset - 8, d->chunks[i].size + 8);
  d->chunks.erase(d->chunks.begin() + i);
}

void RIFF::File::removeChunk(const ByteVector &name)
{
  std::vector<Chunk> newChunks;
  for(size_t i = 0; i < d->chunks.size(); ++i) {
    if(d->chunks[i].name == name)
      removeBlock(d->chunks[i].offset - 8, d->chunks[i].size + 8);
    else
      newChunks.push_back(d->chunks[i]);
  }

  d->chunks.swap(newChunks);
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

static bool isValidChunkID(const ByteVector &name)
{
  if(name.size() != 4) {
    return false;
  }
  for(int i = 0; i < 4; i++) {
    if(name[i] < 32 || name[i] > 127) {
      return false;
    }
  }
  return true;
}

void RIFF::File::read()
{
  bool bigEndian = (d->endianness == BigEndian);

  d->type = readBlock(4);
  d->size = readBlock(4).toUInt(bigEndian);
  d->format = readBlock(4);

  // + 8: chunk header at least, fix for additional junk bytes
  while(tell() + 8 <= length()) {
    ByteVector chunkName = readBlock(4);
    uint chunkSize = readBlock(4).toUInt(bigEndian);

    if(!isValidChunkID(chunkName)) {
      debug("RIFF::File::read() -- Chunk '" + chunkName + "' has invalid ID");
      setValid(false);
      break;
    }

    if(static_cast<ulonglong>(tell()) + chunkSize > static_cast<ulonglong>(length())) {
      debug("RIFF::File::read() -- Chunk '" + chunkName + "' has invalid size (larger than the file size)");
      setValid(false);
      break;
    }

    Chunk chunk;
    chunk.name = chunkName;
    chunk.size = chunkSize;
    chunk.offset = tell();

    seek(chunk.size, Current);

    // check padding
    chunk.padding = 0;
    long uPosNotPadded = tell();
    if((uPosNotPadded & 0x01) != 0) {
      ByteVector iByte = readBlock(1);
      if((iByte.size() != 1) || (iByte[0] != 0)) {
        // not well formed, re-seek
        seek(uPosNotPadded, Beginning);
      }
      else {
        chunk.padding = 1;
      }
    }
    d->chunks.push_back(chunk);

  }
}

void RIFF::File::writeChunk(const ByteVector &name, const ByteVector &data,
                            ulong offset, ulong replace, uint leadingPadding)
{
  ByteVector combined;
  if(leadingPadding) {
    combined.append(ByteVector(leadingPadding, '\x00'));
  }
  combined.append(name);
  combined.append(ByteVector::fromUInt(data.size(), d->endianness == BigEndian));
  combined.append(data);
  if((data.size() & 0x01) != 0) {
    combined.append('\x00');
  }
  insert(combined, offset, replace);
}
