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

#include "tstringlist.h"
#include "tdebug.h"
#include "xmfile.h"
#include "modfileprivate.h"
#include "tpropertymap.h"

#include <string.h>
#include <algorithm>

using namespace TagLib;
using namespace XM;
using TagLib::uint;
using TagLib::ushort;
using TagLib::ulong;

/*!
 * The Reader classes are helpers to make handling of the stripped XM
 * format more easy. In the stripped XM format certain header sizes might
 * be smaller than one would expect. The fields that are not included
 * are then just some predefined valued (e.g. 0).
 *
 * Using these classes this code:
 *
 *   if(headerSize >= 4) {
 *     if(!readU16L(value1)) ERROR();
 *     if(headerSize >= 8) {
 *       if(!readU16L(value2)) ERROR();
 *       if(headerSize >= 12) {
 *         if(!readString(value3, 22)) ERROR();
 *         ...
 *       }
 *     }
 *   }
 *
 * Becomes:
 *
 *   StructReader header;
 *   header.u16L(value1).u16L(value2).string(value3, 22). ...;
 *   if(header.read(*this, headerSize) < std::min(header.size(), headerSize))
 *     ERROR();
 *
 * Maybe if this is useful to other formats these classes can be moved to
 * their own public files.
 */
class Reader
{
public:
  virtual ~Reader()
  {
  }

  /*!
   * Reads associated values from \a file, but never reads more
   * then \a limit bytes.
   */
  virtual uint read(TagLib::File &file, uint limit) = 0;

  /*!
   * Returns the number of bytes this reader would like to read.
   */
  virtual uint size() const = 0;
};

class SkipReader : public Reader
{
public:
  SkipReader(uint size) : m_size(size)
  {
  }

  uint read(TagLib::File &file, uint limit)
  {
    uint count = std::min(m_size, limit);
    file.seek(count, TagLib::File::Current);
    return count;
  }

  uint size() const
  {
    return m_size;
  }

private:
  uint m_size;
};

template<typename T>
class ValueReader : public Reader
{
public:
  ValueReader(T &value) : value(value)
  {
  }

protected:
  T &value;
};

class StringReader : public ValueReader<String>
{
public:
  StringReader(String &string, uint size) :
    ValueReader<String>(string), m_size(size)
  {
  }

  uint read(TagLib::File &file, uint limit)
  {
    ByteVector data = file.readBlock(std::min(m_size, limit));
    uint count = data.size();
    int index = data.find((char) 0);
    if(index > -1) {
      data.resize(index);
    }
    data.replace((char) 0xff, ' ');
    value = data;
    return count;
  }

  uint size() const
  {
    return m_size;
  }

private:
  uint m_size;
};

class ByteReader : public ValueReader<uchar>
{
public:
  ByteReader(uchar &byte) : ValueReader<uchar>(byte) {}

  uint read(TagLib::File &file, uint limit)
  {
    ByteVector data = file.readBlock(std::min(1U,limit));
    if(data.size() > 0) {
      value = data[0];
    }
    return data.size();
  }

  uint size() const
  {
    return 1;
  }
};

template<typename T>
class NumberReader : public ValueReader<T>
{
public:
  NumberReader(T &value, bool bigEndian) :
    ValueReader<T>(value), bigEndian(bigEndian)
  {
  }

protected:
  bool bigEndian;
};

class U16Reader : public NumberReader<ushort>
{
public:
  U16Reader(ushort &value, bool bigEndian)
  : NumberReader<ushort>(value, bigEndian) {}

  uint read(TagLib::File &file, uint limit)
  {
    ByteVector data = file.readBlock(std::min(2U,limit));
    value = data.toUShort(bigEndian);
    return data.size();
  }

  uint size() const
  {
    return 2;
  }
};

class U32Reader : public NumberReader<ulong>
{
public:
  U32Reader(ulong &value, bool bigEndian = true) :
    NumberReader<ulong>(value, bigEndian)
  {
  }

  uint read(TagLib::File &file, uint limit)
  {
    ByteVector data = file.readBlock(std::min(4U,limit));
    value = data.toUInt(bigEndian);
    return data.size();
  }

  uint size() const
  {
    return 4;
  }
};

class StructReader : public Reader
{
public:
  StructReader()
  {
    m_readers.setAutoDelete(true);
  }

  /*!
   * Add a nested reader. This reader takes ownership.
   */
  StructReader &reader(Reader *reader)
  {
    m_readers.append(reader);
    return *this;
  }

  /*!
   * Don't read anything but skip \a size bytes.
   */
  StructReader &skip(uint size)
  {
    m_readers.append(new SkipReader(size));
    return *this;
  }

  /*!
   * Read a string of \a size characters (bytes) into \a string.
   */
  StructReader &string(String &string, uint size)
  {
    m_readers.append(new StringReader(string, size));
    return *this;
  }

  /*!
   * Read a byte into \a byte.
   */
  StructReader &byte(uchar &byte)
  {
    m_readers.append(new ByteReader(byte));
    return *this;
  }

  /*!
   * Read a unsigned 16 Bit integer into \a number. The byte order
   * is controlled by \a bigEndian.
   */
  StructReader &u16(ushort &number, bool bigEndian)
  {
    m_readers.append(new U16Reader(number, bigEndian));
    return *this;
  }

  /*!
   * Read a unsigned 16 Bit little endian integer into \a number.
   */
  StructReader &u16L(ushort &number)
  {
    return u16(number, false);
  }

  /*!
   * Read a unsigned 16 Bit big endian integer into \a number.
   */
  StructReader &u16B(ushort &number)
  {
    return u16(number, true);
  }

  /*!
   * Read a unsigned 32 Bit integer into \a number. The byte order
   * is controlled by \a bigEndian.
   */
  StructReader &u32(ulong &number, bool bigEndian)
  {
    m_readers.append(new U32Reader(number, bigEndian));
    return *this;
  }

  /*!
   * Read a unsigned 32 Bit little endian integer into \a number.
   */
  StructReader &u32L(ulong &number)
  {
    return u32(number, false);
  }

  /*!
   * Read a unsigned 32 Bit big endian integer into \a number.
   */
  StructReader &u32B(ulong &number)
  {
    return u32(number, true);
  }

  uint size() const
  {
    uint size = 0;
    for(List<Reader*>::ConstIterator i = m_readers.begin();
        i != m_readers.end(); ++ i) {
      size += (*i)->size();
    }
    return size;
  }

  uint read(TagLib::File &file, uint limit)
  {
    uint sumcount = 0;
    for(List<Reader*>::Iterator i = m_readers.begin();
        limit > 0 && i != m_readers.end(); ++ i) {
      uint count = (*i)->read(file, limit);
      limit    -= count;
      sumcount += count;
    }
    return sumcount;
  }

private:
  List<Reader*> m_readers;
};

class XM::File::FilePrivate
{
public:
  FilePrivate(AudioProperties::ReadStyle propertiesStyle)
    : tag(), properties(propertiesStyle)
  {
  }

  Mod::Tag       tag;
  XM::Properties properties;
};

XM::File::File(FileName file, bool readProperties,
               AudioProperties::ReadStyle propertiesStyle) :
  Mod::FileBase(file),
  d(new FilePrivate(propertiesStyle))
{
  if(isOpen())
    read(readProperties);
}

XM::File::File(IOStream *stream, bool readProperties,
               AudioProperties::ReadStyle propertiesStyle) :
  Mod::FileBase(stream),
  d(new FilePrivate(propertiesStyle))
{
  if(isOpen())
    read(readProperties);
}

XM::File::~File()
{
  delete d;
}

Mod::Tag *XM::File::tag() const
{
  return &d->tag;
}

PropertyMap XM::File::properties() const
{
  return d->tag.properties();
}

PropertyMap XM::File::setProperties(const PropertyMap &properties)
{
  return d->tag.setProperties(properties);
}

XM::Properties *XM::File::audioProperties() const
{
  return &d->properties;
}

bool XM::File::save()
{
  if(readOnly()) {
    debug("XM::File::save() - Cannot save to a read only file.");
    return false;
  }
  seek(17);
  writeString(d->tag.title(), 20);
  seek(1, Current);
  writeString(d->tag.trackerName(), 20);
  seek(2, Current);
  ulong headerSize = 0;
  if(!readU32L(headerSize))
    return false;
  seek(2+2+2, Current);

  ushort patternCount = 0;
  ushort instrumentCount = 0;
  if(!readU16L(patternCount) || !readU16L(instrumentCount))
    return false;

  seek(60 + headerSize);

  // need to read patterns again in order to seek to the instruments:
  for(ushort i = 0; i < patternCount; ++ i) {
    ulong patternHeaderLength = 0;
    if(!readU32L(patternHeaderLength) || patternHeaderLength < 4)
      return false;

    ushort dataSize = 0;
    StructReader pattern;
    pattern.skip(3).u16L(dataSize);

    uint count = pattern.read(*this, patternHeaderLength - 4U);
    if(count != std::min(patternHeaderLength - 4U, (ulong)pattern.size()))
      return false;

    seek(patternHeaderLength - (4 + count) + dataSize, Current);
  }

  StringList lines = d->tag.comment().split("\n");
  uint sampleNameIndex = instrumentCount;
  for(ushort i = 0; i < instrumentCount; ++ i) {
    ulong instrumentHeaderSize = 0;
    if(!readU32L(instrumentHeaderSize) || instrumentHeaderSize < 4)
      return false;

    uint len = std::min(22UL, instrumentHeaderSize - 4U);
    if(i >= lines.size())
      writeString(String::null, len);
    else
      writeString(lines[i], len);

    long offset = 0;
    if(instrumentHeaderSize >= 29U) {
      ushort sampleCount = 0;
      seek(1, Current);
      if(!readU16L(sampleCount))
        return false;

      if(sampleCount > 0) {
        ulong sampleHeaderSize = 0;
        if(instrumentHeaderSize < 33U || !readU32L(sampleHeaderSize))
          return false;
        // skip unhandeled header proportion:
        seek(instrumentHeaderSize - 33, Current);

        for(ushort j = 0; j < sampleCount; ++ j) {
          if(sampleHeaderSize > 4U) {
            ulong sampleLength = 0;
            if(!readU32L(sampleLength))
              return false;
            offset += sampleLength;

            seek(std::min(sampleHeaderSize, 14UL), Current);
            if(sampleHeaderSize > 18U) {
              uint len = std::min(sampleHeaderSize - 18U, 22UL);
              if(sampleNameIndex >= lines.size())
                writeString(String::null, len);
              else
                writeString(lines[sampleNameIndex ++], len);
              seek(sampleHeaderSize - (18U + len), Current);
            }
          }
          else {
            seek(sampleHeaderSize, Current);
          }
        }
      }
      else {
        offset = instrumentHeaderSize - 29;
      }
    }
    else {
      offset = instrumentHeaderSize - (4 + len);
    }
    seek(offset, Current);
  }

  return true;
}

void XM::File::read(bool)
{
  if(!isOpen())
    return;

  seek(0);
  ByteVector magic = readBlock(17);
  // it's all 0x00 for stripped XM files:
  READ_ASSERT(magic == "Extended Module: " || magic == ByteVector(17, 0));

  READ_STRING(d->tag.setTitle, 20);
  READ_BYTE_AS(escape);
  // in stripped XM files this is 0x00:
  READ_ASSERT(escape == 0x1A || escape == 0x00);

  READ_STRING(d->tag.setTrackerName, 20);
  READ_U16L(d->properties.setVersion);

  READ_U32L_AS(headerSize);
  READ_ASSERT(headerSize >= 4);

  ushort length          = 0;
  ushort restartPosition = 0;
  ushort channels        = 0;
  ushort patternCount    = 0;
  ushort instrumentCount = 0;
  ushort flags    = 0;
  ushort tempo    = 0;
  ushort bpmSpeed = 0;

  StructReader header;
  header.u16L(length)
        .u16L(restartPosition)
        .u16L(channels)
        .u16L(patternCount)
        .u16L(instrumentCount)
        .u16L(flags)
        .u16L(tempo)
        .u16L(bpmSpeed);

  uint count = header.read(*this, headerSize - 4U);
  uint size = std::min(headerSize - 4U, (ulong)header.size());

  READ_ASSERT(count == size);

  d->properties.setLengthInPatterns(length);
  d->properties.setRestartPosition(restartPosition);
  d->properties.setChannels(channels);
  d->properties.setPatternCount(patternCount);
  d->properties.setInstrumentCount(instrumentCount);
  d->properties.setFlags(flags);
  d->properties.setTempo(tempo);
  d->properties.setBpmSpeed(bpmSpeed);

  seek(60 + headerSize);

  // read patterns:
  for(ushort i = 0; i < patternCount; ++ i) {
    READ_U32L_AS(patternHeaderLength);
    READ_ASSERT(patternHeaderLength >= 4);

    uchar  packingType = 0;
    ushort rowCount = 0;
    ushort dataSize = 0;
    StructReader pattern;
    pattern.byte(packingType).u16L(rowCount).u16L(dataSize);

    uint count = pattern.read(*this, patternHeaderLength - 4U);
    READ_ASSERT(count == std::min(patternHeaderLength - 4U, (ulong)pattern.size()));

    seek(patternHeaderLength - (4 + count) + dataSize, Current);
  }

  StringList intrumentNames;
  StringList sampleNames;
  uint sumSampleCount = 0;

  // read instruments:
  for(ushort i = 0; i < instrumentCount; ++ i) {
    READ_U32L_AS(instrumentHeaderSize);
    READ_ASSERT(instrumentHeaderSize >= 4);

    String instrumentName;
    uchar  instrumentType = 0;
    ushort sampleCount = 0;

    StructReader instrument;
    instrument.string(instrumentName, 22).byte(instrumentType).u16L(sampleCount);

    // 4 for instrumentHeaderSize
    uint count = 4 + instrument.read(*this, instrumentHeaderSize - 4U);
    READ_ASSERT(count == std::min(instrumentHeaderSize, (ulong)instrument.size() + 4));

    ulong sampleHeaderSize = 0;
    long offset = 0;
    if(sampleCount > 0) {
      sumSampleCount += sampleCount;
      // wouldn't know which header size to assume otherwise:
      READ_ASSERT(instrumentHeaderSize >= count + 4 && readU32L(sampleHeaderSize));
      // skip unhandeled header proportion:
      seek(instrumentHeaderSize - count - 4, Current);

      for(ushort j = 0; j < sampleCount; ++ j) {
        ulong sampleLength = 0;
        ulong loopStart    = 0;
        ulong loopLength   = 0;
        uchar volume       = 0;
        uchar finetune     = 0;
        uchar sampleType   = 0;
        uchar panning      = 0;
        uchar noteNumber   = 0;
        uchar compression  = 0;
        String sampleName;
        StructReader sample;
        sample.u32L(sampleLength)
              .u32L(loopStart)
              .u32L(loopLength)
              .byte(volume)
              .byte(finetune)
              .byte(sampleType)
              .byte(panning)
              .byte(noteNumber)
              .byte(compression)
              .string(sampleName, 22);

        uint count = sample.read(*this, sampleHeaderSize);
        READ_ASSERT(count == std::min(sampleHeaderSize, (ulong)sample.size()));
        // skip unhandeled header proportion:
        seek(sampleHeaderSize - count, Current);

        offset += sampleLength;
        sampleNames.append(sampleName);
      }
    }
    else {
      offset = instrumentHeaderSize - count;
    }
    intrumentNames.append(instrumentName);
    seek(offset, Current);
  }

  d->properties.setSampleCount(sumSampleCount);
  String comment(intrumentNames.toString("\n"));
  if(sampleNames.size() > 0) {
    comment += "\n";
    comment += sampleNames.toString("\n");
  }
  d->tag.setComment(comment);
}
