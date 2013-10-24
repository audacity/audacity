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

#include "s3mfile.h"
#include "tstringlist.h"
#include "tdebug.h"
#include "modfileprivate.h"
#include "tpropertymap.h"

#include <iostream>

using namespace TagLib;
using namespace S3M;

class S3M::File::FilePrivate
{
public:
  FilePrivate(AudioProperties::ReadStyle propertiesStyle)
    : properties(propertiesStyle)
  {
  }

  Mod::Tag        tag;
  S3M::Properties properties;
};

S3M::File::File(FileName file, bool readProperties,
                AudioProperties::ReadStyle propertiesStyle) :
  Mod::FileBase(file),
  d(new FilePrivate(propertiesStyle))
{
  if(isOpen())
    read(readProperties);
}

S3M::File::File(IOStream *stream, bool readProperties,
                AudioProperties::ReadStyle propertiesStyle) :
  Mod::FileBase(stream),
  d(new FilePrivate(propertiesStyle))
{
  if(isOpen())
    read(readProperties);
}

S3M::File::~File()
{
  delete d;
}

Mod::Tag *S3M::File::tag() const
{
  return &d->tag;
}

PropertyMap S3M::File::properties() const
{
  return d->tag.properties();
}

PropertyMap S3M::File::setProperties(const PropertyMap &properties)
{
  return d->tag.setProperties(properties);
}

S3M::Properties *S3M::File::audioProperties() const
{
  return &d->properties;
}

bool S3M::File::save()
{
  if(readOnly()) {
    debug("S3M::File::save() - Cannot save to a read only file.");
    return false;
  }
  // note: if title starts with "Extended Module: "
  // the file would look like an .xm file
  seek(0);
  writeString(d->tag.title(), 27);
  // string terminating NUL is not optional:
  writeByte(0);

  seek(32);

  ushort length = 0;
  ushort sampleCount = 0;

  if(!readU16L(length) || !readU16L(sampleCount))
    return false;

  seek(28, Current);

  int channels = 0;
  for(int i = 0; i < 32; ++ i) {
    uchar setting = 0;
    if(!readByte(setting))
      return false;
    // or if(setting >= 128)?
    // or channels = i + 1;?
    // need a better spec!
    if(setting != 0xff) ++ channels;
  }

  seek(channels, Current);

  StringList lines = d->tag.comment().split("\n");
  // write comment as sample names:
  for(ushort i = 0; i < sampleCount; ++ i) {
    seek(96L + length + ((long)i << 1));

    ushort instrumentOffset = 0;
    if(!readU16L(instrumentOffset))
      return false;
    seek(((long)instrumentOffset << 4) + 48);

    if(i < lines.size())
      writeString(lines[i], 27);
    else
      writeString(String::null, 27);
    // string terminating NUL is not optional:
    writeByte(0);
  }
  return true;
}

void S3M::File::read(bool)
{
  if(!isOpen())
    return;

  READ_STRING(d->tag.setTitle, 28);
  READ_BYTE_AS(mark);
  READ_BYTE_AS(type);

  READ_ASSERT(mark == 0x1A && type == 0x10);

  seek(32);

  READ_U16L_AS(length);
  READ_U16L_AS(sampleCount);

  d->properties.setSampleCount(sampleCount);

  READ_U16L(d->properties.setPatternCount);
  READ_U16L(d->properties.setFlags);
  READ_U16L(d->properties.setTrackerVersion);
  READ_U16L(d->properties.setFileFormatVersion);

  READ_ASSERT(readBlock(4) == "SCRM");

  READ_BYTE(d->properties.setGlobalVolume);
  READ_BYTE(d->properties.setBpmSpeed);
  READ_BYTE(d->properties.setTempo);

  READ_BYTE_AS(masterVolume);
  d->properties.setMasterVolume(masterVolume & 0x7f);
  d->properties.setStereo((masterVolume & 0x80) != 0);

  // I've seen players who call the next two bytes
  // "ultra click" and "use panning values" (if == 0xFC).
  // I don't see them in any spec, though.
  // Hm, but there is "UltraClick-removal" and some other
  // variables in ScreamTracker IIIs GUI.

  seek(12, Current);

  int channels = 0;
  for(int i = 0; i < 32; ++ i) {
    READ_BYTE_AS(setting);
    // or if(setting >= 128)?
    // or channels = i + 1;?
    // need a better spec!
    if(setting != 0xff) ++ channels;
  }
  d->properties.setChannels(channels);

  seek(96);
  ushort realLength = 0;
  for(ushort i = 0; i < length; ++ i) {
    READ_BYTE_AS(order);
    if(order == 255) break;
    if(order != 254) ++ realLength;
  }
  d->properties.setLengthInPatterns(realLength);

  seek(channels, Current);

  // Note: The S3M spec mentions samples and instruments, but in
  //       the header there are only pointers to instruments.
  //       However, there I never found instruments (SCRI) but
  //       instead samples (SCRS).
  StringList comment;
  for(ushort i = 0; i < sampleCount; ++ i) {
    seek(96L + length + ((long)i << 1));

    READ_U16L_AS(sampleHeaderOffset);
    seek((long)sampleHeaderOffset << 4);

    READ_BYTE_AS(sampleType);
    READ_STRING_AS(dosFileName, 13);
    READ_U16L_AS(sampleDataOffset);
    READ_U32L_AS(sampleLength);
    READ_U32L_AS(repeatStart);
    READ_U32L_AS(repeatStop);
    READ_BYTE_AS(sampleVolume);

    seek(1, Current);

    READ_BYTE_AS(packing);
    READ_BYTE_AS(sampleFlags);
    READ_U32L_AS(baseFrequency);

    seek(12, Current);

    READ_STRING_AS(sampleName, 28);
    // The next 4 bytes should be "SCRS", but I've found
    // files that are otherwise ok with 4 nils instead.
    // READ_ASSERT(readBlock(4) == "SCRS");

    comment.append(sampleName);
  }

  d->tag.setComment(comment.toString("\n"));
  d->tag.setTrackerName("ScreamTracker III");
}
