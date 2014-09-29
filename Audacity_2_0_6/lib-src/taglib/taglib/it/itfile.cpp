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
#include "itfile.h"
#include "tdebug.h"
#include "modfileprivate.h"
#include "tpropertymap.h"

using namespace TagLib;
using namespace IT;

class IT::File::FilePrivate
{
public:
  FilePrivate(AudioProperties::ReadStyle propertiesStyle)
    : tag(), properties(propertiesStyle)
  {
  }

  Mod::Tag       tag;
  IT::Properties properties;
};

IT::File::File(FileName file, bool readProperties,
               AudioProperties::ReadStyle propertiesStyle) :
  Mod::FileBase(file),
  d(new FilePrivate(propertiesStyle))
{
  if(isOpen())
    read(readProperties);
}

IT::File::File(IOStream *stream, bool readProperties,
               AudioProperties::ReadStyle propertiesStyle) :
  Mod::FileBase(stream),
  d(new FilePrivate(propertiesStyle))
{
  if(isOpen())
    read(readProperties);
}

IT::File::~File()
{
  delete d;
}

Mod::Tag *IT::File::tag() const
{
  return &d->tag;
}

PropertyMap IT::File::properties() const
{
  return d->tag.properties();
}

PropertyMap IT::File::setProperties(const PropertyMap &properties)
{
  return d->tag.setProperties(properties);
}

IT::Properties *IT::File::audioProperties() const
{
  return &d->properties;
}

bool IT::File::save()
{
  if(readOnly())
  {
    debug("IT::File::save() - Cannot save to a read only file.");
    return false;
  }
  seek(4);
  writeString(d->tag.title(), 25);
  writeByte(0);

  seek(2, Current);

  ushort length = 0;
  ushort instrumentCount = 0;
  ushort sampleCount = 0;

  if(!readU16L(length) || !readU16L(instrumentCount) || !readU16L(sampleCount))
    return false;

  seek(15, Current);

  // write comment as instrument and sample names:
  StringList lines = d->tag.comment().split("\n");
  for(ushort i = 0; i < instrumentCount; ++ i) {
    seek(192L + length + ((long)i << 2));
    ulong instrumentOffset = 0;
    if(!readU32L(instrumentOffset))
      return false;

    seek(instrumentOffset + 32);

    if(i < lines.size())
      writeString(lines[i], 25);
    else
      writeString(String::null, 25);
    writeByte(0);
  }

  for(ushort i = 0; i < sampleCount; ++ i) {
    seek(192L + length + ((long)instrumentCount << 2) + ((long)i << 2));
    ulong sampleOffset = 0;
    if(!readU32L(sampleOffset))
      return false;

    seek(sampleOffset + 20);

    if((TagLib::uint)(i + instrumentCount) < lines.size())
      writeString(lines[i + instrumentCount], 25);
    else
      writeString(String::null, 25);
    writeByte(0);
  }

  // write rest as message:
  StringList messageLines;
  for(uint i = instrumentCount + sampleCount; i < lines.size(); ++ i)
    messageLines.append(lines[i]);
  ByteVector message = messageLines.toString("\r").data(String::Latin1);

  // it's actually not really stated if the message needs a
  // terminating NUL but it does not hurt to add one:
  if(message.size() > 7999)
    message.resize(7999);
  message.append((char)0);

  ushort special = 0;
  ushort messageLength = 0;
  ulong  messageOffset = 0;

  seek(46);
  if(!readU16L(special))
    return false;

  ulong fileSize = File::length();
  if(special & Properties::MessageAttached) {
    seek(54);
    if(!readU16L(messageLength) || !readU32L(messageOffset))
      return false;

    if(messageLength == 0)
      messageOffset = fileSize;
  }
  else
  {
    messageOffset = fileSize;
    seek(46);
    writeU16L(special | 0x1);
  }

  if(messageOffset + messageLength >= fileSize) {
    // append new message
    seek(54);
    writeU16L(message.size());
    writeU32L(messageOffset);
    seek(messageOffset);
    writeBlock(message);
    truncate(messageOffset + message.size());
  }
  else {
    // Only overwrite existing message.
    // I'd need to parse (understand!) the whole file for more.
    // Although I could just move the message to the end of file
    // and let the existing one be, but that would waste space.
    message.resize(messageLength, 0);
    seek(messageOffset);
    writeBlock(message);
  }
  return true;
}

void IT::File::read(bool)
{
  if(!isOpen())
    return;

  seek(0);
  READ_ASSERT(readBlock(4) == "IMPM");
  READ_STRING(d->tag.setTitle, 26);

  seek(2, Current);

  READ_U16L_AS(length);
  READ_U16L_AS(instrumentCount);
  READ_U16L_AS(sampleCount);

  d->properties.setInstrumentCount(instrumentCount);
  d->properties.setSampleCount(sampleCount);
  READ_U16L(d->properties.setPatternCount);
  READ_U16L(d->properties.setVersion);
  READ_U16L(d->properties.setCompatibleVersion);
  READ_U16L(d->properties.setFlags);
  READ_U16L_AS(special);
  d->properties.setSpecial(special);
  READ_BYTE(d->properties.setGlobalVolume);
  READ_BYTE(d->properties.setMixVolume);
  READ_BYTE(d->properties.setBpmSpeed);
  READ_BYTE(d->properties.setTempo);
  READ_BYTE(d->properties.setPanningSeparation);
  READ_BYTE(d->properties.setPitchWheelDepth);

  // IT supports some kind of comment tag. Still, the
  // sample/instrument names are abused as comments so
  // I just add all together.
  String message;
  if(special & Properties::MessageAttached) {
    READ_U16L_AS(messageLength);
    READ_U32L_AS(messageOffset);
    seek(messageOffset);
    ByteVector messageBytes = readBlock(messageLength);
    READ_ASSERT(messageBytes.size() == messageLength);
    int index = messageBytes.find((char) 0);
    if(index > -1)
      messageBytes.resize(index, 0);
    messageBytes.replace('\r', '\n');
    message = messageBytes;
  }

  seek(64);

  ByteVector pannings = readBlock(64);
  ByteVector volumes  = readBlock(64);
  READ_ASSERT(pannings.size() == 64 && volumes.size() == 64);
  int channels = 0;
  for(int i = 0; i < 64; ++ i) {
    // Strictly speaking an IT file has always 64 channels, but
    // I don't count disabled and muted channels.
    // But this always gives 64 channels for all my files anyway.
    // Strangely VLC does report other values. I wonder how VLC
    // gets it's values.
    if((unsigned char) pannings[i] < 128 && volumes[i] > 0)
        ++channels;
  }
  d->properties.setChannels(channels);

  // real length might be shorter because of skips and terminator
  ushort realLength = 0;
  for(ushort i = 0; i < length; ++ i) {
    READ_BYTE_AS(order);
    if(order == 255) break;
    if(order != 254) ++ realLength;
  }
  d->properties.setLengthInPatterns(realLength);

  StringList comment;
  // Note: I found files that have nil characters somewhere
  //       in the instrument/sample names and more characters
  //       afterwards. The spec does not mention such a case.
  //       Currently I just discard anything after a nil, but
  //       e.g. VLC seems to interprete a nil as a space. I
  //       don't know what is the proper behaviour.
  for(ushort i = 0; i < instrumentCount; ++ i) {
    seek(192L + length + ((long)i << 2));
    READ_U32L_AS(instrumentOffset);
    seek(instrumentOffset);

    ByteVector instrumentMagic = readBlock(4);
    READ_ASSERT(instrumentMagic == "IMPI");

    READ_STRING_AS(dosFileName, 13);

    seek(15, Current);

    READ_STRING_AS(instrumentName, 26);
    comment.append(instrumentName);
  }

  for(ushort i = 0; i < sampleCount; ++ i) {
    seek(192L + length + ((long)instrumentCount << 2) + ((long)i << 2));
    READ_U32L_AS(sampleOffset);

    seek(sampleOffset);

    ByteVector sampleMagic = readBlock(4);
    READ_ASSERT(sampleMagic == "IMPS");

    READ_STRING_AS(dosFileName, 13);
    READ_BYTE_AS(globalVolume);
    READ_BYTE_AS(sampleFlags);
    READ_BYTE_AS(sampleVolume);
    READ_STRING_AS(sampleName, 26);
    /*
    READ_BYTE_AS(sampleCvt);
    READ_BYTE_AS(samplePanning);
    READ_U32L_AS(sampleLength);
    READ_U32L_AS(loopStart);
    READ_U32L_AS(loopStop);
    READ_U32L_AS(c5speed);
    READ_U32L_AS(sustainLoopStart);
    READ_U32L_AS(sustainLoopEnd);
    READ_U32L_AS(sampleDataOffset);
    READ_BYTE_AS(vibratoSpeed);
    READ_BYTE_AS(vibratoDepth);
    READ_BYTE_AS(vibratoRate);
    READ_BYTE_AS(vibratoType);
    */

    comment.append(sampleName);
  }

  if(message.size() > 0)
    comment.append(message);
  d->tag.setComment(comment.toString("\n"));
  d->tag.setTrackerName("Impulse Tracker");
}
