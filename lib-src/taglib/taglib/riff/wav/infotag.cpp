/***************************************************************************
    copyright            : (C) 2012 by Tsuda Kageyu
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

#include <tdebug.h>
#include <tfile.h>

#include "infotag.h"

using namespace TagLib;
using namespace RIFF::Info;

namespace {
  static bool isValidChunkID(const ByteVector &name)
  {
    if(name.size() != 4)
      return false;

    for(int i = 0; i < 4; i++) {
      if(name[i] < 32 || name[i] > 127)
        return false;
    }

    return true;
  }
}

class RIFF::Info::Tag::TagPrivate
{
public:
  TagPrivate() 
  {}

  FieldListMap fieldListMap;

  static const StringHandler *stringHandler;
};

////////////////////////////////////////////////////////////////////////////////
// StringHandler implementation
////////////////////////////////////////////////////////////////////////////////

StringHandler::StringHandler()
{
}

StringHandler::~StringHandler()
{
}

String RIFF::Info::StringHandler::parse(const ByteVector &data) const
{
  return String(data, String::UTF8);
}

ByteVector RIFF::Info::StringHandler::render(const String &s) const
{
  return s.data(String::UTF8);
}

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

static const StringHandler defaultStringHandler;
const RIFF::Info::StringHandler *RIFF::Info::Tag::TagPrivate::stringHandler = &defaultStringHandler;

RIFF::Info::Tag::Tag(const ByteVector &data) 
  : TagLib::Tag()
  , d(new TagPrivate())
{
  parse(data);
}

RIFF::Info::Tag::Tag() 
  : TagLib::Tag()
  , d(new TagPrivate())
{
}

RIFF::Info::Tag::~Tag()
{
  delete d;
}

String RIFF::Info::Tag::title() const
{
  return fieldText("INAM");
}

String RIFF::Info::Tag::artist() const
{
  return fieldText("IART");
}

String RIFF::Info::Tag::album() const
{
  return fieldText("IPRD");
}

String RIFF::Info::Tag::comment() const
{
  return fieldText("ICMT");
}

String RIFF::Info::Tag::genre() const
{
  return fieldText("IGNR");
}

TagLib::uint RIFF::Info::Tag::year() const
{
  return fieldText("ICRD").substr(0, 4).toInt();
}

TagLib::uint RIFF::Info::Tag::track() const
{
  return fieldText("IPRT").toInt();
}

void RIFF::Info::Tag::setTitle(const String &s)
{
  setFieldText("INAM", s);
}

void RIFF::Info::Tag::setArtist(const String &s)
{
  setFieldText("IART", s);
}

void RIFF::Info::Tag::setAlbum(const String &s)
{
  setFieldText("IPRD", s);
}

void RIFF::Info::Tag::setComment(const String &s)
{
  setFieldText("ICMT", s);
}

void RIFF::Info::Tag::setGenre(const String &s)
{
  setFieldText("IGNR", s);
}

void RIFF::Info::Tag::setYear(uint i)
{
  if(i != 0)
    setFieldText("ICRD", String::number(i));
  else
    d->fieldListMap.erase("ICRD");
}

void RIFF::Info::Tag::setTrack(uint i)
{
  if(i != 0)
    setFieldText("IPRT", String::number(i));
  else
    d->fieldListMap.erase("IPRT");
}

bool RIFF::Info::Tag::isEmpty() const
{
  return d->fieldListMap.isEmpty();
}

FieldListMap RIFF::Info::Tag::fieldListMap() const
{
  return d->fieldListMap;
}

String RIFF::Info::Tag::fieldText(const ByteVector &id) const
{
  if(d->fieldListMap.contains(id))
    return String(d->fieldListMap[id]);
  else
    return String();
}

void RIFF::Info::Tag::setFieldText(const ByteVector &id, const String &s)
{
  // id must be four-byte long pure ascii string.
  if(!isValidChunkID(id))
    return;

  if(!s.isEmpty())
    d->fieldListMap[id] = s;
  else
    removeField(id);
}

void RIFF::Info::Tag::removeField(const ByteVector &id)
{
  if(d->fieldListMap.contains(id))
    d->fieldListMap.erase(id);
}

ByteVector RIFF::Info::Tag::render() const
{
  ByteVector data("INFO");

  FieldListMap::ConstIterator it = d->fieldListMap.begin();
  for(; it != d->fieldListMap.end(); ++it) {
    ByteVector text = TagPrivate::stringHandler->render(it->second);
    if(text.isEmpty())
      continue;

    data.append(it->first);
    data.append(ByteVector::fromUInt(text.size() + 1, false));
    data.append(text);
    
    do {
      data.append('\0');
    } while(data.size() & 1);
  }

  if(data.size() == 4)
    return ByteVector();
  else
    return data;
}

void RIFF::Info::Tag::setStringHandler(const StringHandler *handler)
{
  if(handler)
    TagPrivate::stringHandler = handler;
  else
    TagPrivate::stringHandler = &defaultStringHandler;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

void RIFF::Info::Tag::parse(const ByteVector &data)
{
  uint p = 4;
  while(p < data.size()) {
    const uint size = data.toUInt(p + 4, false);
    d->fieldListMap[data.mid(p, 4)] = TagPrivate::stringHandler->parse(data.mid(p + 8, size));

    p += ((size + 1) & ~1) + 8;
  }
}

