/**************************************************************************
    copyright            : (C) 2010 by Anton Sergunov
    email                : setosha@gmail.com
 **************************************************************************/

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

#include <taglib.h>
#include <tdebug.h>
#include "trefcounter.h"
#include "asfattribute.h"
#include "asffile.h"
#include "asfpicture.h"

using namespace TagLib;

class ASF::Picture::PicturePrivate : public RefCounter
{
public:
  bool valid;
  Type type;
  String mimeType;
  String description;
  ByteVector picture;
};

////////////////////////////////////////////////////////////////////////////////
// Picture class members
////////////////////////////////////////////////////////////////////////////////

ASF::Picture::Picture()
{
  d = new PicturePrivate();
  d->valid = true;
}

ASF::Picture::Picture(const Picture& other)
  : d(other.d)
{
  d->ref();
}

ASF::Picture::~Picture()
{
  if(d->deref())
    delete d;
}

bool ASF::Picture::isValid() const
{
  return d->valid;
}

String ASF::Picture::mimeType() const
{
  return d->mimeType;
}

void ASF::Picture::setMimeType(const String &value)
{
  d->mimeType = value;
}

ASF::Picture::Type ASF::Picture::type() const
{
  return d->type;
}

void ASF::Picture::setType(const ASF::Picture::Type& t)
{
  d->type = t;
}

String ASF::Picture::description() const
{
  return d->description;
}

void ASF::Picture::setDescription(const String &desc)
{
  d->description = desc;
}

ByteVector ASF::Picture::picture() const
{
  return d->picture;
}

void ASF::Picture::setPicture(const ByteVector &p)
{
  d->picture = p;
}

int ASF::Picture::dataSize() const
{
  return
    9 + (d->mimeType.length() + d->description.length()) * 2 +
    d->picture.size();
}

ASF::Picture& ASF::Picture::operator=(const ASF::Picture& other)
{
  if(other.d != d) {
    if(d->deref())
      delete d;
    d = other.d;
    d->ref();
  }
  return *this;
}

ByteVector ASF::Picture::render() const
{
  if(!isValid())
    return ByteVector::null;
  return
    ByteVector((char)d->type) +
    ByteVector::fromUInt(d->picture.size(), false) +
    ASF::File::renderString(d->mimeType) +
    ASF::File::renderString(d->description) +
    d->picture;
}

void ASF::Picture::parse(const ByteVector& bytes)
{
  d->valid = false;
  if(bytes.size() < 9)
    return;
  int pos = 0;
  d->type = (Type)bytes[0]; ++pos;
  const uint dataLen = bytes.toUInt(pos, false); pos+=4;

  const ByteVector nullStringTerminator(2, 0);

  int endPos = bytes.find(nullStringTerminator, pos, 2);
  if(endPos < 0)
    return;
  d->mimeType = String(bytes.mid(pos, endPos - pos), String::UTF16LE);
  pos = endPos+2;

  endPos = bytes.find(nullStringTerminator, pos, 2);
  if(endPos < 0)
    return;
  d->description = String(bytes.mid(pos, endPos - pos), String::UTF16LE);
  pos = endPos+2;

  if(dataLen + pos != bytes.size())
    return;

  d->picture = bytes.mid(pos, dataLen);
  d->valid = true;
  return;
}

ASF::Picture ASF::Picture::fromInvalid()
{
  Picture ret;
  ret.d->valid = false;
  return ret;
}

