/**************************************************************************
    copyright            : (C) 2007 by Lukáš Lalinský
    email                : lalinsky@gmail.com
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
#include "mp4item.h"

using namespace TagLib;

class MP4::Item::ItemPrivate : public RefCounter
{
public:
  ItemPrivate() : RefCounter(), valid(true), atomDataType(TypeUndefined) {}

  bool valid;
  AtomDataType atomDataType;
  union {
    bool m_bool;
    int m_int;
    IntPair m_intPair;
    uchar m_byte;
    uint m_uint;
    long long m_longlong;
  };
  StringList m_stringList;
  ByteVectorList m_byteVectorList;
  MP4::CoverArtList m_coverArtList;
};

MP4::Item::Item()
{
  d = new ItemPrivate;
  d->valid = false;
}

MP4::Item::Item(const Item &item) : d(item.d)
{
  d->ref();
}

MP4::Item &
MP4::Item::operator=(const Item &item)
{
  if(d->deref()) {
    delete d;
  }
  d = item.d;
  d->ref();
  return *this;
}

MP4::Item::~Item()
{
  if(d->deref()) {
    delete d;
  }
}

MP4::Item::Item(bool value)
{
  d = new ItemPrivate;
  d->m_bool = value;
}

MP4::Item::Item(int value)
{
  d = new ItemPrivate;
  d->m_int = value;
}

MP4::Item::Item(uchar value)
{
  d = new ItemPrivate;
  d->m_byte = value;
}

MP4::Item::Item(uint value)
{
  d = new ItemPrivate;
  d->m_uint = value;
}

MP4::Item::Item(long long value)
{
  d = new ItemPrivate;
  d->m_longlong = value;
}

MP4::Item::Item(int value1, int value2)
{
  d = new ItemPrivate;
  d->m_intPair.first = value1;
  d->m_intPair.second = value2;
}

MP4::Item::Item(const ByteVectorList &value)
{
  d = new ItemPrivate;
  d->m_byteVectorList = value;
}

MP4::Item::Item(const StringList &value)
{
  d = new ItemPrivate;
  d->m_stringList = value;
}

MP4::Item::Item(const MP4::CoverArtList &value)
{
  d = new ItemPrivate;
  d->m_coverArtList = value;
}

void MP4::Item::setAtomDataType(MP4::AtomDataType type)
{
  d->atomDataType = type;
}

MP4::AtomDataType MP4::Item::atomDataType() const
{
  return d->atomDataType;
}

bool
MP4::Item::toBool() const
{
  return d->m_bool;
}

int
MP4::Item::toInt() const
{
  return d->m_int;
}

uchar
MP4::Item::toByte() const
{
  return d->m_byte;
}

TagLib::uint
MP4::Item::toUInt() const
{
  return d->m_uint;
}

long long
MP4::Item::toLongLong() const
{
  return d->m_longlong;
}

MP4::Item::IntPair
MP4::Item::toIntPair() const
{
  return d->m_intPair;
}

StringList
MP4::Item::toStringList() const
{
  return d->m_stringList;
}

ByteVectorList
MP4::Item::toByteVectorList() const
{
  return d->m_byteVectorList;
}

MP4::CoverArtList
MP4::Item::toCoverArtList() const
{
  return d->m_coverArtList;
}

bool
MP4::Item::isValid() const
{
  return d->valid;
}

