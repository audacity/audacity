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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef WITH_MP4

#include <taglib.h>
#include <tdebug.h>
#include "mp4item.h"

using namespace TagLib;

class MP4::Item::ItemPrivate : public RefCounter
{
public:
  ItemPrivate() : RefCounter(), valid(true) {}

  bool valid;
  union {
    bool m_bool;
    int m_int;
    IntPair m_intPair;
  };
  StringList m_stringList;
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

MP4::Item::Item(int value1, int value2)
{
  d = new ItemPrivate;
  d->m_intPair.first = value1;
  d->m_intPair.second = value2;
}

MP4::Item::Item(const StringList &value)
{
  d = new ItemPrivate;
  d->m_stringList = value;
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

bool
MP4::Item::isValid() const
{
  return d->valid;
}

#endif
