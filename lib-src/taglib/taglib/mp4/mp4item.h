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

#ifndef TAGLIB_MP4ITEM_H
#define TAGLIB_MP4ITEM_H

#include "tstringlist.h"
#include "mp4coverart.h"
#include "taglib_export.h"

namespace TagLib {

  namespace MP4 {

    class TAGLIB_EXPORT Item
    {
    public:
      struct IntPair {
        int first, second;
      };

      Item();
      Item(const Item &item);
      Item &operator=(const Item &item);
      ~Item();

      Item(int value);
      Item(uchar value);
      Item(uint value);
      Item(long long value);
      Item(bool value);
      Item(int first, int second);
      Item(const StringList &value);
      Item(const ByteVectorList &value);
      Item(const CoverArtList &value);

      void setAtomDataType(AtomDataType type);
      AtomDataType atomDataType() const;

      int toInt() const;
      uchar toByte() const;
      uint toUInt() const;
      long long toLongLong() const;
      bool toBool() const;
      IntPair toIntPair() const;
      StringList toStringList() const;
      ByteVectorList toByteVectorList() const;
      CoverArtList toCoverArtList() const;

      bool isValid() const;

    private:
      class ItemPrivate;
      ItemPrivate *d;
    };

  }

}

#endif
