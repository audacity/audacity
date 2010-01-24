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

#ifndef TAGLIB_MP4ITEM_H
#define TAGLIB_MP4ITEM_H

#include <tstringlist.h>
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
      Item(bool value);
      Item(int first, int second);
      Item(const StringList &value);

      int toInt() const;
      bool toBool() const;
      IntPair toIntPair() const;
      StringList toStringList() const;

      bool isValid() const;

    private:
      class ItemPrivate;
      ItemPrivate *d;
    };

  }

}

#endif
