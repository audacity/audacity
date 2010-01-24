/***************************************************************************
    copyright            : (C) 2004 by Allan Sandfeld Jensen
    email                : kde@carewolf.org
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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_APEITEM_H
#define TAGLIB_APEITEM_H

#include "tbytevector.h"
#include "tstring.h"
#include "tstringlist.h"

namespace TagLib {

  namespace APE {

    //! An implementation of APE-items

    /*!
     * This class provides the features of items in the APEv2 standard.
     */
    class TAGLIB_EXPORT Item
    {
    public:
      /*!
       * Enum of types an Item can have. The value of 3 is reserved.
       */
      enum ItemTypes {
        //! Item contains text information coded in UTF-8
        Text = 0,
        //! Item contains binary information
        Binary = 1,
        //! Item is a locator of external stored information
        Locator = 2
      };
      /*!
       * Constructs an empty item.
       */
      Item();

      /*!
       * Constructs an item with \a key and \a value.
       */
      // BIC: Remove this, StringList has a constructor from a single string
      Item(const String &key, const String &value);

      /*!
       * Constructs an item with \a key and \a values.
       */
      Item(const String &key, const StringList &values);

      /*!
       * Construct an item as a copy of \a item.
       */
      Item(const Item &item);

      /*!
       * Destroys the item.
       */
      virtual ~Item();

      /*!
       * Copies the contents of \a item into this item.
       */
      Item &operator=(const Item &item);

      /*!
       * Returns the key.
       */
      String key() const;

      /*!
       * Returns the binary value.
       *
       * \deprecated This will be removed in the next binary incompatible version
       * as it is not kept in sync with the things that are set using setValue()
       * and friends.
       */
      ByteVector value() const;

      /*!
       * Sets the key for the item to \a key.
       */
      void setKey(const String &key);

      /*!
       * Sets the value of the item to \a value and clears any previous contents.
       *
       * \see toString()
       */
      void setValue(const String &value);

      /*!
       * Sets the value of the item to the list of values in \a value and clears
       * any previous contents.
       *
       * \see toStringList()
       */
      void setValues(const StringList &values);

      /*!
       * Appends \a value to create (or extend) the current list of values.
       *
       * \see toString()
       */
      void appendValue(const String &value);

      /*!
       * Appends \a values to extend the current list of values.
       *
       * \see toStringList()
       */
      void appendValues(const StringList &values);

      /*!
       * Returns the size of the full item.
       */
      int size() const;

      /*!
       * Returns the value as a single string. In case of multiple strings,
       * the first is returned.
       */
      String toString() const;

      /*!
       * \deprecated
       * \see values
       */
      StringList toStringList() const;

      /*!
       * Returns the list of values.
       */
      StringList values() const;

      /*!
       * Render the item to a ByteVector.
       */
      ByteVector render() const;

      /*!
       * Parse the item from the ByteVector \a data.
       */
      void parse(const ByteVector& data);

      /*!
       * Set the item to read-only.
       */
      void setReadOnly(bool readOnly);

      /*!
       * Return true if the item is read-only.
       */
      bool isReadOnly() const;

      /*!
       * Sets the type of the item to \a type.
       *
       * \see ItemTypes
       */
      void setType(ItemTypes type);

      /*!
       * Returns the type of the item.
       */
      ItemTypes type() const;

      /*!
       * Returns if the item has any real content.
       */
      bool isEmpty() const;

    private:
      class ItemPrivate;
      ItemPrivate *d;
    };
  }

}

#endif


