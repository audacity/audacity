/**************************************************************************
    copyright            : (C) 2005-2007 by Lukáš Lalinský
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

#ifndef TAGLIB_ASFATTRIBUTE_H
#define TAGLIB_ASFATTRIBUTE_H

#include "tstring.h"
#include "tbytevector.h"
#include "taglib_export.h"
#include "asfpicture.h"

namespace TagLib
{

  namespace ASF
  {

    class File;
    class Picture;

    class TAGLIB_EXPORT Attribute
    {
    public:

      /*!
       * Enum of types an Attribute can have.
       */
      enum AttributeTypes {
        UnicodeType = 0,
        BytesType   = 1,
        BoolType    = 2,
        DWordType   = 3,
        QWordType   = 4,
        WordType    = 5,
        GuidType    = 6
      };

      /*!
       * Constructs an empty attribute.
       */
      Attribute();

      /*!
       * Constructs an attribute with \a key and a UnicodeType \a value.
       */
      Attribute(const String &value);

      /*!
       * Constructs an attribute with \a key and a BytesType \a value.
       */
      Attribute(const ByteVector &value);

      /*!
       * Constructs an attribute with \a key and a Picture \a value.
       *
       * This attribute is compatible with the ID3 frame, APIC. The ID3 specification for the APIC frame stipulates that,
       * while there may be any number of APIC frames associated with a file,
       * only one may be of type 1 and only one may be of type 2.
       *
       * The specification also states that the description of the picture can be no longer than 64 characters, but can be empty.
       * WM/Picture attributes added with TagLib::ASF are not automatically validated to conform to ID3 specifications.
       * You must add code in your application to perform validations if you want to maintain complete compatibility with ID3.
       */
      Attribute(const Picture &value);

      /*!
       * Constructs an attribute with \a key and a DWordType \a value.
       */
      Attribute(unsigned int value);

      /*!
       * Constructs an attribute with \a key and a QWordType \a value.
       */
      Attribute(unsigned long long value);

      /*!
       * Constructs an attribute with \a key and a WordType \a value.
       */
      Attribute(unsigned short value);

      /*!
       * Constructs an attribute with \a key and a BoolType \a value.
       */
      Attribute(bool value);

      /*!
       * Construct an attribute as a copy of \a other.
       */
      Attribute(const Attribute &item);

      /*!
       * Copies the contents of \a other into this item.
       */
      ASF::Attribute &operator=(const Attribute &other);

      /*!
       * Destroys the attribute.
       */
      virtual ~Attribute();

      /*!
       * Returns type of the value.
       */
      AttributeTypes type() const;

      /*!
       * Returns the BoolType \a value.
       */
      unsigned short toBool() const;

      /*!
       * Returns the WordType \a value.
       */
      unsigned short toUShort() const;

      /*!
       * Returns the DWordType \a value.
       */
      unsigned int toUInt() const;

      /*!
       * Returns the QWordType \a value.
       */
      unsigned long long toULongLong() const;

      /*!
       * Returns the UnicodeType \a value.
       */
      String toString() const;

      /*!
       * Returns the BytesType \a value.
       */
      ByteVector toByteVector() const;

      /*!
       * Returns the Picture \a value.
       */
      Picture toPicture() const;

      /*!
       * Returns the language number, or 0 is no stream number was set.
       */
      int language() const;

      /*!
       * Sets the language number.
       */
      void setLanguage(int value);

      /*!
       * Returns the stream number, or 0 is no stream number was set.
       */
      int stream() const;

      /*!
       * Sets the stream number.
       */
      void setStream(int value);

#ifndef DO_NOT_DOCUMENT
      /* THIS IS PRIVATE, DON'T TOUCH IT! */
      String parse(ASF::File &file, int kind = 0);
#endif

      //! Returns the size of the stored data
      int dataSize() const;

    private:
      friend class File;

      ByteVector render(const String &name, int kind = 0) const;

      class AttributePrivate;
      AttributePrivate *d;
    };
  }

}

#endif
