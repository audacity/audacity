/***************************************************************************
    copyright            : (C) 2002 - 2008 by Scott Wheeler
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

#ifndef TAGLIB_ID3V2HEADER_H
#define TAGLIB_ID3V2HEADER_H

#include "tbytevector.h"
#include "taglib_export.h"

namespace TagLib {

  namespace ID3v2 {

    //! An implementation of ID3v2 headers

    /*!
     * This class implements ID3v2 headers.  It attempts to follow, both
     * semantically and programatically, the structure specified in
     * the ID3v2 standard.  The API is based on the properties of ID3v2 headers
     * specified there.  If any of the terms used in this documentation are
     * unclear please check the specification in the linked section.
     * (Structure, <a href="id3v2-structure.html#3.1">3.1</a>)
     */

    class TAGLIB_EXPORT Header
    {
    public:
      /*!
       * Constructs an empty ID3v2 header.
       */
      Header();

      /*!
       * Constructs an ID3v2 header based on \a data.  parse() is called
       * immediately.
       */
      Header(const ByteVector &data);

      /*!
       * Destroys the header.
       */
      virtual ~Header();

      /*!
       * Returns the major version number.  (Note: This is the 4, not the 2 in
       * ID3v2.4.0.  The 2 is implied.)
       */
      uint majorVersion() const;

      /*!
       * Set the the major version number to \a version.  (Note: This is
       * the 4, not the 2 in ID3v2.4.0.  The 2 is implied.)
       * \see majorVersion()
       *
       * \note This is used by the internal parser; this will not change the
       * version which is written and in general should not be called by API
       * users.
       */
      void setMajorVersion(uint version);

      /*!
       * Returns the revision number.  (Note: This is the 0, not the 4 in
       * ID3v2.4.0.  The 2 is implied.)
       */
      uint revisionNumber() const;

      /*!
       * Returns true if unsynchronisation has been applied to all frames.
       */
      bool unsynchronisation() const;

      /*!
       * Returns true if an extended header is present in the tag.
       */
      bool extendedHeader() const;

      /*!
       * Returns true if the experimental indicator flag is set.
       */
      bool experimentalIndicator() const;

      /*!
       * Returns true if a footer is present in the tag.
       */
      bool footerPresent() const;
      /*!
       * Returns the tag size in bytes.  This is the size of the frame content.
       * The size of the \e entire tag will be this plus the header size (10
       * bytes) and, if present, the footer size (potentially another 10 bytes).
       *
       * \note This is the value as read from the header to which TagLib attempts
       * to provide an API to; it was not a design decision on the part of TagLib
       * to not include the mentioned portions of the tag in the \e size.
       *
       * \see completeTagSize()
       */
      uint tagSize() const;

      /*!
       * Returns the tag size, including the header and, if present, the footer
       * size.
       *
       * \see tagSize()
       */
      uint completeTagSize() const;

      /*!
       * Set the tag size to \a s.
       * \see tagSize()
       */
      void setTagSize(uint s);

      /*!
       * Returns the size of the header.  Presently this is always 10 bytes.
       */
      static uint size();

      /*!
       * Returns the string used to identify and ID3v2 tag inside of a file.
       * Presently this is always "ID3".
       */
      static ByteVector fileIdentifier();

      /*!
       * Sets the data that will be used as the header.  10 bytes, starting from
       * the beginning of \a data are used.
       */
      void setData(const ByteVector &data);

      /*!
       * Renders the Header back to binary format.
       */
      ByteVector render() const;

    protected:
      /*!
       * Called by setData() to parse the header data.  It makes this information
       * available through the public API.
       */
      void parse(const ByteVector &data);

    private:
      Header(const Header &);
      Header &operator=(const Header &);

      class HeaderPrivate;
      HeaderPrivate *d;
    };

  }
}

#endif
