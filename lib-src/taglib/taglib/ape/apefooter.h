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

#ifndef TAGLIB_APEFOOTER_H
#define TAGLIB_APEFOOTER_H

#include "tbytevector.h"
#include "taglib_export.h"

namespace TagLib {

  namespace APE {

    //! An implementation of APE footers

    /*!
     * This class implements APE footers (and headers). It attempts to follow, both
     * semantically and programatically, the structure specified in
     * the APE v2.0 standard.  The API is based on the properties of APE footer and
     * headers specified there.
     */

    class TAGLIB_EXPORT Footer
    {
    public:
      /*!
       * Constructs an empty APE footer.
       */
      Footer();

      /*!
       * Constructs an APE footer based on \a data.  parse() is called
       * immediately.
       */
      Footer(const ByteVector &data);

      /*!
       * Destroys the footer.
       */
      virtual ~Footer();

      /*!
       * Returns the version number.  (Note: This is the 1000 or 2000.)
       */
      uint version() const;

      /*!
       * Returns true if a header is present in the tag.
       */
      bool headerPresent() const;

      /*!
       * Returns true if a footer is present in the tag.
       */
      bool footerPresent() const;

      /*!
       * Returns true this is actually the header.
       */
      bool isHeader() const;

      /*!
       * Sets whether the header should be rendered or not
       */
      void setHeaderPresent(bool b) const;

      /*!
       * Returns the number of items in the tag.
       */
      uint itemCount() const;

      /*!
       * Set the item count to \a s.
       * \see itemCount()
       */
      void setItemCount(uint s);

      /*!
       * Returns the tag size in bytes.  This is the size of the frame content and footer.
       * The size of the \e entire tag will be this plus the header size, if present.
       *
       * \see completeTagSize()
       */
      uint tagSize() const;

      /*!
       * Returns the tag size, including if present, the header
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
       * Returns the size of the footer.  Presently this is always 32 bytes.
       */
      static uint size();

      /*!
       * Returns the string used to identify an APE tag inside of a file.
       * Presently this is always "APETAGEX".
       */
      static ByteVector fileIdentifier();

      /*!
       * Sets the data that will be used as the footer.  32 bytes,
       * starting from \a data will be used.
       */
      void setData(const ByteVector &data);

      /*!
       * Renders the footer back to binary format.
       */
      ByteVector renderFooter() const;

      /*!
       * Renders the header corresponding to the footer. If headerPresent is
       * set to false, it returns an empty ByteVector.
       */
      ByteVector renderHeader() const;

    protected:
      /*!
       * Called by setData() to parse the footer data.  It makes this information
       * available through the public API.
       */
      void parse(const ByteVector &data);

      /*!
       * Called by renderFooter and renderHeader
       */
      ByteVector render(bool isHeader) const;

    private:
      Footer(const Footer &);
      Footer &operator=(const Footer &);

      class FooterPrivate;
      FooterPrivate *d;
    };

  }
}

#endif
