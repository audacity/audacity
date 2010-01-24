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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_ID3V2EXTENDEDHEADER_H
#define TAGLIB_ID3V2EXTENDEDHEADER_H

#include "taglib_export.h"
#include "tbytevector.h"
#include "taglib.h"

namespace TagLib {

  namespace ID3v2 {

    //! ID3v2 extended header implementation

    /*!
     * This class implements ID3v2 extended headers.  It attempts to follow,
     * both  semantically and programatically, the structure specified in
     * the ID3v2 standard.  The API is based on the properties of ID3v2 extended
     * headers specified there.  If any of the terms used in this documentation
     * are unclear please check the specification in the linked section.
     * (Structure, <a href="id3v2-structure.html#3.2">3.2</a>)
     */

    class TAGLIB_EXPORT ExtendedHeader
    {
    public:
      /*!
       * Constructs an empty ID3v2 extended header.
       */
      ExtendedHeader();

      /*!
       * Destroys the extended header.
       */
      virtual ~ExtendedHeader();

      /*!
       * Returns the size of the extended header.  This is variable for the
       * extended header.
       */
      uint size() const;

      /*!
       * Sets the data that will be used as the extended header.  Since the
       * length is not known before the extended header has been parsed, this
       * should just be a pointer to the first byte of the extended header.  It
       * will determine the length internally and make that available through
       * size().
       */
      void setData(const ByteVector &data);

    protected:
      /*!
       * Called by setData() to parse the extended header data.  It makes this
       * information available through the public API.
       */
      void parse(const ByteVector &data);

    private:
      ExtendedHeader(const ExtendedHeader &);
      ExtendedHeader &operator=(const ExtendedHeader &);

      class ExtendedHeaderPrivate;
      ExtendedHeaderPrivate *d;
    };

  }
}
#endif
