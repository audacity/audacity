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

#ifndef TAGLIB_ID3V2FOOTER_H
#define TAGLIB_ID3V2FOOTER_H

#include "taglib_export.h"
#include "tbytevector.h"

namespace TagLib {

  namespace ID3v2 {

    class Header;

    //! ID3v2 footer implementation

    /*!
     * Per the ID3v2 specification, the tag's footer is just a copy of the
     * information in the header.  As such there is no API for reading the
     * data from the header, it can just as easily be done from the header.
     *
     * In fact, at this point, TagLib does not even parse the footer since
     * it is not useful internally.  However, if the flag to include a footer
     * has been set in the ID3v2::Tag, TagLib will render a footer.
     */

    class TAGLIB_EXPORT Footer
    {
    public:
      /*!
       * Constructs an empty ID3v2 footer.
       */
      Footer();
      /*!
       * Destroys the footer.
       */
      virtual ~Footer();

      /*!
       * Returns the size of the footer.  Presently this is always 10 bytes.
       */
      static uint size();

      /*!
       * Renders the footer based on the data in \a header.
       */
      ByteVector render(const Header *header) const;

    private:
      Footer(const Footer &);
      Footer &operator=(const Footer &);

      class FooterPrivate;
      FooterPrivate *d;
    };

  }
}
#endif
