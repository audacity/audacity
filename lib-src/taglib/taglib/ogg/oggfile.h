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

#include "taglib_export.h"
#include "tfile.h"
#include "tbytevectorlist.h"

#ifndef TAGLIB_OGGFILE_H
#define TAGLIB_OGGFILE_H

namespace TagLib {

  //! A namespace for the classes used by Ogg-based metadata files

  namespace Ogg {

    class PageHeader;

    //! An implementation of TagLib::File with some helpers for Ogg based formats

    /*!
     * This is an implementation of Ogg file page and packet rendering and is of
     * use to Ogg based formats.  While the API is small this handles the
     * non-trivial details of breaking up an Ogg stream into packets and makes
     * these available (via subclassing) to the codec meta data implementations.
     */

    class TAGLIB_EXPORT File : public TagLib::File
    {
    public:
      virtual ~File();

      /*!
       * Returns the packet contents for the i-th packet (starting from zero)
       * in the Ogg bitstream.
       *
       * \warning The requires reading at least the packet header for every page
       * up to the requested page.
       */
      ByteVector packet(uint i);

      /*!
       * Sets the packet with index \a i to the value \a p.
       */
      void setPacket(uint i, const ByteVector &p);

      /*!
       * Returns a pointer to the PageHeader for the first page in the stream or
       * null if the page could not be found.
       */
      const PageHeader *firstPageHeader();

      /*!
       * Returns a pointer to the PageHeader for the last page in the stream or
       * null if the page could not be found.
       */
      const PageHeader *lastPageHeader();

      virtual bool save();

    protected:
      /*!
       * Contructs an Ogg file from \a file.  If \a readProperties is true the
       * file's audio properties will also be read using \a propertiesStyle.  If
       * false, \a propertiesStyle is ignored.
       *
       * \note This constructor is protected since Ogg::File shouldn't be
       * instantiated directly but rather should be used through the codec
       * specific subclasses.
       */
      File(FileName file);

    private:
      File(const File &);
      File &operator=(const File &);

      /*!
       * Reads the next page and updates the internal "current page" pointer.
       */
      bool nextPage();
      void writePageGroup(const List<int> &group);

      class FilePrivate;
      FilePrivate *d;
    };

  }
}

#endif
