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

#ifndef TAGLIB_ID3V1GENRE_H
#define TAGLIB_ID3V1GENRE_H

#include "tmap.h"
#include "tstringlist.h"
#include "taglib_export.h"

namespace TagLib {
  namespace ID3v1 {

    typedef Map<String, int> GenreMap;

    /*!
     * Returns the list of canonical ID3v1 genre names in the order that they
     * are listed in the standard.
     */
    StringList TAGLIB_EXPORT genreList();

    /*!
     * A "reverse mapping" that goes from the canonical ID3v1 genre name to the
     * respective genre number.   genreMap()["Rock"] ==
     */
    GenreMap genreMap();

    /*!
     * Returns the name of the genre at \a index in the ID3v1 genre list.  If
     * \a index is out of range -- less than zero or greater than 146 -- a null
     * string will be returned.
     */
    String genre(int index);

    /*!
     * Returns the genre index for the (case sensitive) genre \a name.  If the
     * genre is not in the list 255 (which signifies an unknown genre in ID3v1)
     * will be returned.
     */
    int genreIndex(const String &name);
  }
}

#endif
