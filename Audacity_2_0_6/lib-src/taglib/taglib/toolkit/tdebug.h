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

#ifndef TAGLIB_DEBUG_H
#define TAGLIB_DEBUG_H

namespace TagLib {

  class String;
  class ByteVector;

#ifndef DO_NOT_DOCUMENT

  /*!
   * A simple function that outputs the debug messages to the listener. 
   * The default listener redirects the messages to \a stderr when NDEBUG is 
   * not defined.
   *
   * \warning Do not use this outside of TagLib, it could lead to undefined
   * symbols in your build if TagLib is built with NDEBUG defined and your
   * application is not.
   *
   * \internal
   */
  void debug(const String &s);
  
  /*!
   * For debugging binary data.
   *
   * \warning Do not use this outside of TagLib, it could lead to undefined
   * symbols in your build if TagLib is built with NDEBUG defined and your
   * application is not.
   *
   * \internal
   */
  void debugData(const ByteVector &v);
}

#endif
#endif
