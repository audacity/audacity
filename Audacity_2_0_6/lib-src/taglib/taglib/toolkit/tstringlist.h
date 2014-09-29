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

#ifndef TAGLIB_STRINGLIST_H
#define TAGLIB_STRINGLIST_H

#include "tstring.h"
#include "tlist.h"
#include "tbytevectorlist.h"
#include "taglib_export.h"

#include <iostream>

namespace TagLib {

  //! A list of strings

  /*!
   * This is a spcialization of the List class with some members convention for
   * string operations.
   */

  class TAGLIB_EXPORT StringList : public List<String>
  {
  public:

    /*!
     * Constructs an empty StringList.
     */
    StringList();

    /*!
     * Make a shallow, implicitly shared, copy of \a l.  Because this is
     * implicitly shared, this method is lightweight and suitable for
     * pass-by-value usage.
     */
    StringList(const StringList &l);

    /*!
     * Constructs a StringList with \a s as a member.
     */
    StringList(const String &s);

    /*!
     * Makes a deep copy of the data in \a vl.
     *
     * \note This should only be used with the 8-bit codecs Latin1 and UTF8, when
     * used with other codecs it will simply print a warning and exit.
     */
    StringList(const ByteVectorList &vl, String::Type t = String::Latin1);

    /*!
     * Destroys this StringList instance.
     */
    virtual ~StringList();

    /*!
     * Concatenate the list of strings into one string separated by \a separator.
     */
    String toString(const String &separator = " ") const;

    /*!
     * Appends \a s to the end of the list and returns a reference to the
     * list.
     */
    StringList &append(const String &s);

    /*!
     * Appends all of the values in \a l to the end of the list and returns a
     * reference to the list.
     */
    StringList &append(const StringList &l);

    /*!
     * Splits the String \a s into several strings at \a pattern.  This will not include
     * the pattern in the returned strings.
     */
    static StringList split(const String &s, const String &pattern);

  private:
    class StringListPrivate;
    StringListPrivate *d;
  };

}

/*!
 * \related TagLib::StringList
 * Send the StringList to an output stream.
 */
std::ostream &operator<<(std::ostream &s, const TagLib::StringList &l);

#endif
