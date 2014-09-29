/**************************************************************************
    copyright            : (C) 2007,2011 by Lukáš Lalinský
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

// This file is not part of the public API!

#ifndef DO_NOT_DOCUMENT

#ifndef TAGLIB_MP4ATOM_H
#define TAGLIB_MP4ATOM_H

#include "tfile.h"
#include "tlist.h"

namespace TagLib {

  namespace MP4 {

    class Atom;
    typedef TagLib::List<Atom *> AtomList;

    enum AtomDataType
    {
      TypeImplicit  = 0,  // for use with tags for which no type needs to be indicated because only one type is allowed
      TypeUTF8      = 1,  // without any count or null terminator
      TypeUTF16     = 2,  // also known as UTF-16BE
      TypeSJIS      = 3,  // deprecated unless it is needed for special Japanese characters
      TypeHTML      = 6,  // the HTML file header specifies which HTML version
      TypeXML       = 7,  // the XML header must identify the DTD or schemas
      TypeUUID      = 8,  // also known as GUID; stored as 16 bytes in binary (valid as an ID)
      TypeISRC      = 9,  // stored as UTF-8 text (valid as an ID)
      TypeMI3P      = 10, // stored as UTF-8 text (valid as an ID)
      TypeGIF       = 12, // (deprecated) a GIF image
      TypeJPEG      = 13, // a JPEG image
      TypePNG       = 14, // a PNG image
      TypeURL       = 15, // absolute, in UTF-8 characters
      TypeDuration  = 16, // in milliseconds, 32-bit integer
      TypeDateTime  = 17, // in UTC, counting seconds since midnight, January 1, 1904; 32 or 64-bits
      TypeGenred    = 18, // a list of enumerated values
      TypeInteger   = 21, // a signed big-endian integer with length one of { 1,2,3,4,8 } bytes
      TypeRIAAPA    = 24, // RIAA parental advisory; { -1=no, 1=yes, 0=unspecified }, 8-bit ingteger
      TypeUPC       = 25, // Universal Product Code, in text UTF-8 format (valid as an ID)
      TypeBMP       = 27, // Windows bitmap image
      TypeUndefined = 255 // undefined
    };

    struct AtomData {
      AtomData(AtomDataType type, ByteVector data) : type(type), locale(0), data(data) {}
      AtomDataType type;
      int locale;
      ByteVector data;
    };

    typedef TagLib::List<AtomData> AtomDataList;

    class Atom
    {
    public:
        Atom(File *file);
        ~Atom();
        Atom *find(const char *name1, const char *name2 = 0, const char *name3 = 0, const char *name4 = 0);
        bool path(AtomList &path, const char *name1, const char *name2 = 0, const char *name3 = 0);
        AtomList findall(const char *name, bool recursive = false);
        long offset;
        long length;
        TagLib::ByteVector name;
        AtomList children;
    private:
        static const int numContainers = 11;
        static const char *containers[11];
    };

    //! Root-level atoms
    class Atoms
    {
    public:
        Atoms(File *file);
        ~Atoms();
        Atom *find(const char *name1, const char *name2 = 0, const char *name3 = 0, const char *name4 = 0);
        AtomList path(const char *name1, const char *name2 = 0, const char *name3 = 0, const char *name4 = 0);
        AtomList atoms;
    };

  }

}

#endif

#endif
