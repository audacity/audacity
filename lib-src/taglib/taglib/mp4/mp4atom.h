/**************************************************************************
    copyright            : (C) 2007 by Lukáš Lalinský
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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

// This file is not part of the public API!

#ifndef DO_NOT_DOCUMENT

#ifndef TAGLIB_MP4ATOM_H
#define TAGLIB_MP4ATOM_H

#include <tfile.h>
#include <tlist.h>

namespace TagLib {

  namespace MP4 {

    class Atom;
    typedef TagLib::List<Atom *> AtomList;

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
        static const int numContainers = 10;
        static const char *containers[10];
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
