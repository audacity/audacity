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

#ifndef TAGLIB_MP4TAG_H
#define TAGLIB_MP4TAG_H

#include <tag.h>
#include <tbytevectorlist.h>
#include <tfile.h>
#include <tmap.h>
#include <tstringlist.h>
#include "taglib_export.h"
#include "mp4atom.h"
#include "mp4item.h"

namespace TagLib {

  namespace MP4 {

    typedef TagLib::Map<String, Item> ItemListMap;

    class TAGLIB_EXPORT Tag: public TagLib::Tag
    {
    public:
        Tag(TagLib::File *file, Atoms *atoms);
        bool save();

        String title() const;
        String artist() const;
        String album() const;
        String comment() const;
        String genre() const;
        uint year() const;
        uint track() const;

        void setTitle(const String &value);
        void setArtist(const String &value);
        void setAlbum(const String &value);
        void setComment(const String &value);
        void setGenre(const String &value);
        void setYear(uint value);
        void setTrack(uint value);

        ItemListMap &itemListMap();

    private:
        TagLib::ByteVectorList parseData(Atom *atom, TagLib::File *file, int expectedFlags = -1, bool freeForm = false);
        void parseText(Atom *atom, TagLib::File *file, int expectedFlags = 1);
        void parseFreeForm(Atom *atom, TagLib::File *file);
        void parseInt(Atom *atom, TagLib::File *file);
        void parseIntPair(Atom *atom, TagLib::File *file);
        void parseBool(Atom *atom, TagLib::File *file);

        TagLib::ByteVector padIlst(const ByteVector &data, int length = -1);
        TagLib::ByteVector renderAtom(const ByteVector &name, const TagLib::ByteVector &data);
        TagLib::ByteVector renderData(const ByteVector &name, int flags, const TagLib::ByteVectorList &data);
        TagLib::ByteVector renderText(const ByteVector &name, Item &item, int flags = 1);
        TagLib::ByteVector renderFreeForm(const String &name, Item &item);
        TagLib::ByteVector renderBool(const ByteVector &name, Item &item);
        TagLib::ByteVector renderInt(const ByteVector &name, Item &item);
        TagLib::ByteVector renderIntPair(const ByteVector &name, Item &item);
        TagLib::ByteVector renderIntPairNoTrailing(const ByteVector &name, Item &item);

        void updateParents(AtomList &path, long delta, int ignore = 0);
        void updateOffsets(long delta, long offset);

        void saveNew(TagLib::ByteVector &data);
        void saveExisting(TagLib::ByteVector &data, AtomList &path);

        class TagPrivate;
        TagPrivate *d;
    };

  }

}

#endif
