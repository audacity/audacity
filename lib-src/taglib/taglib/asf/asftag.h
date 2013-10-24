/**************************************************************************
    copyright            : (C) 2005-2007 by Lukáš Lalinský
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

#ifndef TAGLIB_ASFTAG_H
#define TAGLIB_ASFTAG_H

#include "tag.h"
#include "tlist.h"
#include "tmap.h"
#include "taglib_export.h"
#include "asfattribute.h"

namespace TagLib {

  namespace ASF {

    typedef List<Attribute> AttributeList;
    typedef Map<String, AttributeList> AttributeListMap;

    class TAGLIB_EXPORT Tag : public TagLib::Tag {

      friend class File;

    public:

      Tag();

      virtual ~Tag();

      /*!
       * Returns the track name.
       */
      virtual String title() const;

      /*!
       * Returns the artist name.
       */
      virtual String artist() const;

      /*!
       * Returns the album name; if no album name is present in the tag
       * String::null will be returned.
       */
      virtual String album() const;

      /*!
       * Returns the track comment.
       */
      virtual String comment() const;

      /*!
       * Returns the genre name; if no genre is present in the tag String::null
       * will be returned.
       */
      virtual String genre() const;

      /*!
       * Returns the rating.
       */
      virtual String rating() const;

      /*!
       * Returns the genre name; if no genre is present in the tag String::null
       * will be returned.
       */
      virtual String copyright() const;

      /*!
       * Returns the year; if there is no year set, this will return 0.
       */
      virtual uint year() const;

      /*!
       * Returns the track number; if there is no track number set, this will
       * return 0.
       */
      virtual uint track() const;

      /*!
       * Sets the title to \a s.
       */
      virtual void setTitle(const String &s);

      /*!
       * Sets the artist to \a s.
       */
      virtual void setArtist(const String &s);

      /*!
       * Sets the album to \a s.  If \a s is String::null then this value will be
       * cleared.
       */
      virtual void setAlbum(const String &s);

      /*!
       * Sets the comment to \a s.
       */
      virtual void setComment(const String &s);

      /*!
       * Sets the rating to \a s.
       */
      virtual void setRating(const String &s);

      /*!
       * Sets the copyright to \a s.
       */
      virtual void setCopyright(const String &s);

      /*!
       * Sets the genre to \a s.
       */
      virtual void setGenre(const String &s);

      /*!
       * Sets the year to \a i.  If \a s is 0 then this value will be cleared.
       */
      virtual void setYear(uint i);

      /*!
       * Sets the track to \a i.  If \a s is 0 then this value will be cleared.
       */
      virtual void setTrack(uint i);

      /*!
       * Returns true if the tag does not contain any data.  This should be
       * reimplemented in subclasses that provide more than the basic tagging
       * abilities in this class.
       */
      virtual bool isEmpty() const;

      /*!
       * Returns a reference to the item list map.  This is an AttributeListMap of
       * all of the items in the tag.
       *
       * This is the most powerfull structure for accessing the items of the tag.
       */
      AttributeListMap &attributeListMap();

      /*!
       * Removes the \a key attribute from the tag
       */
      void removeItem(const String &name);

      /*!
       * Sets the \a key attribute to the value of \a attribute. If an attribute
       * with the \a key is already present, it will be replaced.
       */
      void setAttribute(const String &name, const Attribute &attribute);

      /*!
       * Sets the \a key attribute to the value of \a attribute. If an attribute
       * with the \a key is already present, it will be added to the list.
       */
      void addAttribute(const String &name, const Attribute &attribute);

      PropertyMap properties() const;
      void removeUnsupportedProperties(const StringList& properties);
      PropertyMap setProperties(const PropertyMap &properties);

    private:

      class TagPrivate;
      TagPrivate *d;
    };
  }
}
#endif
