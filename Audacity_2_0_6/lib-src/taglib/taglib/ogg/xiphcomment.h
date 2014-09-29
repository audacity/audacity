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

#ifndef TAGLIB_VORBISCOMMENT_H
#define TAGLIB_VORBISCOMMENT_H

#include "tag.h"
#include "tlist.h"
#include "tmap.h"
#include "tstring.h"
#include "tstringlist.h"
#include "tbytevector.h"
#include "taglib_export.h"

namespace TagLib {

  namespace Ogg {

    /*!
     * A mapping between a list of field names, or keys, and a list of values
     * associated with that field.
     *
     * \see XiphComment::fieldListMap()
     */
    typedef Map<String, StringList> FieldListMap;

    //! Ogg Vorbis comment implementation

    /*!
     * This class is an implementation of the Ogg Vorbis comment specification,
     * to be found in section 5 of the Ogg Vorbis specification.  Because this
     * format is also used in other (currently unsupported) Xiph.org formats, it
     * has been made part of a generic implementation rather than being limited
     * to strictly Vorbis.
     *
     * Vorbis comments are a simple vector of keys and values, called fields.
     * Multiple values for a given key are supported.
     *
     * \see fieldListMap()
     */

    class TAGLIB_EXPORT XiphComment : public TagLib::Tag
    {
    public:
      /*!
       * Constructs an empty Vorbis comment.
       */
      XiphComment();

      /*!
       * Constructs a Vorbis comment from \a data.
       */
      XiphComment(const ByteVector &data);

      /*!
       * Destroys this instance of the XiphComment.
       */
      virtual ~XiphComment();

      virtual String title() const;
      virtual String artist() const;
      virtual String album() const;
      virtual String comment() const;
      virtual String genre() const;
      virtual uint year() const;
      virtual uint track() const;

      virtual void setTitle(const String &s);
      virtual void setArtist(const String &s);
      virtual void setAlbum(const String &s);
      virtual void setComment(const String &s);
      virtual void setGenre(const String &s);
      virtual void setYear(uint i);
      virtual void setTrack(uint i);

      virtual bool isEmpty() const;

      /*!
       * Returns the number of fields present in the comment.
       */
      uint fieldCount() const;

      /*!
       * Returns a reference to the map of field lists.  Because Xiph comments
       * support multiple fields with the same key, a pure Map would not work.
       * As such this is a Map of string lists, keyed on the comment field name.
       *
       * The standard set of Xiph/Vorbis fields (which may or may not be
       * contained in any specific comment) is:
       *
       * <ul>
       *   <li>TITLE</li>
       *   <li>VERSION</li>
       *   <li>ALBUM</li>
       *   <li>ARTIST</li>
       *   <li>PERFORMER</li>
       *   <li>COPYRIGHT</li>
       *   <li>ORGANIZATION</li>
       *   <li>DESCRIPTION</li>
       *   <li>GENRE</li>
       *   <li>DATE</li>
       *   <li>LOCATION</li>
       *   <li>CONTACT</li>
       *   <li>ISRC</li>
       * </ul>
       *
       * For a more detailed description of these fields, please see the Ogg
       * Vorbis specification, section 5.2.2.1.
       *
       * \note The Ogg Vorbis comment specification does allow these key values
       * to be either upper or lower case.  However, it is conventional for them
       * to be upper case.  As such, TagLib, when parsing a Xiph/Vorbis comment,
       * converts all fields to uppercase.  When you are using this data
       * structure, you will need to specify the field name in upper case.
       *
       * \warning You should not modify this data structure directly, instead
       * use addField() and removeField().
       */
      const FieldListMap &fieldListMap() const;

      /*!
       * Implements the unified property interface -- export function.
       * The result is a one-to-one match of the Xiph comment, since it is
       * completely compatible with the property interface (in fact, a Xiph
       * comment is nothing more than a map from tag names to list of values,
       * as is the dict interface).
       */
      PropertyMap properties() const;

      /*!
       * Implements the unified property interface -- import function.
       * The tags from the given map will be stored one-to-one in the file,
       * except for invalid keys (less than one character, non-ASCII, or
       * containing '=' or '~') in which case the according values will
       * be contained in the returned PropertyMap.
       */
      PropertyMap setProperties(const PropertyMap&);

      /*!
       * Check if the given String is a valid Xiph comment key.
       */
      static bool checkKey(const String&);

      /*!
       * Returns the vendor ID of the Ogg Vorbis encoder.  libvorbis 1.0 as the
       * most common case always returns "Xiph.Org libVorbis I 20020717".
       */
      String vendorID() const;

      /*!
       * Add the field specified by \a key with the data \a value.  If \a replace
       * is true, then all of the other fields with the same key will be removed
       * first.
       *
       * If the field value is empty, the field will be removed.
       */
      void addField(const String &key, const String &value, bool replace = true);

      /*!
       * Remove the field specified by \a key with the data \a value.  If
       * \a value is null, all of the fields with the given key will be removed.
       */
      void removeField(const String &key, const String &value = String::null);

      /*!
       * Returns true if the field is contained within the comment.
       *
       * \note This is safer than checking for membership in the FieldListMap.
       */
      bool contains(const String &key) const;

      /*!
       * Renders the comment to a ByteVector suitable for inserting into a file.
       */
      ByteVector render() const; // BIC: remove and merge with below

      /*!
       * Renders the comment to a ByteVector suitable for inserting into a file.
       *
       * If \a addFramingBit is true the standard Vorbis comment framing bit will
       * be appended.  However some formats (notably FLAC) do not work with this
       * in place.
       */
      ByteVector render(bool addFramingBit) const;

    protected:
      /*!
       * Reads the tag from the file specified in the constructor and fills the
       * FieldListMap.
       */
      void parse(const ByteVector &data);

    private:
      XiphComment(const XiphComment &);
      XiphComment &operator=(const XiphComment &);

      class XiphCommentPrivate;
      XiphCommentPrivate *d;
    };
  }
}

#endif
