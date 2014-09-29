/***************************************************************************
    copyright           : (C) 2012 by Michael Helmling
    email               : helmling@mathematik.uni-kl.de
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it  under the terms of the GNU Lesser General Public License version  *
 *   2.1 as published by the Free Software Foundation.                     *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful, but   *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *
 *   MA  02110-1301  USA                                                   *
 ***************************************************************************/

#ifndef PROPERTYMAP_H_
#define PROPERTYMAP_H_

#include "tmap.h"
#include "tstringlist.h"

namespace TagLib {

  typedef Map<String,StringList> SimplePropertyMap;

  //! A map for format-independent <key,valuelist> tag representations.

  /*!
   * This map implements a generic representation of textual audio metadata
   * ("tags") realized as pairs of a case-insensitive key
   * and a nonempty list of corresponding values, each value being an an arbitrary
   * unicode String.
   *
   * Note that most metadata formats pose additional conditions on the tag keys. The
   * most popular ones (Vorbis, APE, ID3v2) should support all ASCII only words of
   * length between 2 and 16.
   * 
   * This class can contain any tags, but here is a list of "well-known" tags that
   * you might want to use:
   *
   * Basic tags:
   *
   *  - TITLE
   *  - ALBUM
   *  - ARTIST
   *  - ALBUMARTIST
   *  - SUBTITLE
   *  - TRACKNUMBER
   *  - DISCNUMBER
   *  - DATE
   *  - ORIGINALDATE
   *  - GENRE
   *  - COMMENT
   *
   * Sort names:
   *
   *  - TITLESORT
   *  - ALBUMSORT
   *  - ARTISTSORT
   *  - ALBUMARTISTSORT
   *
   * Credits:
   *
   *  - COMPOSER
   *  - LYRICIST
   *  - CONDUCTOR
   *  - REMIXER
   *  - PERFORMER:<XXXX>
   *
   * Other tags:
   *
   *  - ISRC
   *  - ASIN
   *  - BPM
   *  - COPYRIGHT
   *  - ENCODEDBY
   *  - MOOD
   *  - COMMENT 
   *  - MEDIA
   *  - LABEL
   *  - CATALOGNUMBER
   *  - BARCODE
   *
   * MusicBrainz identifiers:
   * 
   *  - MUSICBRAINZ_TRACKID
   *  - MUSICBRAINZ_ALBUMID
   *  - MUSICBRAINZ_RELEASEGROUPID
   *  - MUSICBRAINZ_WORKID
   *  - MUSICBRAINZ_ARTISTID
   *  - MUSICBRAINZ_ALBUMARTISTID
   *  - ACOUSTID_ID
   *  - ACOUSTID_FINGERPRINT
   *  - MUSICIP_PUID
   *
   */

  class TAGLIB_EXPORT PropertyMap: public SimplePropertyMap
  {
  public:

    typedef SimplePropertyMap::Iterator Iterator;
    typedef SimplePropertyMap::ConstIterator ConstIterator;

    PropertyMap();

    PropertyMap(const PropertyMap &m);

    /*!
     * Creates a PropertyMap initialized from a SimplePropertyMap. Copies all
     * entries from \a m that have valid keys.
     * Invalid keys will be appended to the unsupportedData() list.
     */
    PropertyMap(const SimplePropertyMap &m);

    virtual ~PropertyMap();

    /*!
     * Inserts \a values under \a key in the map.  If \a key already exists,
     * then \values will be appended to the existing StringList.
     * The returned value indicates success, i.e. whether \a key is a
     * valid key.
     */
    bool insert(const String &key, const StringList &values);

    /*!
     * Replaces any existing values for \a key with the given \a values,
     * and simply insert them if \a key did not exist before.
     * The returned value indicates success, i.e. whether \a key is a
     * valid key.
     */
    bool replace(const String &key, const StringList &values);

    /*!
     * Find the first occurrence of \a key.
     */
    Iterator find(const String &key);

    /*!
     * Find the first occurrence of \a key.
     */
    ConstIterator find(const String &key) const;

    /*!
     * Returns true if the map contains values for \a key.
     */
    bool contains(const String &key) const;

    /*!
     * Returns true if this map contains all keys of \a other
     * and the values coincide for that keys. Does not take
     * the unsupportedData list into account.
     */
    bool contains(const PropertyMap &other) const;

    /*!
     * Erase the \a key and its values from the map.
     */
    PropertyMap &erase(const String &key);

    /*!
     * Erases from this map all keys that appear in \a other.
     */
    PropertyMap &erase(const PropertyMap &other);

    /*!
     * Merge the contents of \a other into this PropertyMap.
     * If a key is contained in both maps, the values of the second
     * are appended to that of the first.
     * The unsupportedData() lists are concatenated as well.
     */
    PropertyMap &merge(const PropertyMap &other);

    /*!
     * Returns a reference to the value associated with \a key.
     *
     * \note: If \a key is not contained in the map, an empty
     * StringList is returned without error.
     */
    const StringList &operator[](const String &key) const;

    /*!
     * Returns a reference to the value associated with \a key.
     *
     * \note: If \a key is not contained in the map, an empty
     * StringList is returned. You can also directly add entries
     * by using this function as an lvalue.
     */
    StringList &operator[](const String &key);

    /*!
     * Returns true if and only if \other has the same contents as this map.
     */
    bool operator==(const PropertyMap &other) const;

    /*!
     * Returns false if and only \other has the same contents as this map.
     */
    bool operator!=(const PropertyMap &other) const;

    /*!
     * If a PropertyMap is read from a File object using File::properties(),
     * the StringList returned from this function will represent metadata
     * that could not be parsed into the PropertyMap representation. This could
     * be e.g. binary data, unknown ID3 frames, etc.
     * You can remove items from the returned list, which tells TagLib to remove
     * those unsupported elements if you call File::setProperties() with the
     * same PropertyMap as argument.
     */
    StringList &unsupportedData();
    const StringList &unsupportedData() const;

    /*!
     * Removes all entries which have an empty value list.
     */
    void removeEmpty();

    String toString() const;

  private:


    StringList unsupported;
  };

}
#endif /* PROPERTYMAP_H_ */
