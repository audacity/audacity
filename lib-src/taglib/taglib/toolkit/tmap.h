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

#ifndef TAGLIB_MAP_H
#define TAGLIB_MAP_H

#include <map>
using namespace std;

#include "taglib.h"

namespace TagLib {

  //! A generic, implicitly shared map.

  /*!
   * This implements a standard map container that associates a key with a value
   * and has fast key-based lookups.  This map is also implicitly shared making
   * it suitable for pass-by-value usage.
   */

  template <class Key, class T> class Map
  {
  public:
#ifndef DO_NOT_DOCUMENT
#ifdef WANT_CLASS_INSTANTIATION_OF_MAP
    // Some STL implementations get snippy over the use of the
    // class keyword to distinguish different templates; Sun Studio
    // in particular finds multiple specializations in certain rare
    // cases and complains about that. GCC doesn't seem to mind,
    // and uses the typedefs further below without the class keyword.
    // Not all the specializations of Map can use the class keyword
    // (when T is not actually a class type), so don't apply this
    // generally.
    typedef typename std::map<class Key, class T>::iterator Iterator;
    typedef typename std::map<class Key, class T>::const_iterator ConstIterator;
#else
    typedef typename std::map<Key, T>::iterator Iterator;
    typedef typename std::map<Key, T>::const_iterator ConstIterator;
#endif
#endif

    /*!
     * Constructs an empty Map.
     */
    Map();

    /*!
     * Make a shallow, implicitly shared, copy of \a m.  Because this is
     * implicitly shared, this method is lightweight and suitable for
     * pass-by-value usage.
     */
    Map(const Map<Key, T> &m);

    /*!
     * Destroys this instance of the Map.
     */
    virtual ~Map();

    /*!
     * Returns an STL style iterator to the beginning of the map.  See
     * std::map::iterator for the semantics.
     */
    Iterator begin();

    /*!
     * Returns an STL style iterator to the beginning of the map.  See
     * std::map::const_iterator for the semantics.
     */
    ConstIterator begin() const;

    /*!
     * Returns an STL style iterator to the end of the map.  See
     * std::map::iterator for the semantics.
     */
    Iterator end();

    /*!
     * Returns an STL style iterator to the end of the map.  See
     * std::map::const_iterator for the semantics.
     */
    ConstIterator end() const;

    /*!
     * Inserts \a value under \a key in the map.  If a value for \a key already
     * exists it will be overwritten.
     */
    Map<Key, T> &insert(const Key &key, const T &value);

    /*!
     * Removes all of the elements from elements from the map.  This however
     * will not delete pointers if the mapped type is a pointer type.
     */
    Map<Key, T> &clear();

    /*!
     * The number of elements in the map.
     *
     * \see isEmpty()
     */
    uint size() const;

    /*!
     * Returns true if the map is empty.
     *
     * \see size()
     */
    bool isEmpty() const;

    /*!
     * Find the first occurrence of \a key.
     */
    Iterator find(const Key &key);

    /*!
     * Find the first occurrence of \a key.
     */
    ConstIterator find(const Key &key) const;

    /*!
     * Returns true if the map contains an instance of \a key.
     */
    bool contains(const Key &key) const;

    /*!
     * Erase the item at \a it from the list.
     */
    Map<Key, T> &erase(Iterator it);

    /*!
     * Erase the item with \a key from the list.
     */
    Map<Key, T> &erase(const Key &key);

    /*!
     * Returns a reference to the value associated with \a key.
     *
     * \note This has undefined behavior if the key is not present in the map.
     */
    const T &operator[](const Key &key) const;

    /*!
     * Returns a reference to the value associated with \a key.
     *
     * \note This has undefined behavior if the key is not present in the map.
     */
    T &operator[](const Key &key);

    /*!
     * Make a shallow, implicitly shared, copy of \a m.  Because this is
     * implicitly shared, this method is lightweight and suitable for
     * pass-by-value usage.
     */
    Map<Key, T> &operator=(const Map<Key, T> &m);

  protected:
    /*
     * If this List is being shared via implicit sharing, do a deep copy of the
     * data and separate from the shared members.  This should be called by all
     * non-const subclass members.
     */
    void detach();

  private:
#ifndef DO_NOT_DOCUMENT
    template <class KeyP, class TP> class MapPrivate;
    MapPrivate<Key, T> *d;
#endif
  };

}

// Since GCC doesn't support the "export" keyword, we have to include the
// implementation.

#include "tmap.tcc"

#endif
