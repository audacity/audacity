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

#ifndef TAGLIB_LIST_H
#define TAGLIB_LIST_H

#include "taglib.h"

#include <list>

namespace TagLib {

  //! A generic, implicitly shared list.

  /*!
   * This is basic generic list that's somewhere between a std::list and a
   * QValueList.  This class is implicitly shared.  For example:
   *
   * \code
   *
   * TagLib::List<int> l = someOtherIntList;
   *
   * \endcode
   *
   * The above example is very cheap.  This also makes lists suitable for the
   * return types of functions.  The above example will just copy a pointer rather
   * than copying the data in the list.  When your \e shared list's data changes,
   * only \e then will the data be copied.
   */

  template <class T> class List
  {
  public:
#ifndef DO_NOT_DOCUMENT
    typedef typename std::list<T>::iterator Iterator;
    typedef typename std::list<T>::const_iterator ConstIterator;
#endif

    /*!
     * Constructs an empty list.
     */
    List();

    /*!
     * Make a shallow, implicitly shared, copy of \a l.  Because this is
     * implicitly shared, this method is lightweight and suitable for
     * pass-by-value usage.
     */
    List(const List<T> &l);

    /*!
     * Destroys this List instance.  If auto deletion is enabled and this list
     * contains a pointer type all of the memebers are also deleted.
     */
    virtual ~List();

    /*!
     * Returns an STL style iterator to the beginning of the list.  See
     * std::list::const_iterator for the semantics.
     */
    Iterator begin();

    /*!
     * Returns an STL style constant iterator to the beginning of the list.  See
     * std::list::iterator for the semantics.
     */
    ConstIterator begin() const;

    /*!
     * Returns an STL style iterator to the end of the list.  See
     * std::list::iterator for the semantics.
     */
    Iterator end();

    /*!
     * Returns an STL style constant iterator to the end of the list.  See
     * std::list::const_iterator for the semantics.
     */
    ConstIterator end() const;

    /*!
     * Inserts a copy of \a value before \a it.
     */
    Iterator insert(Iterator it, const T &value);

    /*!
     * Inserts the \a value into the list.  This assumes that the list is
     * currently sorted.  If \a unique is true then the value will not
     * be inserted if it is already in the list.
     */
    List<T> &sortedInsert(const T &value, bool unique = false);

    /*!
     * Appends \a item to the end of the list and returns a reference to the
     * list.
     */
    List<T> &append(const T &item);

    /*!
     * Appends all of the values in \a l to the end of the list and returns a
     * reference to the list.
     */
    List<T> &append(const List<T> &l);

    /*!
     * Prepends \a item to the beginning list and returns a reference to the
     * list.
     */
    List<T> &prepend(const T &item);

    /*!
     * Prepends all of the items in \a l to the beginning list and returns a
     * reference to the list.
     */
    List<T> &prepend(const List<T> &l);

    /*!
     * Clears the list.  If auto deletion is enabled and this list contains a
     * pointer type the members are also deleted.
     *
     * \see setAutoDelete()
     */
    List<T> &clear();

    /*!
     * Returns the number of elements in the list.
     */
    uint size() const;
    bool isEmpty() const;

    /*!
     * Find the first occurrence of \a value.
     */
    Iterator find(const T &value);

    /*!
     * Find the first occurrence of \a value.
     */
    ConstIterator find(const T &value) const;

    /*!
     * Returns true if the list contains \a value.
     */
    bool contains(const T &value) const;

    /*!
     * Erase the item at \a it from the list.
     */
    Iterator erase(Iterator it);

    /*!
     * Returns a reference to the first item in the list.
     */
    const T &front() const;

    /*!
     * Returns a reference to the first item in the list.
     */
    T &front();

    /*!
     * Returns a reference to the last item in the list.
     */
    const T &back() const;

    /*!
     * Returns a reference to the last item in the list.
     */
    T &back();

    /*!
     * Auto delete the members of the list when the last reference to the list
     * passes out of scope.  This will have no effect on lists which do not
     * contain a pointer type.
     *
     * \note This relies on partial template instantiation -- most modern C++
     * compilers should now support this.
     */
    void setAutoDelete(bool autoDelete);

    /*!
     * Returns a reference to item \a i in the list.
     *
     * \warning This method is slow.  Use iterators to loop through the list.
     */
    T &operator[](uint i);

    /*!
     * Returns a const reference to item \a i in the list.
     *
     * \warning This method is slow.  Use iterators to loop through the list.
     */
    const T &operator[](uint i) const;

    /*!
     * Make a shallow, implicitly shared, copy of \a l.  Because this is
     * implicitly shared, this method is lightweight and suitable for
     * pass-by-value usage.
     */
    List<T> &operator=(const List<T> &l);

    /*!
     * Compares this list with \a l and returns true if all of the elements are
     * the same.
     */
    bool operator==(const List<T> &l) const;

  protected:
    /*
     * If this List is being shared via implicit sharing, do a deep copy of the
     * data and separate from the shared members.  This should be called by all
     * non-const subclass members.
     */
    void detach();

  private:
#ifndef DO_NOT_DOCUMENT
    template <class TP> class ListPrivate;
    ListPrivate<T> *d;
#endif
  };

}

// Since GCC doesn't support the "export" keyword, we have to include the
// implementation.

#include "tlist.tcc"

#endif
