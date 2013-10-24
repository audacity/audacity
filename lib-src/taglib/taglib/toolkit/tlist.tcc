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

#include <algorithm>
#include "trefcounter.h"

namespace TagLib {

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

// The functionality of List<T>::setAutoDelete() is implemented here partial
// template specialization.  This is implemented in such a way that calling
// setAutoDelete() on non-pointer types will simply have no effect.

// A base for the generic and specialized private class types.  New
// non-templatized members should be added here.

// BIC change to RefCounter
class ListPrivateBase : public RefCounterOld
{
public:
  ListPrivateBase() : autoDelete(false) {}
  bool autoDelete;
};

// A generic implementation

template <class T>
template <class TP> class List<T>::ListPrivate  : public ListPrivateBase
{
public:
  ListPrivate() : ListPrivateBase() {}
  ListPrivate(const std::list<TP> &l) : ListPrivateBase(), list(l) {}
  void clear() {
    list.clear();
  }
  std::list<TP> list;
};

// A partial specialization for all pointer types that implements the
// setAutoDelete() functionality.

template <class T>
template <class TP> class List<T>::ListPrivate<TP *>  : public ListPrivateBase
{
public:
  ListPrivate() : ListPrivateBase() {}
  ListPrivate(const std::list<TP *> &l) : ListPrivateBase(), list(l) {}
  ~ListPrivate() {
    clear();
  }
  void clear() {
    if(autoDelete) {
      typename std::list<TP *>::const_iterator it = list.begin();
      for(; it != list.end(); ++it)
        delete *it;
    }
    list.clear();
  }
  std::list<TP *> list;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

template <class T>
List<T>::List()
{
  d = new ListPrivate<T>;
}

template <class T>
List<T>::List(const List<T> &l) : d(l.d)
{
  d->ref();
}

template <class T>
List<T>::~List()
{
  if(d->deref())
    delete d;
}

template <class T>
typename List<T>::Iterator List<T>::begin()
{
  detach();
  return d->list.begin();
}

template <class T>
typename List<T>::ConstIterator List<T>::begin() const
{
  return d->list.begin();
}

template <class T>
typename List<T>::Iterator List<T>::end()
{
  detach();
  return d->list.end();
}

template <class T>
typename List<T>::ConstIterator List<T>::end() const
{
  return d->list.end();
}

template <class T>
typename List<T>::Iterator List<T>::insert(Iterator it, const T &item)
{
  detach();
  return d->list.insert(it, item);
}

template <class T>
List<T> &List<T>::sortedInsert(const T &value, bool unique)
{
  detach();
  Iterator it = begin();
  while(it != end() && *it < value)
    ++it;
  if(unique && it != end() && *it == value)
    return *this;
  insert(it, value);
  return *this;
}

template <class T>
List<T> &List<T>::append(const T &item)
{
  detach();
  d->list.push_back(item);
  return *this;
}

template <class T>
List<T> &List<T>::append(const List<T> &l)
{
  detach();
  d->list.insert(d->list.end(), l.begin(), l.end());
  return *this;
}

template <class T>
List<T> &List<T>::prepend(const T &item)
{
  detach();
  d->list.push_front(item);
  return *this;
}

template <class T>
List<T> &List<T>::prepend(const List<T> &l)
{
  detach();
  d->list.insert(d->list.begin(), l.begin(), l.end());
  return *this;
}

template <class T>
List<T> &List<T>::clear()
{
  detach();
  d->clear();
  return *this;
}

template <class T>
TagLib::uint List<T>::size() const
{
  return d->list.size();
}

template <class T>
bool List<T>::isEmpty() const
{
  return d->list.empty();
}

template <class T>
typename List<T>::Iterator List<T>::find(const T &value)
{
  return std::find(d->list.begin(), d->list.end(), value);
}

template <class T>
typename List<T>::ConstIterator List<T>::find(const T &value) const
{
  return std::find(d->list.begin(), d->list.end(), value);
}

template <class T>
bool List<T>::contains(const T &value) const
{
  return std::find(d->list.begin(), d->list.end(), value) != d->list.end();
}

template <class T>
typename List<T>::Iterator List<T>::erase(Iterator it)
{
  return d->list.erase(it);
}

template <class T>
const T &List<T>::front() const
{
  return d->list.front();
}

template <class T>
T &List<T>::front()
{
  detach();
  return d->list.front();
}

template <class T>
const T &List<T>::back() const
{
  return d->list.back();
}

template <class T>
void List<T>::setAutoDelete(bool autoDelete)
{
  d->autoDelete = autoDelete;
}

template <class T>
T &List<T>::back()
{
  detach();
  return d->list.back();
}

template <class T>
T &List<T>::operator[](uint i)
{
  Iterator it = d->list.begin();

  for(uint j = 0; j < i; j++)
    ++it;

  return *it;
}

template <class T>
const T &List<T>::operator[](uint i) const
{
  ConstIterator it = d->list.begin();

  for(uint j = 0; j < i; j++)
    ++it;

  return *it;
}

template <class T>
List<T> &List<T>::operator=(const List<T> &l)
{
  if(&l == this)
    return *this;

  if(d->deref())
    delete d;
  d = l.d;
  d->ref();
  return *this;
}

template <class T>
bool List<T>::operator==(const List<T> &l) const
{
  return d->list == l.d->list;
}

template <class T>
bool List<T>::operator!=(const List<T> &l) const
{
  return d->list != l.d->list;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

template <class T>
void List<T>::detach()
{
  if(d->count() > 1) {
    d->deref();
    d = new ListPrivate<T>(d->list);
  }
}

} // namespace TagLib
