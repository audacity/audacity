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

namespace TagLib {

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

template <class Key, class T>
template <class KeyP, class TP>
class Map<Key, T>::MapPrivate : public RefCounter
{
public:
  MapPrivate() : RefCounter() {}
#ifdef WANT_CLASS_INSTANTIATION_OF_MAP
  MapPrivate(const std::map<class KeyP, class TP>& m) : RefCounter(), map(m) {}
  std::map<class KeyP, class TP> map;
#else
  MapPrivate(const std::map<KeyP, TP>& m) : RefCounter(), map(m) {}
  std::map<KeyP, TP> map;
#endif
};

template <class Key, class T>
Map<Key, T>::Map()
{
  d = new MapPrivate<Key, T>;
}

template <class Key, class T>
Map<Key, T>::Map(const Map<Key, T> &m) : d(m.d)
{
  d->ref();
}

template <class Key, class T>
Map<Key, T>::~Map()
{
  if(d->deref())
    delete(d);
}

template <class Key, class T>
typename Map<Key, T>::Iterator Map<Key, T>::begin()
{
  detach();
  return d->map.begin();
}

template <class Key, class T>
typename Map<Key, T>::ConstIterator Map<Key, T>::begin() const
{
  return d->map.begin();
}

template <class Key, class T>
typename Map<Key, T>::Iterator Map<Key, T>::end()
{
  detach();
  return d->map.end();
}

template <class Key, class T>
typename Map<Key, T>::ConstIterator Map<Key, T>::end() const
{
  return d->map.end();
}

template <class Key, class T>
Map<Key, T> &Map<Key, T>::insert(const Key &key, const T &value)
{
  detach();
  d->map[key] = value;
  return *this;
}

template <class Key, class T>
Map<Key, T> &Map<Key, T>::clear()
{
  detach();
  d->map.clear();
  return *this;
}

template <class Key, class T>
bool Map<Key, T>::isEmpty() const
{
  return d->map.empty();
}

template <class Key, class T>
typename Map<Key, T>::Iterator Map<Key, T>::find(const Key &key)
{
  detach();
  return d->map.find(key);
}

template <class Key, class T>
typename Map<Key,T>::ConstIterator Map<Key, T>::find(const Key &key) const
{
  return d->map.find(key);
}

template <class Key, class T>
bool Map<Key, T>::contains(const Key &key) const
{
  return d->map.find(key) != d->map.end();
}

template <class Key, class T>
Map<Key, T> &Map<Key,T>::erase(Iterator it)
{
  detach();
  d->map.erase(it);
  return *this;
}

template <class Key, class T>
Map<Key, T> &Map<Key,T>::erase(const Key &key)
{
  detach();
  Iterator it = d->map.find(key);
  if(it != d->map.end())
    d->map.erase(it);
  return *this;
}

template <class Key, class T>
TagLib::uint Map<Key, T>::size() const
{
  return d->map.size();
}

template <class Key, class T>
const T &Map<Key, T>::operator[](const Key &key) const
{
  return d->map[key];
}

template <class Key, class T>
T &Map<Key, T>::operator[](const Key &key)
{
  detach();
  return d->map[key];
}

template <class Key, class T>
Map<Key, T> &Map<Key, T>::operator=(const Map<Key, T> &m)
{
  if(&m == this)
    return *this;

  if(d->deref())
    delete(d);
  d = m.d;
  d->ref();
  return *this;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

template <class Key, class T>
void Map<Key, T>::detach()
{
  if(d->count() > 1) {
    d->deref();
    d = new MapPrivate<Key, T>(d->map);
  }
}

} // namespace TagLib
