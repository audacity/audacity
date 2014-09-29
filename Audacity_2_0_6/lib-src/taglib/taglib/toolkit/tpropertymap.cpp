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

#include "tpropertymap.h"
using namespace TagLib;


PropertyMap::PropertyMap() : SimplePropertyMap()
{
}

PropertyMap::PropertyMap(const PropertyMap &m) : SimplePropertyMap(m), unsupported(m.unsupported)
{
}

PropertyMap::PropertyMap(const SimplePropertyMap &m)
{
  for(SimplePropertyMap::ConstIterator it = m.begin(); it != m.end(); ++it){
    String key = it->first.upper();
    if(!key.isNull())
      insert(it->first, it->second);
    else
      unsupported.append(it->first);
  }
}

PropertyMap::~PropertyMap()
{
}

bool PropertyMap::insert(const String &key, const StringList &values)
{
  String realKey = key.upper();
  Iterator result = SimplePropertyMap::find(realKey);
  if(result == end())
    SimplePropertyMap::insert(realKey, values);
  else
    SimplePropertyMap::operator[](realKey).append(values);
  return true;
}

bool PropertyMap::replace(const String &key, const StringList &values)
{
  String realKey = key.upper();
  SimplePropertyMap::erase(realKey);
  SimplePropertyMap::insert(realKey, values);
  return true;
}

PropertyMap::Iterator PropertyMap::find(const String &key)
{
  return SimplePropertyMap::find(key.upper());
}

PropertyMap::ConstIterator PropertyMap::find(const String &key) const
{
  return SimplePropertyMap::find(key.upper());
}

bool PropertyMap::contains(const String &key) const
{
  return SimplePropertyMap::contains(key.upper());
}

bool PropertyMap::contains(const PropertyMap &other) const
{
  for(ConstIterator it = other.begin(); it != other.end(); ++it) {
    if(!SimplePropertyMap::contains(it->first))
      return false;
    if ((*this)[it->first] != it->second)
      return false;
  }
  return true;
}

PropertyMap &PropertyMap::erase(const String &key)
{
  SimplePropertyMap::erase(key.upper());
  return *this;
}

PropertyMap &PropertyMap::erase(const PropertyMap &other)
{
  for(ConstIterator it = other.begin(); it != other.end(); ++it)
    erase(it->first);
  return *this;
}

PropertyMap &PropertyMap::merge(const PropertyMap &other)
{
  for(PropertyMap::ConstIterator it = other.begin(); it != other.end(); ++it)
    insert(it->first, it->second);
  unsupported.append(other.unsupported);
  return *this;
}

const StringList &PropertyMap::operator[](const String &key) const
{
  return SimplePropertyMap::operator[](key.upper());
}

StringList &PropertyMap::operator[](const String &key)
{
  return SimplePropertyMap::operator[](key.upper());
}

bool PropertyMap::operator==(const PropertyMap &other) const
{
  for(ConstIterator it = other.begin(); it != other.end(); ++it) {
    ConstIterator thisFind = find(it->first);
    if( thisFind == end() || (thisFind->second != it->second) )
      return false;
  }
  for(ConstIterator it = begin(); it != end(); ++it) {
    ConstIterator otherFind = other.find(it->first);
    if( otherFind == other.end() || (otherFind->second != it->second) )
      return false;
  }
  return unsupported == other.unsupported;
}

bool PropertyMap::operator!=(const PropertyMap &other) const
{
  return !(*this == other);
}

String PropertyMap::toString() const
{
  String ret = "";
  for(ConstIterator it = begin(); it != end(); ++it)
    ret += it->first+"="+it->second.toString(", ") + "\n";
  if(!unsupported.isEmpty())
    ret += "Unsupported Data: " + unsupported.toString(", ") + "\n";
  return ret;
}

void PropertyMap::removeEmpty()
{
  StringList emptyKeys;
  for(Iterator it = begin(); it != end(); ++it)
    if(it->second.isEmpty())
      emptyKeys.append(it->first);
  for(StringList::Iterator emptyIt = emptyKeys.begin(); emptyIt != emptyKeys.end(); emptyIt++ )
    erase(*emptyIt);
}

StringList &PropertyMap::unsupportedData()
{
  return unsupported;
}

const StringList &PropertyMap::unsupportedData() const
{
  return unsupported;
}
