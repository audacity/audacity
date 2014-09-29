/***************************************************************************
    copyright            : (C) 2004 by Allan Sandfeld Jensen
    email                : kde@carewolf.com
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

#include <tbytevectorlist.h>
#include <tdebug.h>

#include "apeitem.h"

using namespace TagLib;
using namespace APE;

class APE::Item::ItemPrivate
{
public:
  ItemPrivate() : type(Text), readOnly(false) {}

  Item::ItemTypes type;
  String key;
  ByteVector value;
  StringList text;
  bool readOnly;
};

APE::Item::Item()
{
  d = new ItemPrivate;
}

APE::Item::Item(const String &key, const String &value)
{
  d = new ItemPrivate;
  d->key = key;
  d->text.append(value);
}

APE::Item::Item(const String &key, const StringList &values)
{
  d = new ItemPrivate;
  d->key = key;
  d->text = values;
}

APE::Item::Item(const String &key, const ByteVector &value, bool binary)
{
  d = new ItemPrivate;
  d->key = key;
  if(binary) {
    d->type = Binary;
    d->value = value;
  }
  else
    d->text.append(value);
}

APE::Item::Item(const Item &item)
{
  d = new ItemPrivate(*item.d);
}

APE::Item::~Item()
{
  delete d;
}

Item &APE::Item::operator=(const Item &item)
{
  delete d;
  d = new ItemPrivate(*item.d);
  return *this;
}

void APE::Item::setReadOnly(bool readOnly)
{
  d->readOnly = readOnly;
}

bool APE::Item::isReadOnly() const
{
  return d->readOnly;
}

void APE::Item::setType(APE::Item::ItemTypes val)
{
  d->type = val;
}

APE::Item::ItemTypes APE::Item::type() const
{
  return d->type;
}

String APE::Item::key() const
{
  return d->key;
}

ByteVector APE::Item::binaryData() const
{
  return d->value;
}

void APE::Item::setBinaryData(const ByteVector &value)
{
  d->type = Binary;
  d->value = value;
  d->text.clear();
}

ByteVector APE::Item::value() const
{
  // This seems incorrect as it won't be actually rendering the value to keep it
  // up to date.

  return d->value;
}

void APE::Item::setKey(const String &key)
{
  d->key = key;
}

void APE::Item::setValue(const String &value)
{
  d->type = Text;
  d->text = value;
  d->value.clear();
}

void APE::Item::setValues(const StringList &value)
{
  d->type = Text;
  d->text = value;
  d->value.clear();
}

void APE::Item::appendValue(const String &value)
{
  d->type = Text;
  d->text.append(value);
  d->value.clear();
}

void APE::Item::appendValues(const StringList &values)
{
  d->type = Text;
  d->text.append(values);
  d->value.clear();
}

int APE::Item::size() const
{
  // SFB: Why is d->key.size() used when size() returns the length in UniChars and not UTF-8?
  int result = 8 + d->key.size() /* d->key.data(String::UTF8).size() */ + 1;
  switch (d->type) {
    case Text:
      if(d->text.size()) {
        StringList::ConstIterator it = d->text.begin();

        result += it->data(String::UTF8).size();
        it++;
        for(; it != d->text.end(); ++it)
          result += 1 + it->data(String::UTF8).size();
      }
      break;

    case Binary:
    case Locator:
      result += d->value.size();
      break;
  }
  return result;
}

StringList APE::Item::toStringList() const
{
  return d->text;
}

StringList APE::Item::values() const
{
  return d->text;
}

String APE::Item::toString() const
{
  if(d->type == Text && !isEmpty())
    return d->text.front();
  else
    return String::null;
}

bool APE::Item::isEmpty() const
{
  switch(d->type) {
    case Text:
      if(d->text.isEmpty())
        return true;
      if(d->text.size() == 1 && d->text.front().isEmpty())
        return true;
      return false;
    case Binary:
    case Locator:
      return d->value.isEmpty();
    default:
      return false;
  }
}

void APE::Item::parse(const ByteVector &data)
{
  // 11 bytes is the minimum size for an APE item

  if(data.size() < 11) {
    debug("APE::Item::parse() -- no data in item");
    return;
  }

  const uint valueLength  = data.toUInt(0, false);
  const uint flags        = data.toUInt(4, false);

  d->key = String(data.mid(8), String::UTF8);

  const ByteVector value = data.mid(8 + d->key.size() + 1, valueLength);

  setReadOnly(flags & 1);
  setType(ItemTypes((flags >> 1) & 3));

  if(Text == d->type) 
    d->text = StringList(ByteVectorList::split(value, '\0'), String::UTF8);
  else
    d->value = value;
}

ByteVector APE::Item::render() const
{
  ByteVector data;
  TagLib::uint flags = ((d->readOnly) ? 1 : 0) | (d->type << 1);
  ByteVector value;

  if(isEmpty())
    return data;

  if(d->type == Text) {
    StringList::ConstIterator it = d->text.begin();

    value.append(it->data(String::UTF8));
    it++;
    for(; it != d->text.end(); ++it) {
      value.append('\0');
      value.append(it->data(String::UTF8));
    }
    d->value = value;
  }
  else
    value.append(d->value);

  data.append(ByteVector::fromUInt(value.size(), false));
  data.append(ByteVector::fromUInt(flags, false));
  data.append(d->key.data(String::UTF8));
  data.append(ByteVector('\0'));
  data.append(value);

  return data;
}
