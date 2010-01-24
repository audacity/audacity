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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifdef __SUNPRO_CC
// Sun Studio finds multiple specializations of Map because
// it considers specializations with and without class types
// to be different; this define forces Map to use only the
// specialization with the class keyword.
#define WANT_CLASS_INSTANTIATION_OF_MAP (1)
#endif

#include <tfile.h>
#include <tstring.h>
#include <tmap.h>

#include "apetag.h"
#include "apefooter.h"
#include "apeitem.h"

using namespace TagLib;
using namespace APE;

class APE::Tag::TagPrivate
{
public:
  TagPrivate() : file(0), footerLocation(-1), tagLength(0) {}

  File *file;
  long footerLocation;
  long tagLength;

  Footer footer;

  ItemListMap itemListMap;
};

////////////////////////////////////////////////////////////////////////////////
// public methods
////////////////////////////////////////////////////////////////////////////////

APE::Tag::Tag() : TagLib::Tag()
{
  d = new TagPrivate;
}

APE::Tag::Tag(File *file, long footerLocation) : TagLib::Tag()
{
  d = new TagPrivate;
  d->file = file;
  d->footerLocation = footerLocation;

  read();
}

APE::Tag::~Tag()
{
  delete d;
}

ByteVector APE::Tag::fileIdentifier()
{
  return ByteVector::fromCString("APETAGEX");
}

String APE::Tag::title() const
{
  if(d->itemListMap["TITLE"].isEmpty())
    return String::null;
  return d->itemListMap["TITLE"].toString();
}

String APE::Tag::artist() const
{
  if(d->itemListMap["ARTIST"].isEmpty())
    return String::null;
  return d->itemListMap["ARTIST"].toString();
}

String APE::Tag::album() const
{
  if(d->itemListMap["ALBUM"].isEmpty())
    return String::null;
  return d->itemListMap["ALBUM"].toString();
}

String APE::Tag::comment() const
{
  if(d->itemListMap["COMMENT"].isEmpty())
    return String::null;
  return d->itemListMap["COMMENT"].toString();
}

String APE::Tag::genre() const
{
  if(d->itemListMap["GENRE"].isEmpty())
    return String::null;
  return d->itemListMap["GENRE"].toString();
}

TagLib::uint APE::Tag::year() const
{
  if(d->itemListMap["YEAR"].isEmpty())
    return 0;
  return d->itemListMap["YEAR"].toString().toInt();
}

TagLib::uint APE::Tag::track() const
{
  if(d->itemListMap["TRACK"].isEmpty())
    return 0;
  return d->itemListMap["TRACK"].toString().toInt();
}

void APE::Tag::setTitle(const String &s)
{
  addValue("TITLE", s, true);
}

void APE::Tag::setArtist(const String &s)
{
  addValue("ARTIST", s, true);
}

void APE::Tag::setAlbum(const String &s)
{
  addValue("ALBUM", s, true);
}

void APE::Tag::setComment(const String &s)
{
  addValue("COMMENT", s, true);
}

void APE::Tag::setGenre(const String &s)
{
  addValue("GENRE", s, true);
}

void APE::Tag::setYear(uint i)
{
  if(i <= 0)
    removeItem("YEAR");
  else
    addValue("YEAR", String::number(i), true);
}

void APE::Tag::setTrack(uint i)
{
  if(i <= 0)
    removeItem("TRACK");
  else
    addValue("TRACK", String::number(i), true);
}

APE::Footer *APE::Tag::footer() const
{
  return &d->footer;
}

const APE::ItemListMap& APE::Tag::itemListMap() const
{
  return d->itemListMap;
}

void APE::Tag::removeItem(const String &key)
{
  Map<const String, Item>::Iterator it = d->itemListMap.find(key.upper());
  if(it != d->itemListMap.end())
    d->itemListMap.erase(it);
}

void APE::Tag::addValue(const String &key, const String &value, bool replace)
{
  if(replace)
    removeItem(key);
  if(!value.isEmpty()) {
    if(d->itemListMap.contains(key) || !replace)
      d->itemListMap[key.upper()].appendValue(value);
    else
      setItem(key, Item(key, value));
  }
}

void APE::Tag::setItem(const String &key, const Item &item)
{
  d->itemListMap.insert(key.upper(), item);
}

////////////////////////////////////////////////////////////////////////////////
// protected methods
////////////////////////////////////////////////////////////////////////////////

void APE::Tag::read()
{
  if(d->file && d->file->isValid()) {

    d->file->seek(d->footerLocation);
    d->footer.setData(d->file->readBlock(Footer::size()));

    if(d->footer.tagSize() <= Footer::size() ||
       d->footer.tagSize() > uint(d->file->length()))
      return;

    d->file->seek(d->footerLocation + Footer::size() - d->footer.tagSize());
    parse(d->file->readBlock(d->footer.tagSize() - Footer::size()));
  }
}

ByteVector APE::Tag::render() const
{
  ByteVector data;
  uint itemCount = 0;

  {
    for(Map<const String, Item>::ConstIterator it = d->itemListMap.begin();
        it != d->itemListMap.end(); ++it)
    {
      data.append(it->second.render());
      itemCount++;
    }
  }

  d->footer.setItemCount(itemCount);
  d->footer.setTagSize(data.size() + Footer::size());
  d->footer.setHeaderPresent(true);

  return d->footer.renderHeader() + data + d->footer.renderFooter();
}

void APE::Tag::parse(const ByteVector &data)
{
  uint pos = 0;

  // 11 bytes is the minimum size for an APE item

  for(uint i = 0; i < d->footer.itemCount() && pos <= data.size() - 11; i++) {
    APE::Item item;
    item.parse(data.mid(pos));

    d->itemListMap.insert(item.key().upper(), item);

    pos += item.size();
  }
}
