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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef WITH_ASF

#include "asftag.h"

using namespace TagLib;

class ASF::Tag::TagPrivate
{
public:
  String title;
  String artist;
  String copyright;
  String comment;
  String rating;
  AttributeListMap attributeListMap;
};

ASF::Tag::Tag()
: TagLib::Tag()
{
  d = new TagPrivate;
}

ASF::Tag::~Tag()
{
  if(d)
    delete d;
}

String
ASF::Tag::title() const
{
  return d->title;
}

String
ASF::Tag::artist() const
{
  return d->artist;
}

String
ASF::Tag::album() const
{
  if(d->attributeListMap.contains("WM/AlbumTitle"))
    return d->attributeListMap["WM/AlbumTitle"][0].toString();
  return String::null;
}

String
ASF::Tag::copyright() const
{
  return d->copyright;
}

String
ASF::Tag::comment() const
{
  return d->comment;
}

String
ASF::Tag::rating() const
{
  return d->rating;
}

unsigned int
ASF::Tag::year() const
{
  if(d->attributeListMap.contains("WM/Year"))
    return d->attributeListMap["WM/Year"][0].toString().toInt();
  return 0;
}

unsigned int
ASF::Tag::track() const
{
  if(d->attributeListMap.contains("WM/TrackNumber"))
    return d->attributeListMap["WM/TrackNumber"][0].toString().toInt();
  if(d->attributeListMap.contains("WM/Track"))
    return d->attributeListMap["WM/Track"][0].toUInt();
  return 0;
}

String
ASF::Tag::genre() const
{
  if(d->attributeListMap.contains("WM/Genre"))
    return d->attributeListMap["WM/Genre"][0].toString();
  return String::null;
}

void
ASF::Tag::setTitle(const String &value)
{
  d->title = value;
}

void
ASF::Tag::setArtist(const String &value)
{
  d->artist = value;
}

void
ASF::Tag::setCopyright(const String &value)
{
  d->copyright = value;
}

void
ASF::Tag::setComment(const String &value)
{
  d->comment = value;
}

void
ASF::Tag::setRating(const String &value)
{
  d->rating = value;
}

void
ASF::Tag::setAlbum(const String &value)
{
  setAttribute("WM/AlbumTitle", value);
}

void
ASF::Tag::setGenre(const String &value)
{
  setAttribute("WM/Genre", value);
}

void
ASF::Tag::setYear(uint value)
{
  setAttribute("WM/Year", String::number(value));
}

void
ASF::Tag::setTrack(uint value)
{
  setAttribute("WM/TrackNumber", String::number(value));
}

ASF::AttributeListMap&
ASF::Tag::attributeListMap()
{
  return d->attributeListMap;
}

void ASF::Tag::removeItem(const String &key)
{
  AttributeListMap::Iterator it = d->attributeListMap.find(key);
  if(it != d->attributeListMap.end())
    d->attributeListMap.erase(it);
}

void ASF::Tag::setAttribute(const String &name, const Attribute &attribute)
{
  AttributeList value;
  value.append(attribute);
  d->attributeListMap.insert(name, value);
}

void ASF::Tag::addAttribute(const String &name, const Attribute &attribute)
{
  if(d->attributeListMap.contains(name)) {
    d->attributeListMap[name].append(attribute);
  }
  else {
    setAttribute(name, attribute);
  }
}

bool ASF::Tag::isEmpty() const {
  return TagLib::Tag::isEmpty() &&
         copyright().isEmpty() &&
         rating().isEmpty() &&
         d->attributeListMap.isEmpty();
}

#endif
