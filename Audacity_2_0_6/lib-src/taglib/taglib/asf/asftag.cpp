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
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *
 *   02110-1301  USA                                                       *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#include <tpropertymap.h>
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

String ASF::Tag::title() const
{
  return d->title;
}

String ASF::Tag::artist() const
{
  return d->artist;
}

String ASF::Tag::album() const
{
  if(d->attributeListMap.contains("WM/AlbumTitle"))
    return d->attributeListMap["WM/AlbumTitle"][0].toString();
  return String::null;
}

String ASF::Tag::copyright() const
{
  return d->copyright;
}

String ASF::Tag::comment() const
{
  return d->comment;
}

String ASF::Tag::rating() const
{
  return d->rating;
}

unsigned int ASF::Tag::year() const
{
  if(d->attributeListMap.contains("WM/Year"))
    return d->attributeListMap["WM/Year"][0].toString().toInt();
  return 0;
}

unsigned int ASF::Tag::track() const
{
  if(d->attributeListMap.contains("WM/TrackNumber")) {
    const ASF::Attribute attr = d->attributeListMap["WM/TrackNumber"][0];
    if(attr.type() == ASF::Attribute::DWordType)
      return attr.toUInt();
    else
      return attr.toString().toInt();
  }
  if(d->attributeListMap.contains("WM/Track"))
    return d->attributeListMap["WM/Track"][0].toUInt();
  return 0;
}

String ASF::Tag::genre() const
{
  if(d->attributeListMap.contains("WM/Genre"))
    return d->attributeListMap["WM/Genre"][0].toString();
  return String::null;
}

void ASF::Tag::setTitle(const String &value)
{
  d->title = value;
}

void ASF::Tag::setArtist(const String &value)
{
  d->artist = value;
}

void ASF::Tag::setCopyright(const String &value)
{
  d->copyright = value;
}

void ASF::Tag::setComment(const String &value)
{
  d->comment = value;
}

void ASF::Tag::setRating(const String &value)
{
  d->rating = value;
}

void ASF::Tag::setAlbum(const String &value)
{
  setAttribute("WM/AlbumTitle", value);
}

void ASF::Tag::setGenre(const String &value)
{
  setAttribute("WM/Genre", value);
}

void ASF::Tag::setYear(uint value)
{
  setAttribute("WM/Year", String::number(value));
}

void ASF::Tag::setTrack(uint value)
{
  setAttribute("WM/TrackNumber", String::number(value));
}

ASF::AttributeListMap& ASF::Tag::attributeListMap()
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

bool ASF::Tag::isEmpty() const
{
  return TagLib::Tag::isEmpty() &&
         copyright().isEmpty() &&
         rating().isEmpty() &&
         d->attributeListMap.isEmpty();
}

static const char *keyTranslation[][2] = {
  { "WM/AlbumTitle", "ALBUM" },
  { "WM/Composer", "COMPOSER" },
  { "WM/Writer", "WRITER" },
  { "WM/Conductor", "CONDUCTOR" },
  { "WM/ModifiedBy", "REMIXER" },
  { "WM/Year", "DATE" },
  { "WM/OriginalReleaseYear", "ORIGINALDATE" },
  { "WM/Producer", "PRODUCER" },
  { "WM/ContentGroupDescription", "GROUPING" },
  { "WM/SubTitle", "SUBTITLE" },
  { "WM/SetSubTitle", "DISCSUBTITLE" },
  { "WM/TrackNumber", "TRACKNUMBER" },
  { "WM/PartOfSet", "DISCNUMBER" },
  { "WM/Genre", "GENRE" },
  { "WM/BeatsPerMinute", "BPM" },
  { "WM/Mood", "MOOD" },
  { "WM/ISRC", "ISRC" },
  { "WM/Lyrics", "LYRICS" },
  { "WM/Media", "MEDIA" },
  { "WM/Publisher", "LABEL" },
  { "WM/CatalogNo", "CATALOGNUMBER" },
  { "WM/Barcode", "BARCODE" },
  { "WM/EncodedBy", "ENCODEDBY" },
  { "WM/AlbumSortOrder", "ALBUMSORT" },
  { "WM/AlbumArtistSortOrder", "ALBUMARTISTSORT" },
  { "WM/ArtistSortOrder", "ARTISTSORT" },
  { "WM/TitleSortOrder", "TITLESORT" },
  { "WM/Script", "SCRIPT" },
  { "WM/Language", "LANGUAGE" },
  { "MusicBrainz/Track Id", "MUSICBRAINZ_TRACKID" },
  { "MusicBrainz/Artist Id", "MUSICBRAINZ_ARTISTID" },
  { "MusicBrainz/Album Id", "MUSICBRAINZ_ALBUMID" },
  { "MusicBrainz/Album Artist Id", "MUSICBRAINZ_ALBUMARTISTID" },
  { "MusicBrainz/Release Group Id", "MUSICBRAINZ_RELEASEGROUPID" },
  { "MusicBrainz/Work Id", "MUSICBRAINZ_WORKID" },
  { "MusicIP/PUID", "MUSICIP_PUID" },
  { "Acoustid/Id", "ACOUSTID_ID" },
  { "Acoustid/Fingerprint", "ACOUSTID_FINGERPRINT" },
};

PropertyMap ASF::Tag::properties() const
{
  static Map<String, String> keyMap;
  if(keyMap.isEmpty()) {
    int numKeys = sizeof(keyTranslation) / sizeof(keyTranslation[0]);
    for(int i = 0; i < numKeys; i++) {
      keyMap[keyTranslation[i][0]] = keyTranslation[i][1];
    }
  }

  PropertyMap props;

  if(!d->title.isEmpty()) {
    props["TITLE"] = d->title;
  }
  if(!d->artist.isEmpty()) {
    props["ARTIST"] = d->artist;
  }
  if(!d->copyright.isEmpty()) {
    props["COPYRIGHT"] = d->copyright;
  }
  if(!d->comment.isEmpty()) {
    props["COMMENT"] = d->comment;
  }

  ASF::AttributeListMap::ConstIterator it = d->attributeListMap.begin();
  for(; it != d->attributeListMap.end(); ++it) {
    if(keyMap.contains(it->first)) {
      String key = keyMap[it->first];
      AttributeList::ConstIterator it2 = it->second.begin();
      for(; it2 != it->second.end(); ++it2) {
        if(key == "TRACKNUMBER") {
          if(it2->type() == ASF::Attribute::DWordType)
            props.insert(key, String::number(it2->toUInt()));
          else
            props.insert(key, it2->toString());
        }
        else {
          props.insert(key, it2->toString());
        }
      }
    }
    else {
      props.unsupportedData().append(it->first);
    }
  }
  return props;
}

void ASF::Tag::removeUnsupportedProperties(const StringList &props)
{
  StringList::ConstIterator it = props.begin();
  for(; it != props.end(); ++it)
    d->attributeListMap.erase(*it);
}

PropertyMap ASF::Tag::setProperties(const PropertyMap &props)
{
  static Map<String, String> reverseKeyMap;
  if(reverseKeyMap.isEmpty()) {
    int numKeys = sizeof(keyTranslation) / sizeof(keyTranslation[0]);
    for(int i = 0; i < numKeys; i++) {
      reverseKeyMap[keyTranslation[i][1]] = keyTranslation[i][0];
    }
  }

  PropertyMap origProps = properties();
  PropertyMap::ConstIterator it = origProps.begin();
  for(; it != origProps.end(); ++it) {
    if(!props.contains(it->first) || props[it->first].isEmpty()) {
      if(it->first == "TITLE") {
        d->title = String::null;
      }
      else if(it->first == "ARTIST") {
        d->artist = String::null;
      }
      else if(it->first == "COMMENT") {
        d->comment = String::null;
      }
      else if(it->first == "COPYRIGHT") {
        d->copyright = String::null;
      }
      else {
        d->attributeListMap.erase(reverseKeyMap[it->first]);
      }
    }
  }

  PropertyMap ignoredProps;
  it = props.begin();
  for(; it != props.end(); ++it) {
    if(reverseKeyMap.contains(it->first)) {
      String name = reverseKeyMap[it->first];
      removeItem(name);
      StringList::ConstIterator it2 = it->second.begin();
      for(; it2 != it->second.end(); ++it2) {
        addAttribute(name, *it2);
      }
    }
    else if(it->first == "TITLE") {
      d->title = it->second.toString();
    }
    else if(it->first == "ARTIST") {
      d->artist = it->second.toString();
    }
    else if(it->first == "COMMENT") {
      d->comment = it->second.toString();
    }
    else if(it->first == "COPYRIGHT") {
      d->copyright = it->second.toString();
    }
    else {
      ignoredProps.insert(it->first, it->second);
    }
  }

  return ignoredProps;
}
