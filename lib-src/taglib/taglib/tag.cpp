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

#include "tag.h"
#include "tstringlist.h"
#include "tpropertymap.h"

using namespace TagLib;

class Tag::TagPrivate
{

};

Tag::Tag()
{

}

Tag::~Tag()
{

}

bool Tag::isEmpty() const
{
  return (title().isEmpty() &&
          artist().isEmpty() &&
          album().isEmpty() &&
          comment().isEmpty() &&
          genre().isEmpty() &&
          year() == 0 &&
          track() == 0);
}

PropertyMap Tag::properties() const
{
  PropertyMap map;
  if(!(title().isNull()))
    map["TITLE"].append(title());
  if(!(artist().isNull()))
    map["ARTIST"].append(artist());
  if(!(album().isNull()))
    map["ALBUM"].append(album());
  if(!(comment().isNull()))
    map["COMMENT"].append(comment());
  if(!(genre().isNull()))
    map["GENRE"].append(genre());
  if(!(year() == 0))
    map["DATE"].append(String::number(year()));
  if(!(track() == 0))
    map["TRACKNUMBER"].append(String::number(track()));
  return map;
}

void Tag::removeUnsupportedProperties(const StringList&)
{
}

PropertyMap Tag::setProperties(const PropertyMap &origProps)
{
  PropertyMap properties(origProps);
  properties.removeEmpty();
  StringList oneValueSet;
  // can this be simplified by using some preprocessor defines / function pointers?
  if(properties.contains("TITLE")) {
    setTitle(properties["TITLE"].front());
    oneValueSet.append("TITLE");
  } else
    setTitle(String::null);

  if(properties.contains("ARTIST")) {
    setArtist(properties["ARTIST"].front());
    oneValueSet.append("ARTIST");
  } else
    setArtist(String::null);

  if(properties.contains("ALBUM")) {
    setAlbum(properties["ALBUM"].front());
    oneValueSet.append("ALBUM");
  } else
    setAlbum(String::null);

  if(properties.contains("COMMENT")) {
    setComment(properties["COMMENT"].front());
    oneValueSet.append("COMMENT");
  } else
    setComment(String::null);

  if(properties.contains("GENRE")) {
    setGenre(properties["GENRE"].front());
    oneValueSet.append("GENRE");
  } else
    setGenre(String::null);

  if(properties.contains("DATE")) {
    bool ok;
    int date = properties["DATE"].front().toInt(&ok);
    if(ok) {
      setYear(date);
      oneValueSet.append("DATE");
    } else
      setYear(0);
  }
  else
    setYear(0);

  if(properties.contains("TRACKNUMBER")) {
    bool ok;
    int track = properties["TRACKNUMBER"].front().toInt(&ok);
    if(ok) {
      setTrack(track);
      oneValueSet.append("TRACKNUMBER");
    } else
      setTrack(0);
  }
  else
    setYear(0);

  // for each tag that has been set above, remove the first entry in the corresponding
  // value list. The others will be returned as unsupported by this format.
  for(StringList::Iterator it = oneValueSet.begin(); it != oneValueSet.end(); ++it) {
    if(properties[*it].size() == 1)
      properties.erase(*it);
    else
      properties[*it].erase( properties[*it].begin() );
  }
  return properties;
}

void Tag::duplicate(const Tag *source, Tag *target, bool overwrite) // static
{
  if(overwrite) {
    target->setTitle(source->title());
    target->setArtist(source->artist());
    target->setAlbum(source->album());
    target->setComment(source->comment());
    target->setGenre(source->genre());
    target->setYear(source->year());
    target->setTrack(source->track());
  }
  else {
    if(target->title().isEmpty())
      target->setTitle(source->title());
    if(target->artist().isEmpty())
      target->setArtist(source->artist());
    if(target->album().isEmpty())
      target->setAlbum(source->album());
    if(target->comment().isEmpty())
      target->setComment(source->comment());
    if(target->genre().isEmpty())
      target->setGenre(source->genre());
    if(target->year() <= 0)
      target->setYear(source->year());
    if(target->track() <= 0)
      target->setTrack(source->track());
  }
}
