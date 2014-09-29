/***************************************************************************
    copyright           : (C) 2011 by Mathias Panzenböck
    email               : grosser.meister.morti@gmx.net
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

#include "modtag.h"
#include "tstringlist.h"
#include "tpropertymap.h"

using namespace TagLib;
using namespace Mod;

class Mod::Tag::TagPrivate
{
public:
  TagPrivate()
  {
  }

  String title;
  String comment;
  String trackerName;
};

Mod::Tag::Tag() : TagLib::Tag()
{
  d = new TagPrivate;
}

Mod::Tag::~Tag()
{
  delete d;
}

String Mod::Tag::title() const
{
  return d->title;
}

String Mod::Tag::artist() const
{
  return String::null;
}

String Mod::Tag::album() const
{
  return String::null;
}

String Mod::Tag::comment() const
{
  return d->comment;
}

String Mod::Tag::genre() const
{
  return String::null;
}

TagLib::uint Mod::Tag::year() const
{
  return 0;
}

TagLib::uint Mod::Tag::track() const
{
  return 0;
}

String Mod::Tag::trackerName() const
{
  return d->trackerName;
}

void Mod::Tag::setTitle(const String &title)
{
  d->title = title;
}

void Mod::Tag::setArtist(const String &)
{
}

void Mod::Tag::setAlbum(const String &)
{
}

void Mod::Tag::setComment(const String &comment)
{
  d->comment = comment;
}

void Mod::Tag::setGenre(const String &)
{
}

void Mod::Tag::setYear(uint)
{
}

void Mod::Tag::setTrack(uint)
{
}

void Mod::Tag::setTrackerName(const String &trackerName)
{
  d->trackerName = trackerName;
}

PropertyMap Mod::Tag::properties() const
{
  PropertyMap properties;
  properties["TITLE"] = d->title;
  properties["COMMENT"] = d->comment;
  if(!(d->trackerName.isNull()))
    properties["TRACKERNAME"] = d->trackerName;
  return properties;
}

PropertyMap Mod::Tag::setProperties(const PropertyMap &origProps)
{
  PropertyMap properties(origProps);
  properties.removeEmpty();
  StringList oneValueSet;
  if(properties.contains("TITLE")) {
    d->title = properties["TITLE"].front();
    oneValueSet.append("TITLE");
  } else
    d->title = String::null;

  if(properties.contains("COMMENT")) {
    d->comment = properties["COMMENT"].front();
    oneValueSet.append("COMMENT");
  } else
    d->comment = String::null;

  if(properties.contains("TRACKERNAME")) {
    d->trackerName = properties["TRACKERNAME"].front();
    oneValueSet.append("TRACKERNAME");
  } else
    d->trackerName = String::null;

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
