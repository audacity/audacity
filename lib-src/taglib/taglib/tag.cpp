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

#include "tag.h"

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
