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

#include <tbytevectorlist.h>
#include <tpropertymap.h>
#include <tdebug.h>

#include "id3v2tag.h"
#include "uniquefileidentifierframe.h"

using namespace TagLib;
using namespace ID3v2;

class UniqueFileIdentifierFrame::UniqueFileIdentifierFramePrivate
{
public:
  String owner;
  ByteVector identifier;
};

////////////////////////////////////////////////////////////////////////////////
// public methods
////////////////////////////////////////////////////////////////////////////////

UniqueFileIdentifierFrame::UniqueFileIdentifierFrame(const ByteVector &data) :
    ID3v2::Frame(data)
{
  d = new UniqueFileIdentifierFramePrivate;
  setData(data);
}

UniqueFileIdentifierFrame::UniqueFileIdentifierFrame(const String &owner, const ByteVector &id) :
    ID3v2::Frame("UFID")
{
  d = new UniqueFileIdentifierFramePrivate;
  d->owner = owner;
  d->identifier = id;
}

UniqueFileIdentifierFrame::~UniqueFileIdentifierFrame()
{
  delete d;
}

String UniqueFileIdentifierFrame::owner() const
{
    return d->owner;
}

ByteVector UniqueFileIdentifierFrame::identifier() const
{
  return d->identifier;
}

void UniqueFileIdentifierFrame::setOwner(const String &s)
{
  d->owner = s;
}

void UniqueFileIdentifierFrame::setIdentifier(const ByteVector &v)
{
  d->identifier = v;
}

String UniqueFileIdentifierFrame::toString() const
{
  return String::null;
}

PropertyMap UniqueFileIdentifierFrame::asProperties() const
{
  PropertyMap map;
  if(d->owner == "http://musicbrainz.org") {
    map.insert("MUSICBRAINZ_TRACKID", String(d->identifier));
  }
  else {
    map.unsupportedData().append(frameID() + String("/") + d->owner);
  }
  return map;
}

UniqueFileIdentifierFrame *UniqueFileIdentifierFrame::findByOwner(const ID3v2::Tag *tag, const String &o) // static
{
  ID3v2::FrameList comments = tag->frameList("UFID");

  for(ID3v2::FrameList::ConstIterator it = comments.begin();
      it != comments.end();
      ++it)
  {
    UniqueFileIdentifierFrame *frame = dynamic_cast<UniqueFileIdentifierFrame *>(*it);
    if(frame && frame->owner() == o)
      return frame;
  }

  return 0;
}

void UniqueFileIdentifierFrame::parseFields(const ByteVector &data)
{
  if(data.size() < 1) {
    debug("An UFID frame must contain at least 1 byte.");
    return;
  }

  int pos = 0;
  d->owner = readStringField(data, String::Latin1, &pos);
  d->identifier = data.mid(pos);
}

ByteVector UniqueFileIdentifierFrame::renderFields() const
{
  ByteVector data;

  data.append(d->owner.data(String::Latin1));
  data.append(char(0));
  data.append(d->identifier);

  return data;
}

UniqueFileIdentifierFrame::UniqueFileIdentifierFrame(const ByteVector &data, Header *h) :
  Frame(h)
{
  d = new UniqueFileIdentifierFramePrivate;
  parseFields(fieldData(data));
}
