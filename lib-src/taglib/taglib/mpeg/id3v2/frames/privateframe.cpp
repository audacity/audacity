/***************************************************************************
    copyright            : (C) 2008 by Serkan Kalyoncu
    copyright            : (C) 2008 by Scott Wheeler
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
#include <id3v2tag.h>
#include <tdebug.h>

#include "privateframe.h"

using namespace TagLib;
using namespace ID3v2;


class PrivateFrame::PrivateFramePrivate
{
public:
  ByteVector data;
  String owner;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

PrivateFrame::PrivateFrame() : Frame("PRIV")
{
  d = new PrivateFramePrivate;
}

PrivateFrame::PrivateFrame(const ByteVector &data) : Frame(data)
{
  d = new PrivateFramePrivate;
  setData(data);
}

PrivateFrame::~PrivateFrame()
{
  delete d;
}

String PrivateFrame::toString() const
{
  return d->owner;
}

String PrivateFrame::owner() const
{
  return d->owner;
}

ByteVector PrivateFrame::data() const
{
  return d->data;
}

void PrivateFrame::setOwner(const String &s)
{
  d->owner = s;
}

void PrivateFrame::setData(const ByteVector & data)
{
  d->data = data;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

void PrivateFrame::parseFields(const ByteVector &data)
{
  if(data.size() < 2) {
    debug("A private frame must contain at least 2 bytes.");
    return;
  }

  // Owner identifier is assumed to be Latin1

  const int byteAlign =  1;
  const int endOfOwner = data.find(textDelimiter(String::Latin1), 0, byteAlign);

  d->owner =  String(data.mid(0, endOfOwner));
  d->data = data.mid(endOfOwner + 1);
}

ByteVector PrivateFrame::renderFields() const
{
  ByteVector v;

  v.append(d->owner.data(String::Latin1));
  v.append(textDelimiter(String::Latin1));
  v.append(d->data);

  return v;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

PrivateFrame::PrivateFrame(const ByteVector &data, Header *h) : Frame(h)
{
  d = new PrivateFramePrivate();
  parseFields(fieldData(data));
}
