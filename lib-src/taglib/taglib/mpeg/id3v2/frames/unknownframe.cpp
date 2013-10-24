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

#include "unknownframe.h"

using namespace TagLib;
using namespace ID3v2;

class UnknownFrame::UnknownFramePrivate
{
public:
  ByteVector fieldData;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

UnknownFrame::UnknownFrame(const ByteVector &data) : Frame(data)
{
  d = new UnknownFramePrivate;
  setData(data);
}

UnknownFrame::~UnknownFrame()
{
  delete d;
}

String UnknownFrame::toString() const
{
  return String::null;
}

ByteVector UnknownFrame::data() const
{
  return d->fieldData;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

void UnknownFrame::parseFields(const ByteVector &data)
{
  d->fieldData = data;
}

ByteVector UnknownFrame::renderFields() const
{
  return d->fieldData;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

UnknownFrame::UnknownFrame(const ByteVector &data, Header *h) : Frame(h)
{
  d = new UnknownFramePrivate;
  parseFields(fieldData(data));
}
