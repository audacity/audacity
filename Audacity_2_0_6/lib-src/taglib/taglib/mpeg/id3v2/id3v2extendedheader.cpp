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

#include "id3v2extendedheader.h"
#include "id3v2synchdata.h"

using namespace TagLib;
using namespace ID3v2;

class ExtendedHeader::ExtendedHeaderPrivate
{
public:
  ExtendedHeaderPrivate() : size(0) {}

  uint size;
};

////////////////////////////////////////////////////////////////////////////////
// public methods
////////////////////////////////////////////////////////////////////////////////

ExtendedHeader::ExtendedHeader()
{
  d = new ExtendedHeaderPrivate();
}

ExtendedHeader::~ExtendedHeader()
{
  delete d;
}

TagLib::uint ExtendedHeader::size() const
{
  return d->size;
}

void ExtendedHeader::setData(const ByteVector &data)
{
  parse(data);
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

void ExtendedHeader::parse(const ByteVector &data)
{
  d->size = SynchData::toUInt(data.mid(0, 4)); // (structure 3.2 "Extended header size")
}
