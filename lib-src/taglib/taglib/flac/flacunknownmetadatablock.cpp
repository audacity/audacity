/**************************************************************************
    copyright            : (C) 2010 by Lukáš Lalinský
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

#include <taglib.h>
#include <tdebug.h>
#include <tstring.h>
#include "flacunknownmetadatablock.h"

using namespace TagLib;

class FLAC::UnknownMetadataBlock::UnknownMetadataBlockPrivate
{
public:
  UnknownMetadataBlockPrivate() : code(0) {}

  int code;
  ByteVector data;
};

FLAC::UnknownMetadataBlock::UnknownMetadataBlock(int code, const ByteVector &data)
{
  d = new UnknownMetadataBlockPrivate;
  d->code = code;
  //debug(String(data.toHex()));
  d->data = data;
}

FLAC::UnknownMetadataBlock::~UnknownMetadataBlock()
{
  delete d;
}

int FLAC::UnknownMetadataBlock::code() const
{
  return d->code;
}

void FLAC::UnknownMetadataBlock::setCode(int code)
{
  d->code = code;
}

ByteVector FLAC::UnknownMetadataBlock::data() const
{
  return d->data;
}

void FLAC::UnknownMetadataBlock::setData(const ByteVector &data)
{
  d->data = data;
}

ByteVector FLAC::UnknownMetadataBlock::render() const
{
  return d->data;
}

